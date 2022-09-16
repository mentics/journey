module Forecastar
import Dates:Dates,Date
import Flux
import Transformers:Transformer,TransformerDecoder,Positionwise
using BaseTypes
import Forecast
import MLUtil:MLUtil,BinDef,N

# using Forecastar ; fr = Forecastar

import SeqSpy
config = SeqSpy.config
makeSeq = SeqSpy.make

# import SeqGenned
# config = SeqGenned.config
# makeSeq = SeqGenned.make

function run()
    init()
    train()
    test()
end
function init()
    global config, seq, batchers = makeSeq()
    global mod = makeModel(config)
    return
end
train() = Forecast.train(config, mod, batchers.train)
test() = Forecast.test(config, mod, batchers.test)

using DrawUtil
function disp(n=1)
    origY = Int.(fr.seq[1][4,:])

    for b in batchers.test
        global yhat = mapslices(argmax, mod.exec(b.bufsX, b.bufsCast) |> Flux.cpu; dims=1)
        global y = mapslices(argmax, b.bufsY[1] |> Flux.cpu; dims=1)
        draw(y[]; color=:white)
        draw!(yhat; color=:green)
        break
    end
end

function makeModel(cfg)
    tfSize = cfg.encSize[1] * cfg.encSize[2]

    decSize = cfg.binCnt * cfg.castLen
    castSize = sum(cfg.castWidths) * cfg.castLen

    encer = cfg.encoder |> DEV
    encerCast = cfg.encoderCast |> DEV

    tenc1 = Transformer(tfSize,2,4,32) |> DEV
    tenc2 = Transformer(tfSize,2,4,32) |> DEV
    decin = Flux.Dense(tfSize, decSize) |> DEV
    decinCast = Flux.Dense(castSize, decSize) |> DEV
    tdec1 = TransformerDecoder(decSize,2,4,32) |> DEV
    tdec2 = TransformerDecoder(decSize,2,4,32) |> DEV
    # linout = Positionwise(Flux.Dense(decInSize, cfg.binCnt), Flux.softmax) |> DEV
    linout = Positionwise(Flux.Dense(cfg.binCnt, cfg.binCnt), Flux.softmax) |> DEV
    exec = (x, cast) -> begin
        # checkX = x
        # checkCast = cast
        # println("typeof(cast) ", typeof(cast))
        e = encer(x)
        @assert size(e) == cfg.encSize "Size mismatch $(size(e)) $(cfg.encSize)"
        ef = Flux.flatten(e)
        @assert size(ef) == (tfSize, cfg.batchLen)
        enc1 = tenc1(ef)
        enc2 = tenc2(enc1)
        enc2a = decin(enc2)

        castin1 = encerCast(cast...) # cat(ForecastUtil.encodeDur(cast[1]), ForecastUtil.encodeDates(cast[2]); dims=1)
        @assert size(castin1) == (sum(cfg.castWidths), cfg.castLen, cfg.batchLen)
        # println(size(castin1))
        castin2 = decinCast(Flux.flatten(castin1))
        dec1 = tdec1(enc2a, castin2)
        dec2 = tdec2(dec1, castin2)
        dec3 = reshape(dec2, (cfg.binCnt, cfg.castLen, cfg.batchLen))
        out = linout(dec3)
        return out
    end
    # TODO: consider  Flux.Losses.label_smoothing(Flux.onehot(1, 1:10), .1)
    function loss(batch)
        x, cast, y = batch
        pred = exec(x, cast)
        # NOTE: check if pred is all zeros anywhere along the way
        r = Flux.logitcrossentropy(pred, y[1])
        return r
    end

    layers = (;encer, tenc1, tenc2, decin, encerCast, decinCast, tdec1, tdec2, linout)
    # params = union(Flux.params(tenc1, tenc2, tdec1, tdec2, linout), paramsEncoder)
    params = Flux.params(layers...)
    opt = Flux.Adam()
    reset = () -> for layer in layers; Flux.reset!(layer) end
    return (;layers, params, reset, exec, loss, opt)
end

# function makeModelStage1(cfg)
#     hiddenSize = 512
#     model = Flux.Chain(;
#         enc=encoder,
#         # tin=Transformer(16,2,4,32),
#         flat=Flux.flatten,
#         d1=Flux.Dense(cfg.encodedWidth * cfg.inputLen => hiddenSize),
#         d1a=Flux.Dropout(0.1),
#         d2=Flux.Dense(hiddenSize => hiddenSize),
#         d3=Flux.Dense(hiddenSize => cfg.binCnt * cfg.castLen, Flux.relu),
#         resh=x -> reshape(x, (cfg.binCnt, cfg.castLen, size(x)[end]))
#     ) |> DEV
#     function loss(batch)
#         x, y = batch
#         pred = model(x)
#         # NOTE: check if pred is all zeros anywhere along the way
#         r = Flux.logitcrossentropy(pred, y)
#         return r
#     end

#     # params = union(Flux.params(model), paramsEncoder)
#     params = Flux.params(model)
#     opt = Flux.Adam()
#     return (;model, params, loss, opt)
# end

end