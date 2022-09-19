module Forecastar
import Dates:Dates,Date
import Flux
import Transformers:Transformer,TransformerDecoder,Positionwise
using BaseTypes
import CudaUtil:DEV
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
    global config, seq, origY, batchers = makeSeq()
    global mod = makeModel(config)
    checkSeq()
    return
end
train() = Forecast.train(config, mod, batchers.train)
test() = Forecast.test(config, mod, batchers.test)

# batchOffset(batchLen, i) = (i-1) * batchLen

function checkSeq()
    seqTrain, seqTest = MLUtil.splitTrainTest(seq, config.testHoldOut)
    origYTrain, origYTest = MLUtil.splitTrainTest(origY, config.testHoldOut)
    bufs = MLUtil.makeBufs(config, seq)
    global checked = 0
    checkSeq(bufs, seqTrain, origYTrain)
    println("Train Y checked ", checked)
    global checked = 0
    checkSeq(bufs, seqTest, origYTest)
    println("Test Y checked ", checked)
end

function checkSeq(bufs, cseq, oy)
    for b in 1:MLUtil.batchCount(config, cseq)
        batch = MLUtil.makeBatch!(bufs, config, cseq, b)
        batchY = config.fromY(batch.bufsY)
        for i in 1:config.batchLen
            ic = MLUtil.indsCast(config, b, i)
            orig = MLUtil.sliceLastDim(oy, ic)
            made = MLUtil.sliceLastDim(batchY, i)
            if orig != made
                error("Found mismatch ", b, ' ', i)
            end
            # println(config.batchLen, ' ', i)
            global checked += 1
        end
    end
end

import CollUtil
function stitchCasts(batcher, castInd)
    yh = CollUtil.simEmpty(origY)
    y = CollUtil.simEmpty(origY)
    seqDim = ndims(origY)
    for cbatch in batcher
        batch = cbatch |> DEV
        cyh = mapslices(argmax, mod.exec(batch.bufsX, batch.bufsCast) |> Flux.cpu; dims=1)
        yh = hcat(yh, selectdim(cyh, seqDim, castInd))
        cy = mapslices(argmax, batch.bufsY[1] |> Flux.cpu; dims=1)
        y = hcat(y, selectdim(cy, seqDim, castInd))
    end
    return (yh, y)
end

using DrawUtil
function disp(; castInd=1, batcher=batchers.test, count=typemax(Int))
    yh, y = stitchCasts(batcher, castInd)
    display(draw(first(dropdims(y; dims=1), count); color=:white))
    draw!(first(dropdims(yh; dims=1), count); color=:green)
    # origY = Int.(fr.seq[1][4,:])

    # for b in batchers.test
    #     global yhat = mapslices(argmax, mod.exec(b.bufsX, b.bufsCast) |> Flux.cpu; dims=1)
    #     global y = mapslices(argmax, b.bufsY[1] |> Flux.cpu; dims=1)
    #     draw(y[]; color=:white)
    #     draw!(yhat; color=:green)
    #     break
    # end
end

function makeModel(cfg)
    tfSize = cfg.encSize[1] # * cfg.encSize[2]
    numHeads = 8
    neurPerHead = 64
    hidSize = 2048

    decSize = cfg.binCnt * cfg.castLen
    castSize = sum(cfg.castWidths) * cfg.castLen

    encer = cfg.encoder |> DEV
    encerCast = cfg.encoderCast |> DEV

    tenc1 = Transformer(tfSize,numHeads,neurPerHead,hidSize; pdrop=0.0) |> DEV
    tenc2 = Transformer(tfSize,numHeads,neurPerHead,hidSize; pdrop=0.0) |> DEV
    decin = Flux.Dense(tfSize => decSize) |> DEV
    decinCast = Flux.Dense(castSize => decSize) |> DEV
    tdec1 = TransformerDecoder(decSize,numHeads,neurPerHead,hidSize; pdrop=0.0) |> DEV
    tdec2 = TransformerDecoder(decSize,numHeads,neurPerHead,hidSize; pdrop=0.0) |> DEV
    linout = Positionwise(Flux.Dense(cfg.binCnt => cfg.binCnt), logsoftmax) |> DEV
    # linout = Positionwise(Flux.Dense(cfg.binCnt, cfg.binCnt), Flux.softmax) |> DEV
    # linout = Positionwise(Flux.Dense(cfg.binCnt, cfg.binCnt), Flux.relu) |> DEV
    exec = (x, cast) -> begin
        # checkX = x
        # checkCast = cast
        # println("typeof(cast) ", typeof(cast))
        e = encer(x)
        # @assert size(e) == cfg.encSize "Size mismatch $(size(e)) $(cfg.encSize)"
        ef = Flux.flatten(e)
        # @assert size(ef) == (tfSize, cfg.batchLen)
        enc1 = tenc1(ef)
        enc2 = tenc2(enc1)
        enc2a = decin(enc2)

        castin1 = encerCast(cast...) # cat(ForecastUtil.encodeDur(cast[1]), ForecastUtil.encodeDates(cast[2]); dims=1)
        # @assert size(castin1) == (sum(cfg.castWidths), cfg.castLen, cfg.batchLen)
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
    # reset = () -> for layer in layers; Flux.reset!(layer) end
    return (;layers, params, exec, loss, opt)
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