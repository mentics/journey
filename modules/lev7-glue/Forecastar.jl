module Forecastar
import Dates:Dates,Date
import Flux:Flux, gpu
import Transformers:Transformer,TransformerDecoder,Positionwise
import CUDA
using BaseTypes
# import Forecast:Forecast,N
import ForecastUtil
import MLUtil:MLUtil,BinDef

# TODO: use
# dev = has_cuda() ? gpu : cpu

# using Forecastar ; fr = Forecastar

function config()
    binCnt = 21
    def = BinDef(binCnt)
    binner = x -> MLUtil.toBin(def, x)
    vdef = BinDef(binCnt, 10.0, 40.0)
    binnerVix = x -> MLUtil.toBin(vdef, x)
    return (;
        # inputWidths = (8, 12, 4),
        inputLen = 50,
        outputInds = [4],
        castLen = 10,
        batchLen = 128,
        embedSize = 32,
        encodedWidth = 20,
        testHoldOut = .3,
        lossTarget = 0.001,
        maxIter = typemax(Int),
        binCnt,
        binner,
        binnerVix,
        useCpu = (CUDA.version() == v"11.1.0")
    )
end

function run()
    init()
    train()
    test()
end
function init()
    global cfg = config()
    global seq, batcher, encoder, paramsEncoder = ForecastUtil.makeSeq(cfg)
    global mod = makeModel(cfg)
    return
end
train() = ForecastUtil.trainModel(cfg, mod, seq)
test() = ForecastUtil.testModel(cfg, mod, seq)

function makeModel(cfg)
    modEnc = Flux.Chain(;
        tenc1=Transformer(16,2,4,32),
        tenc2=Transformer(16,2,4,32),
    ) |> gpu
    tdec1 = TransformerDecoder(16,2,4,32) |> gpu
    tdec2 = TransformerDecoder(16,2,4,32) |> gpu
    linout = Positionwise(Flux.Dense(16, cfg.binCnt), Flux.softmax) |> gpu
    global exec = (x, cast) -> begin
        e = encoder(x)
        stage1 = modEnc(e)
        dec1 = tdec1(stage1, cast)
        dec2 = tdec2(dec1, cast)
        out = linout(dec2)
        return out
    end
    # TODO: consider  Flux.Losses.label_smoothing(Flux.onehot(1, 1:10), .1)
    function loss(batch)
        x, y = batch
        global pred = model(x)
        # NOTE: check if pred is all zeros anywhere along the way
        r = Flux.logitcrossentropy(pred, y)
        return r
    end

    params = union(Flux.params(model), paramsEncoder)
    # params = Flux.params(model)
    opt = Flux.Adam()
    return (;model, params, exec, loss, opt)
end

function makeModelStage1(cfg)
    hiddenSize = 512
    model = Flux.Chain(;
        enc=encoder,
        # tin=Transformer(16,2,4,32),
        flat=Flux.flatten,
        d1=Flux.Dense(cfg.encodedWidth * cfg.inputLen => hiddenSize),
        d1a=Flux.Dropout(0.1),
        d2=Flux.Dense(hiddenSize => hiddenSize),
        d3=Flux.Dense(hiddenSize => cfg.binCnt * cfg.castLen, Flux.relu),
        resh=x -> reshape(x, (cfg.binCnt, cfg.castLen, size(x)[end]))
    )
    cfg.useCpu || (model = model |> gpu)
    function loss(batch)
        x, y = batch
        global pred = model(x)
        # NOTE: check if pred is all zeros anywhere along the way
        r = Flux.logitcrossentropy(pred, y)
        return r
    end

    # params = union(Flux.params(model), paramsEncoder)
    params = Flux.params(model)
    opt = Flux.Adam()
    return (;model, params, loss, opt)
end

end