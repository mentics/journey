module Forecastar
import Dates:Dates,Date
import Flux:Flux, gpu
using BaseTypes
import Forecast:Forecast,N
import ForecastUtil
import CUDA

# using Forecastar ; fr = Forecastar

function config()
    return (;
        inputWidth = 17,
        encodedWidth = 16,
        inputLen = 50,
        outputInds = [4],
        castLen = 10,
        binCnt = 21,
        binDef = Forecast.BinDef(21),
        batchLen = 128,
        testHoldOut = .3,
        lossTarget = 0.001,
        maxIter = 100,
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
    global seq, encoder = ForecastUtil.makeSeq()
    global mod = makeModel(cfg)
    return
end
train() = ForecastUtil.trainModel(cfg, mod, seq)
test() = ForecastUtil.testModel(cfg, mod, seq)

function makeModel(cfg)
    hiddenSize = 512
    model = Flux.Chain(
        encoder,
        Flux.flatten,
        Flux.Dense(cfg.encodedWidth * cfg.inputLen => hiddenSize),
        Flux.Dense(hiddenSize => hiddenSize),
        Flux.Dense(hiddenSize => cfg.binCnt * cfg.castLen, Flux.relu),
        x -> reshape(x, (cfg.binCnt, cfg.castLen, size(x)[end])),
    )
    cfg.useCpu || (model = model |> gpu)
    function loss(batch)
        x, y = batch
        pred = model(x)
        r = Flux.logitcrossentropy(pred, y)
        return r
    end

    opt = Flux.Adam()
    return (;model, loss, opt)
end

end