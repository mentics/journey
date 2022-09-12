module Forecastar
import Dates:Dates,Date
import Flux
import Transformers:Transformer,TransformerDecoder,Positionwise
using BaseTypes
import ForecastUtil:ForecastUtil,DEV
import MLUtil:MLUtil,BinDef,N
import FluxLayers:EncoderLayer

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
        binnerVix
    )
end

function run()
    init()
    train()
    test()
end
function init()
    global cfg, seq, batcher = makeSeqTest(configTest())
    global mod = makeModel(cfg)
    return
end
train() = ForecastUtil.trainModel(cfg, mod, seq)
test() = ForecastUtil.testModel(cfg, mod, seq)

function makeModel(cfg)
    tfSize = cfg.encSize[1] * cfg.encSize[2]

    decSize = cfg.binCnt * cfg.castLen
    castSize = cfg.castWidth * cfg.castLen

    encer = cfg.encoder |> DEV
    encerCast = cfg.encoderCast |> DEV

    tenc1 = Transformer(tfSize,2,4,32) |> DEV
    tenc2 = Transformer(tfSize,2,4,32) |> DEV
    decin = Flux.Dense(tfSize, decSize) |> DEV
    decinCast = Flux.Dense(castSize, decSize) |> DEV
    tdec1 = TransformerDecoder(decSize,2,4,32) |> DEV
    tdec2 = TransformerDecoder(decSize,2,4,32) |> DEV
    # linout = Positionwise(Flux.Dense(decInSize, cfg.binCnt), Flux.softmax) |> DEV
    linout = Positionwise(Flux.Dense(cfg.binCnt, cfg.binCnt)) |> DEV
    exec = (x, cast) -> begin
        checkX = x
        checkCast = cast
        # println("typeof(cast) ", typeof(cast))
        e = encer(x)
        @assert size(e) == cfg.encSize "Size mismatch $(size(e)) $(cfg.encSize)"
        ef = Flux.flatten(e)
        @assert size(ef) == (tfSize, cfg.batchLen)
        enc1 = tenc1(ef)
        enc2 = tenc2(enc1)
        enc2a = decin(enc2)

        castin1 = encerCast(cast...) # cat(ForecastUtil.encodeDur(cast[1]), ForecastUtil.encodeDates(cast[2]); dims=1)
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

    model = Flux.Chain(; encoder, tenc1, tenc2, decin, decinCast, tdec1, tdec2, linout)
    # params = union(Flux.params(tenc1, tenc2, tdec1, tdec2, linout), paramsEncoder)
    params = Flux.params(model)
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
    ) |> DEV
    function loss(batch)
        x, y = batch
        pred = model(x)
        # NOTE: check if pred is all zeros anywhere along the way
        r = Flux.logitcrossentropy(pred, y)
        return r
    end

    # params = union(Flux.params(model), paramsEncoder)
    params = Flux.params(model)
    opt = Flux.Adam()
    return (;model, params, loss, opt)
end


function configTest()
    binCnt = 21
    def = BinDef(binCnt, 0.0, 1.0)
    binner = x -> MLUtil.toBin(def, x)
    return (;
        # inputWidths = (8, 12, 4),
        inputLen = 50,
        castLen = 10,
        batchLen = 128,
        embedSize = 32,

        testHoldOut = .3,
        lossTarget = 0.001,
        maxIter = typemax(Int),
        binCnt,
        binner
    )
end

import CollUtil
function makeSeqTest(baseCfg)
    outWidths = (2,2)

    seqTup = map(1:1000) do i
        s5 = (sin(2π*i/100))
        s4 = (sin(2π*i/10))
        s3 = sin((s5 > 0 ? -1 : 1) * 2π*i/200)
        s2 = sin((s4 > 0 ? -1 : 1) * 2π*i/37)
        s1 = s2 + s3
        return (UInt8.((baseCfg.binner(s1), baseCfg.binner(s2))), N.((s3, s4, s5)))
    end
    seq = CollUtil.tupsToMat(seqTup)

    inputSize = (length.(seq), baseCfg.inputLen)

    enc1 = EncoderLayer(baseCfg.embedSize, baseCfg.binCnt, 2, baseCfg.inputLen, outWidths[1])
    enc2 = Flux.Dense(3 => outWidths[2])
    encoder = Flux.Parallel((xs...) -> cat(xs...; dims=1); enc1, enc2)
    encoderCast = enc2 # Flux.Parallel((_, x) -> x; enc1=identity, enc2)
    batcher = MLUtil.makeBatchIter
    # encSize = size(encoder(first(batcher(baseCfg, seq))[1]))
    # println("Encoded size: ", encSize)
    toCast = x -> (x[2],)
    toY = x -> (Flux.onehotbatch(selectdim(x[1], 1, 1), 1:cfg.binCnt),)
    cfg = merge(baseCfg, (;inputSize, encSize=(sum(outWidths), baseCfg.inputLen, baseCfg.batchLen), castWidth=outWidths[2], encoder, encoderCast, toY, toCast))
    return (;cfg, seq, batcher)
end

end