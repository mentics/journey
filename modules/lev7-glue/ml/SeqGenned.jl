module SeqGenned
import Flux
import CollUtil
import MLUtil:MLUtil,N,BinDef
import FluxLayers:EncoderLayer

function config()
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

function make()
    baseCfg = config()
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

    inputSizes = (size.(seq)..., baseCfg.inputLen)

    enc1 = EncoderLayer(baseCfg.embedSize, baseCfg.binCnt, inputSizes[1][1], baseCfg.inputLen, outWidths[1])
    enc2 = Flux.Dense(inputSizes[2][1] => outWidths[2])
    encoder = Flux.Parallel((xs...) -> cat(xs...; dims=1); enc1, enc2)
    encoderCast = enc2 # Flux.Parallel((_, x) -> x; enc1=identity, enc2)
    batcher = MLUtil.makeBatchIter
    # encSize = size(encoder(first(batcher(baseCfg, seq))[1]))
    # println("Encoded size: ", encSize)
    toCast = x -> (x[2],)
    toY = x -> (Flux.onehotbatch(selectdim(x[1], 1, 1), 1:cfg.binCnt),)
    cfg = merge(baseCfg, (;inputSizes, encSize=(sum(outWidths), baseCfg.inputLen, baseCfg.batchLen), castWidth=outWidths[2], encoder, encoderCast, toY, toCast))
    return (;cfg, seq, batcher)
end

end