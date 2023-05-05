module SeqSpy
using Dates
import Flux
import CollUtil
import MLUtil:MLUtil,N,BinDef
import FluxLayers:EncoderLayer
import MarketDurTypes:MarketDur
import Calendars, HistData

function config()
    binCnt = 21
    def = BinDef(binCnt, -.1, .1)
    binner = x -> MLUtil.toBin(def, x)
    vdef = BinDef(binCnt, 10.0, 40.0)
    binnerVix = x -> UInt8(MLUtil.toBin(vdef, x))
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
        binner,
        binnerVix
    )
end

function make()
    baseCfg = config()
    outWidths = (16,2,2)

    spy = filter(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("SPY")))
    vix = filter(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("VIX")))
    @assert length(spy) == length(vix)
    seqLen = length(spy) - 1
    seq = (Array{UInt8}(undef, 8, seqLen), Array{N}(undef, 12, seqLen), Array{N}(undef, 4, seqLen))

    seqTup = map(axes(spy, 1)[2:end]) do i
        @assert spy[i].date == vix[i].date
        prevSpy = spy[i-1]
        prevSpyClose = prevSpy.close
        # prevVixClose = vix[i-1].close
        s = spy[i]
        v = vix[i]
        dur = durToLogits(Calendars.calcDurCloses(prevSpy.date, s.date))
        return (
            (
                prep2(s.open, prevSpyClose, baseCfg.binner),
                prep2(s.high, prevSpyClose, baseCfg.binner),
                prep2(s.low, prevSpyClose, baseCfg.binner),
                prep2(s.close, prevSpyClose, baseCfg.binner),
                baseCfg.binnerVix(v.open),
                baseCfg.binnerVix(v.high),
                baseCfg.binnerVix(v.low),
                baseCfg.binnerVix(v.close)
            ),
            dur,
            (
                N(dayofweek(s.date) / 7),
                N(dayofmonth(s.date) / daysinmonth(s.date)),
                N(dayofquarter(s.date) / DateUtil.daysinquarter(s.date)),
                N(dayofyear(s.date) / DateUtil.daysinyear(s.date))
            )
        )
    end
    seq = CollUtil.tupsToMat(seqTup)
    # TODO: drop last number from sizes because it's full seq length which won't be used anywhere else
    inputWidths = getindex.(size.(seq), 1)
    # @show inputWidths
    origY = Int.(seq[1][4:4,:])

    enc1 = EncoderLayer(baseCfg.embedSize, baseCfg.binCnt, inputWidths[1], baseCfg.inputLen, outWidths[1])
    enc2 = Flux.Dense(inputWidths[2] => outWidths[2])
    enc3 = Flux.Dense(inputWidths[3] => outWidths[3])
    encoder = Flux.Parallel((xs...) -> cat(xs...; dims=1); enc1, enc2, enc3)
    encoderCast = Flux.Parallel((xs...) -> cat(xs...; dims=1); enc2, enc3)
    # encSize = size(encoder(first(batcher(baseCfg, seq))[1]))
    # println("Encoded size: ", encSize)
    toCast = x -> (x[2],x[3])
    toY = x -> (Flux.onehotbatch(selectdim(x[1], 1, 4), 1:cfg.binCnt),)
    fromY = bufsY -> mapslices(argmax, bufsY[1]; dims=1)
    cfg = merge(baseCfg, (;inputWidths, encSize=(sum(outWidths), baseCfg.inputLen, baseCfg.batchLen), castWidths=outWidths[2:3], encoder, encoderCast, toY, toCast, fromY))
    seqTrain, seqTest = MLUtil.splitTrainTest(seq, cfg.testHoldOut)
    batcherTrain = MLUtil.makeBatcher(cfg, seqTrain)
    batcherTest = MLUtil.makeBatcher(cfg, seqTest)
    return (;cfg, seq, origY, batchers=(;train=batcherTrain, test=batcherTest))

    # posEmb = PositionEmbedding(baseCfg.embedSize) |> DEV
    # embed = Embed(baseCfg.embedSize, baseCfg.binCnt) |> DEV
    # den = Flux.Dense(8 * baseCfg.embedSize => 16) |> DEV
    # encodeVals = x -> begin
    #     we = embed(x, inv(sqrt(baseCfg.embedSize)))
    #     v = we .+ posEmb(we)
    #     sz = size(v)
    #     v2 = reshape(v, (sz[1]*sz[2], sz[3:end]...))
    #     # println(sz)
    #     return den(v2)
    # end
    # encodeDur = Flux.Dense(12 => 2, Flux.relu) |> DEV
    # encodeDates = Flux.Dense(4 => 2, Flux.relu) |> DEV
    # # encoder = SELayer((identity, 1:8), (encodeDates, 9:11), (encodeDur, 12:17))
    # encoder = Parallel((xs...) -> cat(xs...; dims=1), encodeVals, encodeDur, encodeDates) |> DEV
    # batcher = MLUtil.makeBatchIter
    # println("Encoded size: ", size(encoder(first(batcher(baseCfg, seq))[1] |> DEV)))
    # return (;seq, batcher, encoder, params=Flux.params(embed, encodeDur, encodeDates, den))
end

durToLogit(seconds::Second)::N = seconds.value/3600
# durToLogit2(seconds::Second)::N = N(sqrt(seconds.value/3600))
function durToLogits(dur::MarketDur)
    logits = durToLogit.((
        dur.closed,
        dur.pre,
        dur.open,
        dur.post,
        dur.weekend,
        dur.holiday
    ))
    return N.((logits..., sqrt.(logits)...))
end

prep2(x, xp, binner)::UInt8 = binner((x - xp) / xp)

end


# import Calendars
# import SliceMap
# function makeSeq(cfg)
#     spy = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("SPY")))
#     vix = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("VIX")))
#     @assert length(spy) == length(vix)
#     seqLen = length(spy) - 1
#     seq = (Array{UInt8}(undef, 8, seqLen), Array{N}(undef, 12, seqLen), Array{N}(undef, 4, seqLen))
#     for i in axes(spy, 1)[2:end]
#         @assert spy[i].date == vix[i].date
#         prevSpy = spy[i-1]
#         prevSpyClose = prevSpy.close
#         # prevVixClose = vix[i-1].close
#         s = spy[i]
#         v = vix[i]
#         durFrom = durToLogits(Calendars.calcDurCloses(prevSpy.date, s.date))
#         seq[1][:,i-1] .= (
#             prep2(s.open, prevSpyClose, cfg.binner),
#             prep2(s.high, prevSpyClose, cfg.binner),
#             prep2(s.low, prevSpyClose, cfg.binner),
#             prep2(s.close, prevSpyClose, cfg.binner),
#             cfg.binnerVix(v.open),
#             cfg.binnerVix(v.high),
#             cfg.binnerVix(v.low),
#             cfg.binnerVix(v.close)
#         )
#         seq[2][:,i-1] .= durFrom
#         seq[3][:,i-1] .= (
#             N(Dates.dayofweek(s.date) / 7),
#             N(Dates.dayofmonth(s.date) / Dates.daysinmonth(s.date)),
#             N(Dates.dayofquarter(s.date) / daysinquarter(s.date)),
#             N(Dates.dayofyear(s.date) / daysinyear(s.date))
#         )
#     end
#     global pe = PositionEmbedding(cfg.embedSize) |> DEV
#     global embed = Embed(cfg.embedSize, cfg.binCnt) |> DEV
#     global den = Flux.Dense(8 * cfg.embedSize => 16) |> DEV
#     global encodeVals = x -> begin
#         we = embed(x, inv(sqrt(cfg.embedSize)))
#         v = we .+ pe(we)
#         sz = size(v)
#         v2 = reshape(v, (sz[1]*sz[2], sz[3:end]...))
#         # println(sz)
#         return den(v2)
#     end
#     global encodeDur = Flux.Dense(12 => 2, Flux.relu) |> DEV
#     global encodeDates = Flux.Dense(4 => 2, Flux.relu) |> DEV
#     # encoder = SELayer((identity, 1:8), (encodeDates, 9:11), (encodeDur, 12:17))
#     encoder = Parallel((xs...) -> cat(xs...; dims=1), encodeVals, encodeDur, encodeDates) |> DEV
#     batcher = MLUtil.makeBatchIter
#     println("Encoded size: ", size(encoder(first(batcher(cfg, seq))[1] |> DEV)))
#     return (;seq, batcher, encoder, params=Flux.params(embed, encodeDur, encodeDates, den))
# end

# function makeSeqSimple()
#     spy = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("SPY")))
#     vix = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("VIX")))
#     @assert length(spy) == length(vix)
#     seq = Array{N}(undef, 17, length(spy)-1)
#     for i in axes(spy, 1)[2:end]
#         @assert spy[i].date == vix[i].date
#         prevSpy = spy[i-1]
#         prevSpyClose = prevSpy.close
#         prevVixClose = vix[i-1].close
#         s = spy[i]
#         v = vix[i]
#         durFrom = durToLogits(Calendars.calcDurCloses(prevSpy.date, s.date))
#         seq[:,i-1] .= (
#             prep1(s.open, prevSpyClose),
#             prep1(s.high, prevSpyClose),
#             prep1(s.low, prevSpyClose),
#             prep1(s.close, prevSpyClose),
#             prep1(v.open, prevVixClose),
#             prep1(v.high, prevVixClose),
#             prep1(v.low, prevVixClose),
#             prep1(v.close, prevVixClose),
#             N(Dates.dayofweek(s.date) / 7),
#             N(Dates.dayofmonth(s.date) / Dates.daysinmonth(s.date)),
#             N(Dates.dayofquarter(s.date) / daysinquarter(s.date)),
#             durFrom...
#         )
#     end
#     encodeDates = Flux.Dense(3 => 6)
#     encodeDur = Flux.Dense(6 => 2, Flux.relu)
#     encoder = SELayer((identity, 1:8), (encodeDates, 9:11), (encodeDur, 12:17))
#     return (seq, encoder)
# end
