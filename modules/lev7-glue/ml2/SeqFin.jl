module SeqFin
import Dates:Dates,Date,Day
import Flux
import CollUtil
import DictUtil:toDict
import MLUtil:MLUtil,N,BinDef
import FluxLayers
import HistData

function config()
    return (;
        inputLen = 70,
        castLen = 20,
        batchLen = 128,
        embedSize = 32,

        testHoldOut = .3,
        lossTarget = 0.001,
        maxIter = typemax(Int)
    )
end

function make()
    baseCfg = config()
    # spy = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("SPY")))
    # vix = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("VIX")))
    dateFrom = Date(2000,1,1)
    spy = toDict(dateFor, filter!(x -> x.date >= dateFrom, HistData.dataDaily("SPY")))
    vix = toDict(dateFor, filter!(x -> x.date >= dateFrom, HistData.dataDaily("VIX")))
    @assert length(spy) == length(vix)
    dateTo = maximum(keys(spy))
    seqLen = Dates.value(dateTo - dateFrom) + 1
    seq = (Array{N}(undef, 8, seqLen), Array{N}(undef, 12, seqLen), Array{Int16}(undef, 4, seqLen))

    seqTup = map(dateFrom:Day(1):dateTo) do date
        if haskey(spy, date)
            s = spy[date]
            v = vix[date]
            # (to forecast, future unknown, future known 1, future known 2)
            return (
                (s.close,),
                (s.open, s.high, s.low, v.open, v.high, v.low, v.close),
                dayData(true, date)...
            )
        else
            return ( (0.0,), zeros(N, 7), dayData(false, date)...)
        end
    end
    seq = CollUtil.tupsToMat(seqTup)
    inputWidths = getindex.(size.(seq), 1)
    outputWidths = (size(seq[1],1), size(seq[2],1), length(dayData(false, Date(0))[1]) + sum(last.(embedDayData())))
    @show size.(seq) inputWidths outputWidths
    origY = seq[1]

    encoderDays = FluxLayers.EmbedMulti(embedDayData()...)
    encoderKnown = Flux.Parallel(FluxLayers.cat1; bools=identity, days=encoderDays)
    encoder = Flux.Parallel(FluxLayers.cat1; y=identity, unknown=identity, known1=identity, known2=encoderKnown) # TODO: could be optimized
    encoderCast = encoderKnown
    toCast = x -> (x[3], x[4])
    toY = x -> x[1]
    fromY = bufsY -> mapslices(argmax, bufsY[1]; dims=1)
    cfg = merge(baseCfg, (;inputWidths, encedSize=(sum(outputWidths), baseCfg.inputLen, baseCfg.batchLen), castWidths=outputWidths[3], encoder, encoderCast, toY, toCast, fromY))
    seqTrain, seqTest = MLUtil.splitTrainTest(seq, cfg.testHoldOut)
    batcherTrain = MLUtil.makeBatcher(cfg, seqTrain)
    batcherTest = MLUtil.makeBatcher(cfg, seqTest)
    return (;cfg, seq, origY, batchers=(;train=batcherTrain, test=batcherTest))
end

embedDayData() = (7 => 4, 31 => 4, 92 => 4, 366 => 4)
dayData(isMktOpen, d) = (
    (
        N(isMktOpen),
        N(isWeekday(d)),
        N(!isMktOpen && isWeekday(d))
    ),(
        Dates.dayofweek(d),
        Dates.dayofmonth(d),
        Dates.dayofquarter(d),
        Dates.dayofyear(d)
    )
)

isWeekday(d) = !(Dates.issaturday(d) || Dates.issunday(d))
dateFor(x) = x.date

end
