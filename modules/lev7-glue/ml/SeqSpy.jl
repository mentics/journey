module SeqSpy
import Flux
import CollUtil
import MLUtil:MLUtil,N,BinDef
import FluxLayers:EncoderLayer
import MarketDurTypes:MarketDur
import Calendars, HistData

function config()
    binCnt = 21
    def = BinDef(binCnt, 0.0, 1.0)
    binner = x -> MLUtil.toBin(def, x)
    vdef = BinDef(binCnt, 10.0, 40.0)
    binnerVix = x -> MLUtil.toBin(vdef, x)
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
    outWidths = (2,2)

    spy = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("SPY")))
    vix = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("VIX")))
    @assert length(spy) == length(vix)
    seqLen = length(spy) - 1
    seq = (Array{UInt8}(undef, 8, seqLen), Array{N}(undef, 12, seqLen), Array{N}(undef, 4, seqLen))
    for i in axes(spy, 1)[2:end]
        @assert spy[i].date == vix[i].date
        prevSpy = spy[i-1]
        prevSpyClose = prevSpy.close
        # prevVixClose = vix[i-1].close
        s = spy[i]
        v = vix[i]
        durFrom = durToLogits(Calendars.calcDurCloses(prevSpy.date, s.date))
        seq[1][:,i-1] .= (
            prep2(s.open, prevSpyClose, baseCfg.binner),
            prep2(s.high, prevSpyClose, baseCfg.binner),
            prep2(s.low, prevSpyClose, baseCfg.binner),
            prep2(s.close, prevSpyClose, baseCfg.binner),
            baseCfg.binnerVix(v.open),
            baseCfg.binnerVix(v.high),
            baseCfg.binnerVix(v.low),
            baseCfg.binnerVix(v.close)
        )
        seq[2][:,i-1] .= durFrom
        seq[3][:,i-1] .= (
            N(Dates.dayofweek(s.date) / 7),
            N(Dates.dayofmonth(s.date) / Dates.daysinmonth(s.date)),
            N(Dates.dayofquarter(s.date) / daysinquarter(s.date)),
            N(Dates.dayofyear(s.date) / daysinyear(s.date))
        )
    end

    posEmb = PositionEmbedding(baseCfg.embedSize) |> DEV
    embed = Embed(baseCfg.embedSize, baseCfg.binCnt) |> DEV
    den = Flux.Dense(8 * baseCfg.embedSize => 16) |> DEV
    encodeVals = x -> begin
        we = embed(x, inv(sqrt(baseCfg.embedSize)))
        v = we .+ posEmb(we)
        sz = size(v)
        v2 = reshape(v, (sz[1]*sz[2], sz[3:end]...))
        # println(sz)
        return den(v2)
    end
    encodeDur = Flux.Dense(12 => 2, Flux.relu) |> DEV
    encodeDates = Flux.Dense(4 => 2, Flux.relu) |> DEV
    # encoder = SELayer((identity, 1:8), (encodeDates, 9:11), (encodeDur, 12:17))
    encoder = Parallel((xs...) -> cat(xs...; dims=1), encodeVals, encodeDur, encodeDates) |> DEV
    batcher = MLUtil.makeBatchIter
    println("Encoded size: ", size(encoder(first(batcher(baseCfg, seq))[1] |> DEV)))
    return (;seq, batcher, encoder, params=Flux.params(embed, encodeDur, encodeDates, den))
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

end