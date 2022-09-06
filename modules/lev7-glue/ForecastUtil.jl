module ForecastUtil
import Dates:Dates,Date,Second
import Flux
import Forecast:Forecast,N
import HistData

randomInput(cfg) = rand(cfg.inputWidth, cfg.inputLen, cfg.batchLen)
randomInput(cfg) = rand(cfg.inputWidth, cfg.inputLen, cfg.batchLen)
randomOutput(cfg) = Flux.onehotbatch([Forecast.toBin(cfg.binDef, randn() ./ 20) for _ in 1:cfg.castLen, _ in 1:cfg.batchLen], 1:cfg.binCnt)
randomLoss(cfg, los) = Statistics.mean(1:100) do _
    los((randomInput(cfg), randomOutput(cfg)))
end

import MarketDurTypes:MarketDur
durToLogits(seconds::Second)::N = N(sqrt(seconds.value/3600)) # TODO: might want to sqrt after encoding instead of here
durToLogits(dur::MarketDur) = durToLogits.((
    dur.closed,
    dur.pre,
    dur.open,
    dur.post,
    dur.weekend,
    dur.holiday
))

import Calendars
import SliceMap
function makeSeq(cfg)
    spy = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("SPY")))
    vix = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("VIX")))
    @assert length(spy) == length(vix)
    seq = Array{N}(undef, 17, length(spy)-1)
    # seq = Vector{Tuple{NTuple{8,N},NTuple{3,N},NTuple{6,N}}}(undef, length(spy)-1)
    # seq = Vector{NTuple{3,Vector{N}}}(undef, length(spy)-1)
    for i in axes(spy, 1)[2:end]
        @assert spy[i].date == vix[i].date
        prevSpy = spy[i-1]
        prevSpyClose = prevSpy.close
        prevVixClose = vix[i-1].close
        s = spy[i]
        v = vix[i]
        # (s.date - prevSpy.date).value / 10, # TODO: embed these day and duration values
        durFrom = durToLogits(Calendars.calcDurCloses(prevSpy.date, s.date))
        # seq[:,i-1] .= (
        # seq[i-1] = collect.((
        #     (
        #         prep1(s.open, prevSpyClose),
        #         prep1(s.high, prevSpyClose),
        #         prep1(s.low, prevSpyClose),
        #         prep1(s.close, prevSpyClose),
        #         prep1(v.open, prevVixClose),
        #         prep1(v.high, prevVixClose),
        #         prep1(v.low, prevVixClose),
        #         prep1(v.close, prevVixClose)
        #     ),
        #     (
        #         N(Dates.dayofweek(s.date) / 7),
        #         N(Dates.dayofmonth(s.date) / Dates.daysinmonth(s.date)),
        #         N(Dates.dayofquarter(s.date) / daysinquarter(s.date))
        #     ),
        #     (durFrom...,),
        # ))
        seq[:,i-1] .= (
            prep1(s.open, prevSpyClose),
            prep1(s.high, prevSpyClose),
            prep1(s.low, prevSpyClose),
            prep1(s.close, prevSpyClose),
            prep1(v.open, prevVixClose),
            prep1(v.high, prevVixClose),
            prep1(v.low, prevVixClose),
            prep1(v.close, prevVixClose),
            N(Dates.dayofweek(s.date) / 7),
            N(Dates.dayofmonth(s.date) / Dates.daysinmonth(s.date)),
            N(Dates.dayofquarter(s.date) / daysinquarter(s.date)),
            durFrom...
        )
    end
    # binsSpy = Forecast.binLog(cfg.binCnt, seqm[4,:])
    # binsVix = Forecast.binLog(cfg.binCnt, seqm[8,:])
    # for j in axes(seqm, 2)
    #     for i in 1:4
    #         seqm[i,j] = Forecast.findBin(binsSpy, seqm[i,j]) / cfg.binCnt
    #     end
    #     for i in 5:8
    #         seqm[i,j] = Forecast.findBin(binsVix, seqm[i,j]) / cfg.binCnt
    #     end
    # end
    # encoder = Flux.Parallel(vcat, identity, Flux.Dense(3 => 6), Flux.Dense(6 => 2))
    encodeDates = Flux.Dense(3 => 6)
    encodeDur = Flux.Dense(6 => 2)
    # function encode(x)
    #     # dates = mapslices(encodeDates, x[9:11,:,:]; dims=1)
    #     # durs = mapslices(encodeDur, x[12:17,:,:]; dims=1)
    #     dates = SliceMap.slicemap(encodeDates, x[9:11,:,:]; dims=1)
    #     durs = SliceMap.slicemap(encodeDur, x[12:17,:,:]; dims=1)
    #     return cat(x[1:8,:,:], dates, durs; dims=1)
    # end
    encoder = SELayer((identity, 1:8), (encodeDates, 9:11), (encodeDur, 12:17))

    # return (seq, encode, Flux.params(encodeDates, encodeDur))
    return (seq, encoder)
end

# function test((x, y)...)
#     @show x y
#     @show typeof(x) typeof(y)
#     @show size(x) size(y)
# end

struct SeqEncodeLayer{T,P}
    encoders::T
    inputs::P
end
SELayer(encIns::Tuple...) = SeqEncodeLayer(first.(encIns), last.(encIns))
Flux.@functor SeqEncodeLayer
(m::SeqEncodeLayer)(x::AbstractArray) = mapreduce((enc, inds) -> enc(x[inds,:,:]), vcat, m.encoders, m.inputs)
Flux.trainable(sel::SeqEncodeLayer) = (sel.encoders,)

function trainModel(cfg, mod, seq)
    seqTrain, _ = Forecast.makeViews(seq, cfg.testHoldOut)
    Forecast.train(cfg, mod.model, mod.opt, mod.loss, seqTrain)
end

function testModel(cfg, mod, seq)
    _, seqTest = Forecast.makeViews(seq, cfg.testHoldOut)
    Forecast.test(cfg, mod.model, mod.loss, seqTest)
end

prep1(x, xp)::N = (x - xp) / xp
daysinquarter(d)::N = ( q1 = Dates.firstdayofquarter(d) ; (q1 + Dates.Month(3) - q1).value )

end