module ForecastSpy
import Dates:Date
import Forecast:N
using BaseTypes

#==
Input:
    SPY: O,H,L,C ./ prevC: log ret bins
    VIX: O,H,L,C ./ prevC: log ret bins
    days since prev
    day of the week
    day of the quarter
    day of the month
    TODO: days to dividend
Output:
    bincnt x castlen matrix prob forecast

Layers:
    softmax (which algo to make it reasonably smooth probs?)
==#

function run()
    global cfg = config()
    global model, loss = makeModel(cfg)
    global seqm = makeSeqm(cfg)
    Forecast.train!(model, loss, cfg, seqm)
end

function config()
    return (;
        inputWidth = 12,
        inputLen = 30,
        outputInds = [1],
        castLen = 20,
        binCnt = 100,
        batchCount = 100,
        batchSize = 128
    )
end

using HistData
function makeSeqm(cfg)
    spy = filter!(x -> x.date > Date(2000,1,1), dataDaily("SPY"))
    vix = filter!(x -> x.date > Date(2000,1,1), dataDaily("VIX"))
    @assert length(spy) == length(vix)
    global seqm = Array{N}(undef, cfg.inputWidth, length(spy)-1)
    for i in axes(spy, 1)[(begin+1):end]
        @assert spy[i].date == vix[i].date
        prevSpy = spy[i-1]
        prevSpyClose = prevSpy.close
        prevVixClose = vix[i-1].close
        s = spy[i]
        v = vix[i]
        seqm[:,i-1] .= (
            prep1(s.open, prevSpyClose),
            prep1(s.high, prevSpyClose),
            prep1(s.low, prevSpyClose),
            prep1(s.close, prevSpyClose),
            prep1(v.open, prevVixClose),
            prep1(v.high, prevVixClose),
            prep1(v.low, prevVixClose),
            prep1(v.close, prevVixClose),
            (s.date - prevSpy.date).value, # TODO: embed these day and duration values
            Dates.dayofweek(s.date),
            Dates.dayofmonth(s.date),
            Dates.dayofquarter(s.date)
        )
    end
    binsSpy = Forecast.binLog(cfg.binCnt, seqm[4,:])
    binsVix = Forecast.binLog(cfg.binCnt, seqm[8,:])
    for j in axes(seqm, 2)
        for i in 1:4
            seqm[i,j] = F(Forecast.findBin(binsSpy, seqm[i,j]))
        end
        for i in 5:8
            seqm[i,j] = F(Forecast.findBin(binsVix, seqm[i,j]))
        end
    end
    return seqm
end
# prep1(x, xp) = 100.0 * (x / xp - 1.0)
prep1(x, xp) = 100.0 * (x / xp)

function makeModel(cfg)
    model = Chain(Flux.flatten,
          Dense(cfg.inputWidth * cfg.inputLen => 4096),
          Dense(4096 => cfg.binCnt * cfg.castLen),
          softmax)
    function loss(batch)
        x, y = batch
        Flux.Losses.mse(model(x), y)
    end
    return (;model, loss)
end

end