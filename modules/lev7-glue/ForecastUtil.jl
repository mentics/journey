module ForecastUtil
import Dates:Dates,Date
import Flux
import Forecast:Forecast,N
import HistData

randomInput(cfg) = rand(cfg.inputWidth, cfg.inputLen, cfg.batchLen)
randomOutput(cfg) = Flux.onehotbatch([Forecast.toBin(cfg.binDef, randn() ./ 20) for _ in 1:cfg.castLen, _ in 1:cfg.batchLen], 1:cfg.binCnt)
randomLoss(cfg, los) = Statistics.mean(1:100) do _
    los((randomInput(cfg), randomOutput(cfg)))
end

function makeSeq(cfg)
    spy = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("SPY")))
    vix = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("VIX")))
    @assert length(spy) == length(vix)
    seq = Array{N}(undef, cfg.inputWidth, length(spy)-1)
    for i in axes(spy, 1)[(begin+1):end]
        @assert spy[i].date == vix[i].date
        prevSpy = spy[i-1]
        prevSpyClose = prevSpy.close
        prevVixClose = vix[i-1].close
        s = spy[i]
        v = vix[i]
        seq[:,i-1] .= (
            prep1(s.open, prevSpyClose),
            prep1(s.high, prevSpyClose),
            prep1(s.low, prevSpyClose),
            prep1(s.close, prevSpyClose),
            prep1(v.open, prevVixClose),
            prep1(v.high, prevVixClose),
            prep1(v.low, prevVixClose),
            prep1(v.close, prevVixClose),
            (s.date - prevSpy.date).value / 10, # TODO: embed these day and duration values
            Dates.dayofweek(s.date) / 7,
            Dates.dayofmonth(s.date) / Dates.daysinmonth(s.date),
            Dates.dayofquarter(s.date) / daysinquarter(s.date)
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
    return seq
end

function trainModel(cfg, mod, seq)
    seqTrain, _ = Forecast.makeViews(seq, cfg.testHoldOut)
    Forecast.train(cfg, mod.model, mod.opt, mod.loss, seqTrain)
end

function testModel(cfg, mod, seq)
    _, seqTest = Forecast.makeViews(seq, cfg.testHoldOut)
    Forecast.test(cfg, mod.model, mod.loss, seqTest)
end

prep1(x, xp) = (x - xp) / xp
daysinquarter(d) = ( q1 = Dates.firstdayofquarter(d) ; (q1 + Dates.Month(3) - q1).value )

end