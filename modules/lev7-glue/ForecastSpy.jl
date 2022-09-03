module ForecastSpy
import Dates:Dates,Date
import Flux:Flux, gpu
using BaseTypes
import Forecast:Forecast,N

# enable_gpu

#==
# TODO: can't make equal sized bins: too much future info and maybe not as useful for low prob outliers

using Forecast ; fc = Forecast ; using ForecastSpy ; fs = ForecastSpy
import Flux
fs.run(;skipTrain=true, skipTest=true)
batchIter = fc.makeBatchIter(fs.cfg, fs.seqm)
b1 = first(batchIter)
grad = Flux.gradient(() -> fs.loss(b1), fs.params)
fs.run()

i = 2
xy1 = fc.singleXY(fs.cfg, fs.seqm, i)
fs.loss(xy1)

argmax(fs.model(xy1[1]); dims=1)
argmax(fs.yoh(fs.cfg, xy1[2]); dims=1)

xy1 = fc.singleXY(fs.cfg, fs.seqm, 1);
argmax(fs.model(xy1[1]); dims=1)
xy1 = fc.singleXY(fs.cfg, fs.seqm, 2);
argmax(fs.model(xy1[1]); dims=1)
xy1 = fc.singleXY(fs.cfg, fs.seqm, 3);
argmax(fs.model(xy1[1]); dims=1)

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

function config()
    return (;
        inputWidth = 12,
        inputLen = 50,
        outputInds = [4],
        castLen = 10,
        binCnt = 21,
        batchLen = 128,
        testHoldOut = .3,
        lossTarget = 0.001,
        maxIter = 10000
    )
end

import GLMakie:surface,surface!
function run(; skipTrain=false, skipTest=false)
    global cfg = config()
    global model, loss = makeModel(cfg)
    global params = Flux.params(model)
    global seqm = makeSeqm(cfg)
    global opt = Flux.Adam()

    # b1 = first(Forecast.makeBatchIter(cfg, seqm))
    # function cb()
    #     display(surface(model(b1[1])[:,:,1]; colormap=:blues))
    #     surface!(ytoit(cfg, b1[2])[:,:,1]; colormap=:greens)
    # end
    seqTrain, seqTest = Forecast.makeViews(seqm, cfg.testHoldOut)
    skipTrain || Forecast.train!(model, opt, loss, cfg, seqTrain)
    skipTest || Forecast.test(cfg, loss, seqTest)
    return
end

function train()
    # global cfg = config()
    seqTrain, seqTest = Forecast.makeViews(seqm, cfg.testHoldOut)
    Forecast.train!(model, opt, loss, cfg, seqTrain)
    test()
    return
end

function test()
    seqTrain, seqTest = Forecast.makeViews(seqm, cfg.testHoldOut)
    Forecast.test(cfg, model, loss, seqTest)
end

using HistData
function makeSeqm(cfg)
    spy = filter!(x -> x.date > Date(2000,1,1), reverse(dataDaily("SPY")))
    vix = filter!(x -> x.date > Date(2000,1,1), reverse(dataDaily("VIX")))
    @assert length(spy) == length(vix)
    seqm = Array{N}(undef, cfg.inputWidth, length(spy)-1)
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
    return seqm
end

function makeModel(cfg)
    model = Flux.Chain(
        Flux.flatten,
        # Flux.Dense(cfg.inputWidth * cfg.inputLen => 1024),
        Flux.LSTM(cfg.inputWidth * cfg.inputLen => 4096),
        Flux.Dense(4096 => cfg.binCnt * cfg.castLen),
        x -> reshape(x, (cfg.binCnt, cfg.castLen, size(x)[end])),
        x -> Flux.softmax(x; dims=1),
        # x -> map(x -> x[1] / cfg.binCnt, argmax(x; dims=1))
    )  |> gpu
    function loss(batch)
        x, y = batch
        # @assert size(x) == (cfg.inputWidth, cfg.inputLen, cfg.batchLen)
        # @assert size(y) == (length(cfg.outputInds), cfg.castLen, cfg.batchLen) "size(y) == $(size(y))"
        # @show "x" typeof(x) size(x)
        # @show "y" typeof(y) size(y)

        # pred = model(x)
        # y = round.(Int, (y .* cfg.binCnt))
        # yoh = reshape(Flux.onehotbatch(y, 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, cfg.batchLen))
        # return Flux.crossentropy(pred, yoh)

        # return Flux.crossentropy(model(x), yoh(cfg, y))
        # return Flux.crossentropy(model(x), y)
        return calcDiff(model(x), y)
    end
    return (;model, loss)
end

# function BinDef(num)
#     left = -0.15
#     right = 0.15
#     span = right - left
#     binWidth = span / num
#     return (; left, right, span, num, binWidth)
# end

# function toBin(def, val)::Int
#     max(1, min(def.num, (val - def.left) ÷ def.binWidth))
# end

# function toBin(val)::Int
#     max(1, min(40, (val - -0.15) ÷ (.3/40)))
# end

# prep1(x, xp) = 100.0 * (x / xp - 1.0)
prep1(x, xp) = (x - xp) / xp
daysinquarter(d) = ( q1 = Dates.firstdayofquarter(d) ; (q1 + Dates.Month(3) - q1).value )

function yoh(cfg, y)
    # def = BinDef(cfg.binCnt)
    # binn = val -> max(1, min(40, (val - (-0.15)) ÷ (.3/40)))
    # reshape(Flux.onehotbatch(map(binn, y), 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, size(y)[end]))
    reshape(Flux.onehotbatch(y, 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, size(y)[end]))
end

function ytoit(cfg, y)
    y1 = round.(Int, (y .* cfg.binCnt))
    yoh = reshape(Flux.onehotbatch(y1, 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, cfg.batchLen))
    return yoh
end

# function worstMatch()
#     y = Flux.onehotbatch(fill(cfg.binCnt-1,cfg.batchLen), 1:cfg.binCnt)
#     r = zeros(cfg.binCnt-1, cfg.batchLen)
#     r = vcat(r, fill(1, cfg.batchLen)')
#     Flux.crossentropy(r, y)
# end

function calcDiff(m1, m2)
    w, len = size(m1)
    mid = w ÷ 2 + 1
    res = 0.0
    for i in 1:len
        x1 = argmax(m1[:,i])
        x2 = argmax(m2[:,i])
        res += err2d(mid, x1, x2)
    end
    return res / len
end

function err2d(mid, x1, x2)
    x1 -= mid
    x2 -= mid
    diff = abs(x1 - x2)
    if x1 * x2 < 0.0
        return diff^2
    else
        return diff
    end
end

function randM()
    def = Forecast.BinDef(cfg.binCnt)
    return Flux.onehotbatch([Forecast.toBin(def, randn() ./ 20) for _ in 1:cfg.batchLen], 1:cfg.binCnt)
end

function testCalcDiff()
    avg = 0.0
    for _ in 1:100
        # m1 = reduce(hcat, [rand(10) for i in 1:cfg.batchLen])
        # m2 = reduce(hcat, [rand(10) for i in 1:cfg.batchLen])
        m1 = randM()
        m2 = randM()
        avg += calcDiff(m1, m2)
    end
    avg /= 100
    println("Average random loss: $(avg)")

    avg = 0.0
    for _ in 1:100
        # m1 = reduce(hcat, [rand(10) for i in 1:cfg.batchLen])
        m1 = randM()
        m2 = deepcopy(m1)
        avg += calcDiff(m1, m2)
    end
    avg /= 100
    println("Average match loss: $(avg)")
end

end
