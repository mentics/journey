module BacktestAnalysis
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
import Backtests as bt, SimpleStore as SS
using DateUtil, DrawUtil, CollUtil, ChainUtil, BacktestUtil, OutputUtil, Pricing
using StatsBase

#region Exports
export topen, tclosed
topen(id) = trad.open(bt.keepAcct, id)
tclosed(id) = trad.closed(bt.keepAcct, id)
#endregion

#region Public
function showResult(info=bt.info, acct=bt.keepAcct, params=bt.keepParams)::Nothing
    !isempty(acct.closed) || ( println("no trades closed") ; return )
    balReals = mapPoint(pts.balReal, acct.closed, (info.tsFrom, params.balInit))
    draw(:scatter, balReals)

    balReal = last(last(balReals))
    rpnl = balReal - params.balInit
    # unreal = urpnl(acct.open)
    # total = rpnl + unreal

    dateFrom, dateTo, dateToActual = runDates()
    # dateFrom = Date(info.tsFrom)
    # dateTo = Date(info.tsTo)
    # # days = bdays(dateFrom, dateTo)
    # dateToActual = findDateToActual(acct)

    days = bdays(dateFrom, dateToActual)
    rateMean = StatsBase.mean(map(t -> trad.rate(t), acct.closed))
    rateMedian = StatsBase.median(map(t -> trad.rate(t), acct.closed))
    rr = rpnl < 0.0 ? rpnl : (1 + rpnl / params.balInit) ^ (1 / (days / DateUtil.bdaysPerYear())) - 1
    dateOrig = dateToActual != dateTo ? " (orig: $(dateTo))" : ""
    @blog "Summary $(dateFrom) - $(dateToActual)$(dateOrig) (ran $(days) bdays): $(pnlCount(acct))"
    @blog "  bal = $(acct.bal), balReal = $(balReal), rpnl = $(rpnl)" # , urpnl = $(unreal)")
    # blog("  Total: $(rd5(total))")
    @blog "  overall realized rate: $(rd5(rr))"
    @blog "  trade rate mean: $(rd5(rateMean))"
    @blog "  trade rate median: $(rd5(rateMedian))"
    # blog("  openMax: $(acct.openMax)")
    # blog("  marginMax: $(acct.marginMax)")

    println("Summary $(dateFrom) - $(dateToActual)$(dateOrig) (ran $(days) bdays): $(pnlCount(acct))")
    println("  bal = $(acct.bal), balReal = $(balReal), rpnl = $(rpnl)") # , urpnl = $(unreal)")
    # println("  Total: $(total)")
    println("  overall realized rate: $(rd5(rr))")
    println("  trade rate mean: $(rd5(rateMean))")
    println("  trade rate median: $(rd5(rateMedian))")
    # println("  openMax: $(acct.openMax)")
    # println("  marginMax: $(acct.marginMax)")

    return
end
runDates() = (Date(bt.info.tsFrom), Date(bt.info.tsTo), findDateToActual(bt.keepAcct))
findDateToActual(acct) = Date(max(maximum(x -> x.ts, acct.open; init=DATE_ZERO), acct.closed[end].close.ts))

rateMax() = ( (r, i) = findmax(x -> trad.rate(x), bt.keepAcct.closed) ; (i, r, bt.keepAcct.closed[i]) )
rateMin() = ( (r, i) = findmin(x -> trad.rate(x), bt.keepAcct.closed) ; (i, r, bt.keepAcct.closed[i]) )

reviewOpen(id::Int) = reviewOpen(trad.open(bt.keepAcct, id))
reviewOpen(trade) = lyzeOpen(trade) do ts, lms
    trade.neto + Pricing.price(lms)
end
function lmssOpen(trade)
    tss = []
    lmss = []
    lyzeOpen(trade) do ts, lms
        push!(tss, ts)
        push!(lmss, lms)
        trade.neto + Pricing.price(lms)
    end
    return tss, lmss
end

tradeTheta(trade) = lyzeTrade(trade) do ts, lms
    getGreeks(lms).theta
end
tradeVega(trade) = lyzeTrade(trade) do ts, lms
    getGreeks(lms).vega
end
tradeVal(id::Int) = tradeVal(trad.closed(bt.keepAcct, id))
function tradeVal(trade)
    neto = trade.open.neto
    lyzeTrade(trade) do ts, lms
        neto + Pricing.price(lms)
    end
end

tradeVal2(id::Int) = tradeVal2(trad.closed(bt.keepAcct, id))
tradeVal2(trade) = lyzeTrade(trade, NTuple{3,Currency}) do ts, lms
    Pricing.price.(lms)
end

tradeCurp(trade) = lyzeTrade(trade) do ts, lms
    SS.curpFor(ts)
end
import HistData
tradeVix(trade) = lyzeTrade(trade) do ts, lms
    HistData.vixOpen(Date(ts))
end

function lyzeOpen(f::Function, trade::TradeBTOpen, ::Type{T}=Float64) where T
    from = trade.ts
    to = trad.targetDate(trade)
    xys = Vector{Tuple{DateTime,T}}()
    SS.run(from, to) do tim, chain
        otoq = ChainUtil.toOtoq(chain)
        lms = tosn(LegMetaClose, trade.lms, otoq)
        !isnothing(lms) || return
        y = f(tim.ts, lms)
        push!(xys, (tim.ts, y))
        return
    end
    drawRes(xys)
end

function lyzeTrade(f, trade::TradeBT, ::Type{T}=Float64) where T
    from = trade.open.ts
    to = trade.close.ts
    xys = Vector{Tuple{DateTime,T}}()
    SS.run(from, to) do tim, chain
        otoq = ChainUtil.toOtoq(chain)
        lms = tosn(LegMetaClose, trade.open.lms, otoq)
        !isnothing(lms) || return
        y = f(tim.ts, lms)
        push!(xys, (tim.ts, y))
        return
    end
    drawRes(xys)
end

lmsVal(lms, from::DateTime, to::DateTime) = lyzeLms(lms, from, to) do ts, lms
    Pricing.price(lms)
end

function lyzeLms(f, lms0, from::DateTime, to::DateTime, ::Type{T}=Float64) where T
    xys = Vector{Tuple{DateTime,T}}()
    SS.run(from, to) do tim, chain
        otoq = ChainUtil.toOtoq(chain)
        lms = tosn(LegMetaClose, lms0, otoq)
        # theta = getGreeks(lms).theta
        !isnothing(lms) || return
        y = f(tim.ts, lms)
        push!(xys, (tim.ts, y))
        return
    end
    drawRes(xys)
end

drawRes(xys::Vector{<:Tuple{DateTime,Real}}) = draw(:scatter, xys)
function drawRes(xys)
    xs, ytups = CollUtil.vtToTv(xys)
    yvs = CollUtil.vtToTv(ytups)
    draw(:scatter, xs, first(yvs))
    for i in eachindex(yvs)[2:end]
        draw!(:scatter, xs, yvs[i])
    end
end

drawTrade(i::Int) = drawTrade(bt.keepAcct.closed[i].open)
function drawTrade(trade)
    curp = SS.curpFor(trade.ts)
    draw(:hlines, 0; color=:grey)
    draw!(:vlines, curp; color=:grey)
    draw!(:lines, toDraw(trade.lms))
end
#endregion

#region LyzeClosed
function medianDurs(acct=bt.keepAcct)
    trades = acct.closed
    durs = map(trade -> trade.close.ts - trade.open.ts, trades)
    StatsBase.median(durs)
end
function xpirDays(filt=identity, acct=bt.keepAcct)
    trades = acct.closed
    dayss = map(trade -> bdays(Date(trade.open.ts), getExpir(trade.open.lms[1])), filter(filt, trades))
    draw(:hist, dayss);
end
function pnlCount(acct=bt.keepAcct)
    wins, losses = reduce(acct.closed; init=(0,0)) do a, trade
        trad.pnl(trade) > 0 ? (first(a) + 1, last(a)) : (first(a), last(a) + 1)
    end
    return (;wins, losses)
end

filtLoss = t -> (t.open.neto + t.close.netc) < 0.0
filtWin = t -> (t.open.neto + t.close.netc) > 0.0
losses() = filter(filtLoss, bt.keepAcct.closed)
wins() = filter(filtWin, bt.keepAcct.closed)

function wlhist(f; weights=nothing)
    ws = map(f, wins())
    ls = map(f, losses())
    wmn, wmx = extrema(ws)
    lmn, lmx = extrema(ls)
    mx = max(wmx, lmx)
    mn = min(wmn, lmn)
    bins = mn:(mx-mn)/200.0:mx
    if isnothing(weights)
        draw(:hist, ws; bins)
        draw!(:hist, ls; bins)
    else
        wwts = map(weights, wins())
        lwts = map(weights, losses())
        draw(:hist, ws; bins, weights=wwts)
        draw!(:hist, ls; bins, weights=lwts)
    end
end

function wlcomp(f)
    ws = map(f, wins())
    ls = map(f, losses())
    wsm = mean(ws)
    lsm = mean(ls)
    wsd = std(ws; mean=wsm)
    lsd = std(ls; mean=lsm)
    println("  win  mean: $(wsm) ; std: $(wsd)")
    println("  loss mean: $(lsm) ; std: $(lsd)")
end

function wl()
    println("Net open:")
    wlcomp(t -> t.open.neto)
    println("Profit:")
    wlcomp(t -> t.open.extra.profit)
    println("Strike width:")
    wlcomp(strikeWidth)
    println("Rate:")
    wlcomp(rate)
    println("Kelly:")
    wlcomp(kel)
    println("Risk:")
    wlcomp(risk)
end

import LinesLeg as LL
function shapes()
    # s = map(ts) do t
    #     segs = LL.toSegments(t.open.lms)
    #     diff = segs.points[end].y - segs.points[1].y
    #     return diff
    # end
    wlhist() do t
        segs = LL.toSegments(t.open.lms)
        return segs.points[end].y - segs.points[1].y
    end
end

function pnlvv()
    dataHigh = map(t -> (t.open.ts, pnl(t)), filter(isHigh, bt.keepAcct.closed))
    dataLow = map(t -> (t.open.ts, pnl(t)), filter(!isHigh, bt.keepAcct.closed))
    draw(:scatter, dataHigh; color=:lightblue)
    draw!(:scatter, dataLow; color=:blue)
    plotVix()
end

import Makie, Calendars
function plotVix()
    from, _, to = runDates()
    vix = map(x -> (Calendars.getMarketOpen(x.date), (x.open)), HistData.dataDaily(from, to, "VIX"))
    # ax2 = layout[2,1] = LAxis(scene, title="VIX")
    axis = Makie.Axis(DrawUtil.getFig(; newFig=false)[1,1]; yaxisposition = :right)
    Makie.hidespines!(axis)
    Makie.hidexdecorations!(axis)
    draw!(:lines, vix; axis)
end
#endregion

#region Points
module pts
using BaseTypes, BackTypes, ..bt.trad, Pricing
balReal(prev::PT, trade::TradeBT) = prev + trad.pnlm(trade)
theta(lms) = getGreeks(lms).theta
price(lms) = Pricing.price(lms, .2)
end
#endregion

#region Util
function mapPoint(f, trades, init)
    buf = Vector{typeof(init)}(undef, length(trades))
    mapPoint!(f, buf, trades, init)
    return buf
end

mapPoint!(f, buf, trades, init) = CollUtil.accum!(buf, trades; init) do prev, trade
    return (bt.trad.tsClose(trade), f(prev[2], trade))
end

SS.curpFor(t::TradeBT) = SS.curpFor(t.open.ts)

isHigh(t) = !isLong(t.open.lms[1])

strikeWidth(t) = getStrike(t.open.lms[end]) - getStrike(t.open.lms[1])
SH.getNetOpen(t::TradeBT) = t.open.neto
pnl(t::TradeBT) = t.open.neto + t.close.netc
vix(t::TradeBT) = HistData.vixOpen(Date(t.open.ts))
# score(t::TradeBT) = t.open.extra.score
rate(t::TradeBT) = t.open.extra.rate
kel(t::TradeBT) = t.open.extra.kel
risk(t::TradeBT) = t.open.extra.risk

pat(f, t::TradeBT) = map(f, t.open.lms)
patdict(f, ts) = countmap(map(t -> pat(f, t), ts))
#endregion

end