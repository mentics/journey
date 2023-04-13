module BacktestAnalysis
using Dates
using SH, BaseTypes, BackTypes, LegMetaTypes
import Backtests as bt, SimpleStore as SS
using DateUtil, DrawUtil, CollUtil, ChainUtil, BacktestUtil, OutputUtil, Pricing
import StatsBase

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
    dateFrom = Date(info.tsFrom)
    dateTo = Date(info.tsTo)
    # days = bdays(dateFrom, dateTo)
    dateToActual = Date(max(maximum(x -> x.ts, acct.open; init=DATE_ZERO), acct.closed[end].close.ts))
    days = bdays(dateFrom, dateToActual)
    rateMean = StatsBase.mean(map(t -> trad.rate(t), acct.closed))
    rateMedian = StatsBase.median(map(t -> trad.rate(t), acct.closed))
    rr = rpnl < 0.0 ? rpnl : (1 + rpnl / params.balInit) ^ (1 / (days / DateUtil.bdaysPerYear())) - 1
    @blog "Summary $(dateFrom) - $(dateTo)($(dateToActual)) (ran $(days) bdays): $(pnlCount(acct))"
    @blog "  bal = $(acct.bal), balReal = $(balReal), rpnl = $(rpnl)" # , urpnl = $(unreal)")
    # blog("  Total: $(rd5(total))")
    @blog "  overall realized rate: $(rd5(rr))"
    @blog "  trade rate mean: $(rd5(rateMean))"
    @blog "  trade rate median: $(rd5(rateMedian))"
    # blog("  openMax: $(acct.openMax)")
    # blog("  marginMax: $(acct.marginMax)")

    println("Summary $(dateFrom) - $(dateTo) (ran $(days) bdays): $(pnlCount(acct))")
    println("  bal = $(acct.bal), balReal = $(balReal), rpnl = $(rpnl)") # , urpnl = $(unreal)")
    # println("  Total: $(total)")
    println("  overall realized rate: $(rd5(rr))")
    println("  trade rate mean: $(rd5(rateMean))")
    println("  trade rate median: $(rd5(rateMedian))")
    # println("  openMax: $(acct.openMax)")
    # println("  marginMax: $(acct.marginMax)")

    return
end

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
function xpirDays(acct=bt.keepAcct)
    trades = acct.closed
    dayss = map(trade -> bdays(Date(trade.open.ts), getExpir(trade.open.lms[1])), trades)
    draw(:hist, dayss);
end
function pnlCount(acct=bt.keepAcct)
    wins, losses = reduce(acct.closed; init=(0,0)) do a, trade
        trad.pnl(trade) > 0 ? (first(a) + 1, last(a)) : (first(a), last(a) + 1)
    end
    return (;wins, losses)
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
#endregion

end