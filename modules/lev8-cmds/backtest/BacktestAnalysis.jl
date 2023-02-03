module BacktestAnalysis
using Dates
using SH, BaseTypes, BackTypes, LegMetaTypes
import Backtests as bt, SimpleStore as SS
using DateUtil, DrawUtil, CollUtil, ChainUtil, BacktestUtil, OutputUtil
import StatsBase

#region Public
function showResult(info=bt.info, acct=bt.keepAcct, params=bt.keepParams)::Nothing
    balReals = mapPoint(pts.balReal, acct.closed, (info.tsFrom, params.balInit))
    draw(:scatter, balReals)

    balReal = last(last(balReals))
    rpnl = balReal - params.balInit
    # unreal = urpnl(acct.open)
    # total = rpnl + unreal
    dateFrom = Date(info.tsFrom)
    dateTo = Date(info.tsTo)
    days = bdays(dateFrom, dateTo)
    rateMean = StatsBase.mean(map(t -> trad.rate(t), acct.closed))
    rateMedian = StatsBase.median(map(t -> trad.rate(t), acct.closed))
    rr = rpnl < 0.0 ? rpnl : (1 + rpnl / params.balInit) ^ (1 / (days / DateUtil.bdaysPerYear())) - 1
    @blog "Summary $(dateFrom) - $(dateTo) (ran $(days) bdays):"
    @blog "  bal = $(acct.bal), balReal = $(balReal), rpnl = $(rpnl)" # , urpnl = $(unreal)")
    # blog("  Total: $(rd5(total))")
    @blog "  overall realized rate: $(rd5(rr))"
    @blog "  trade rate mean: $(rd5(rateMean))"
    @blog "  trade rate median: $(rd5(rateMedian))"
    # blog("  openMax: $(acct.openMax)")
    # blog("  marginMax: $(acct.marginMax)")

    println("Summary $(dateFrom) - $(dateTo) (ran $(days) bdays):")
    println("  bal = $(acct.bal), balReal = $(balReal), rpnl = $(rpnl)") # , urpnl = $(unreal)")
    # println("  Total: $(total)")
    println("  overall realized rate: $(rd5(rr))")
    println("  trade rate mean: $(rd5(rateMean))")
    println("  trade rate median: $(rd5(rateMedian))")
    # println("  openMax: $(acct.openMax)")
    # println("  marginMax: $(acct.marginMax)")

    return
end

rateMax() = bt.keepAcct.closed[findmax(x -> trad.rate(x), bt.keepAcct.closed)[2]]

function lyzeTrade(f, trade::TradeBT)
    from = trade.open.ts
    to = trade.close.ts
    thetas = Vector{Tuple{DateTime,Float64}}()
    SS.run(from, to) do tim, chain
        otoq = ChainUtil.toOtoq(chain)
        lms = tos(LegMetaClose, trade.open.lms, otoq)
        theta = getGreeks(lms).theta
        push!(thetas, (tim.ts, theta))
    end
    draw(:scatter, thetas)
end
#endregion

#region Points
module pts
using BaseTypes, BackTypes, ..bt.trad, Pricing
balReal(prev::PT, trade::TradeBT) = prev + trad.pnl(trade)
theta(lms) = getGreeks(lms).theta
price(lms) = bap(lms, .2)
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