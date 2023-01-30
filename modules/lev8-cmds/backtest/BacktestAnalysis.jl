module BacktestAnalysis
using Dates
using SH, BaseTypes, BackTypes, LegMetaTypes
import Backtests as bt, SimpleStore as SS
using DrawUtil, CollUtil, ChainUtil, BacktestUtil

#region Public
function showResult(acct=bt.keepAcct, params=bt.keepParams)::Nothing
    # balReals = foldl(red.balReal, acct.closed; init=[params.balInit])
    # balReals = maps(red.balReal, acct.closed, params.balInit)
    balReals = mapPoint(pts.balReal, acct.closed, (ac.acctTsStart(acct), params.balInit))
    draw(:scatter, balReals)

    # println(balReals)

    # balReal = params.balInit
    # for trade in acct.closed
    #     balReal += getPnl(trade)
    # end
    # println(pp((;balReals=balReals[end])))
    return
end

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