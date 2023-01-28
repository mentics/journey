module BacktestAnalysis
using BaseTypes, BackTypes
import Backtests as bt
using DrawUtil, CollUtil

#region Public
function showResult(acct=bt.keepAcct, params=bt.keepParams)::Nothing
    # balReals = foldl(red.balReal, acct.closed; init=[params.balInit])
    # balReals = maps(red.balReal, acct.closed, params.balInit)
    balReals = mapPoint(pts.balReal, acct.closed, (bt.acctTsStart(acct), params.balInit))
    draw(:scatter, balReals)

    # println(balReals)

    # balReal = params.balInit
    # for trade in acct.closed
    #     balReal += getPnl(trade)
    # end
    # println(pp((;balReals=balReals[end])))
    return
end
#endregion

#region Points
module pts
import ..Currency, ..TradeBT, ..bt.trad
balReal(prev::Currency, trade::TradeBT) = prev + trad.pnl(trade)
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