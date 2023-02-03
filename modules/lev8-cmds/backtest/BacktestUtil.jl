module BacktestUtil
using LogUtil

export trad, ac, @blog

macro blog(exs...)
    prblk = Expr(:call, (esc(LogUtil.logit)))
    LogUtil.inner((:backtest, exs...), prblk)
    return Expr(:block, prblk)
end

# blog(args...) = LogUtil.logit(:backtest, args...)

#region Account
module ac
using Dates
using BaseTypes, SmallTypes, BackTypes

acctTsStart(acct)::DateTime =
    isempty(acct.open) ?
        (isempty(acct.closed) ? DATETIME_ZERO : acct.closed[1].open.ts) :
        (isempty(acct.closed) ? acct.open[1].ts : min(acct.open[1].ts, acct.closed[1].open.ts))

marginMax(acct::Account)::PT = acct.bal - 50.0 # TODO: make param?
marginAvail(acct)::Sides{PT} = ( mx = marginMax(acct) ; Sides(mx - acct.margin.long.margin, mx - acct.margin.short.margin) )

function marginAdd(bal::PT, m1::MarginInfo, side::Side.T, multiple::Int, margAdd::PT)::MarginInfo
    if side == Side.long
        long = MarginSide(m1.long.margin + margAdd, m1.long.count + multiple)
        short = m1.short
    else
        long = m1.long
        short = MarginSide(m1.short.margin + margAdd, m1.short.count + multiple)
    end
    tot = max(long.margin, short.margin)
    rat = tot / bal
    return MarginInfo(tot, rat, long, short)
end

# function marginAdd(bal::PT, m1::MarginInfo, m2::MarginInfo)::MarginInfo
#     mlong = marginSideAdd(m1.long, m2.long)
#     mshort = marginSideAdd(m1.short, m2.short)
#     tot = max(mlong.margin, mshort.margin)
#     rat = tot / bal
#     return MarginInfo(tot, rat, mlong, mshort)
# end
# marginSideAdd(m1::MarginSide, m2::MarginSide) = MarginSide(m1.margin + m2.margin, m1.count + m2.count)
# function marginSub(bal::PT, m1::MarginInfo, m2::MarginInfo)::MarginInfo
#     mlong = marginSideSub(m1.long, m2.long)
#     mshort = marginSideSub(m1.short, m2.short)
#     tot = max(mlong.margin, mshort.margin)
#     rat = tot / bal
#     return MarginInfo(tot, rat, mlong, mshort)
# end
# marginSideSub(m1::MarginSide, m2::MarginSide) = MarginSide(m1.margin - m2.margin, m1.count - m2.count)

end
#endregion

#region Trade
module trad

using SH, Dates, BackTypes, DateUtil, CollUtil
side(trade::TradeBTOpen) = getSide(trade.lms[1])
targetDate(t::TradeBTOpen) = getExpir(t.lms[1])
targetDate(trade::TradeBT) = targetDate(trade.open)
targetRatio(t::TradeBTOpen, date::Date) = ( to = targetDate(t) ; bdays(date, to) / bdays(Date(t.ts), to) )
tsClose(trade::TradeBT) = trade.close.ts
pnl(trade) = trade.open.neto + trade.close.netc
# rate(trade) = DateUtil.calcRate(Date(trade.open.ts), Date(trade.close.ts), pnl(trade), max(trade.open.margin))
function rate(trade::TradeBT)
    # @show Date(trade.open.ts) Date(trade.close.ts) pnl(trade) max(trade.open.margin)
    DateUtil.calcRate(Date(trade.open.ts), Date(trade.close.ts), pnl(trade), max(trade.open.margin))
end
openDur(trade::TradeBT) = bdays(toDateMarket(trade.open.ts), toDateMarket(trade.close.ts)) + 1
bdaysLeft(from::Date, trade) = bdays(from, targetDate(trade))
closed(acct, id) = CollUtil.find(x -> x.open.id == id, acct.closed)

end
#endregion

end