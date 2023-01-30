module BacktestUtil

export trad, ac

#region Account
module ac
using Dates
using BaseTypes, SmallTypes, BackTypes

acctTsStart(acct)::DateTime =
    isempty(acct.open) ?
        (isempty(acct.closed) ? DATETIME_ZERO : acct.closed[1].open.ts) :
        (isempty(acct.closed) ? acct.open[1].ts : min(acct.open[1].ts, acct.closed[1].open.ts))

marginMax(acct::Account)::PT = acct.bal * .8 # TODO: make param?
marginAvail(acct)::Sides{PT} = ( mx = marginMax(acct) ; Sides(mx - acct.margin.long.margin, mx - acct.margin.short.margin) )

end
#endregion

#region Trade
module trad

using SH, Dates, BackTypes, DateUtil, CollUtil
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