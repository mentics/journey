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
using SH, BaseTypes, SmallTypes, BackTypes

acctTsStart(acct)::DateTime =
    isempty(acct.open) ?
        (isempty(acct.closed) ? DATETIME_ZERO : acct.closed[1].open.ts) :
        (isempty(acct.closed) ? acct.open[1].ts : min(acct.open[1].ts, acct.closed[1].open.ts))

marginMax(acct::Account)::PT = acct.bal - 50.0 # TODO: make param?
marginAvail(acct)::Sides{PT} = ( mx = marginMax(acct) ; Sides(mx - acct.margin.long.margin, mx - acct.margin.short.margin) )

function marginAdd(bal::PT, m1::MarginInfo, multiple::Int, margAdd::Sides{PT})::MarginInfo
    long = margAdd.long > 0 ? MarginSide(m1.long.margin + multiple * margAdd.long, m1.long.count + multiple) : m1.long
    short = margAdd.short > 0 ? MarginSide(m1.short.margin + multiple * margAdd.short, m1.short.count + multiple) : m1.short
    tot = max(long.margin, short.margin)
    rat = tot / bal
    return MarginInfo(tot, rat, long, short)
end

# function marginAdd(bal::PT, m1::MarginInfo, side::Side.T, multiple::Int, margAdd::PT)::MarginInfo
#     if side == Side.long
#         long = MarginSide(m1.long.margin + margAdd, m1.long.count + multiple)
#         short = m1.short
#     else
#         long = m1.long
#         short = MarginSide(m1.short.margin + margAdd, m1.short.count + multiple)
#     end
#     tot = max(long.margin, short.margin)
#     rat = tot / bal
#     return MarginInfo(tot, rat, long, short)
# end

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

using SH, Dates, BackTypes, LegQuoteTypes, DateUtil, CollUtil
side(trade::TradeBTOpen) = getSide(trade.lms[1])
SH.getTargetDate(t::TradeBTOpen) = minimum(getExpir, t.lms)
SH.getTargetDate(trade::TradeBT) = getTargetDate(trade.open)
targetRatio(t, from::Date) = ( to = getTargetDate(t) ; bdays(from, to) / bdays(Date(getDateOpen(t)), to) )
tsClose(trade::TradeBT) = trade.close.ts
pnl(trade) = getNetOpen(trade) + getNetClose(trade)
pnlm(trade) = trade.open.multiple * pnl(trade)
function calcRateOrig(trade)
    # @show getDateOpen(trade) getTargetDate(trade) getNetOpen(trade) getRisk(trade)
    DateUtil.calcRate(getDateOpen(trade), getTargetDate(trade), getNetOpen(trade), getRisk(trade))
end
calcRateOrig(tradeOpen::TradeBTOpen) = tradeOpen.extra.rate
# rate(trade) = DateUtil.calcRate(Date(trade.open.ts), Date(trade.close.ts), pnl(trade), max(trade.open.margin))
function rate(trade::TradeBT)
    # @show Date(trade.open.ts) Date(trade.close.ts) pnl(trade) max(trade.open.margin)
    DateUtil.calcRate(Date(getDateOpen(trade)), Date(trade.close.ts), pnl(trade), getRisk(trade.open))
end
openDur(trade::TradeBT) = bdays(toDateMarket(trade.open.ts), toDateMarket(trade.close.ts)) + 1
bdaysLeft(from::Date, trade) = bdays(from, getTargetDate(trade))
open(acct, id) = CollUtil.find(x -> x.id == id, acct.open)
closed(acct, id) = CollUtil.find(x -> x.open.id == id, acct.closed)
function calcCloseInfo(trade::TradeBTOpen, ts, otoq, calcPrice)
    lmsc = tosn(LegQuoteClose, trade.lms, otoq)
    !isnothing(lmsc) || return nothing
    netc = calcPrice(lmsc)
    cv = trade.neto + netc
    rate = calcRate(Date(trade.ts), Date(ts), cv, trade.extra.risk)
    return (;rate, curVal=cv, trade.neto, netc, lmsc, trade)
end
SH.getDateOpen(tradeOpen::TradeBT) = Date(tradeOpen.open.ts)
SH.getDateOpen(tradeOpen::TradeBTOpen) = Date(tradeOpen.ts)
SH.getRisk(tradeOpen::TradeBTOpen) = tradeOpen.extra.risk
SH.getNetOpen(tradeOpen::TradeBT) = tradeOpen.open.neto
SH.getNetOpen(tradeOpen::TradeBTOpen) = tradeOpen.neto
SH.getNetClose(tradeOpen::TradeBT) = tradeOpen.close.netc
SH.getLegs(tradeOpen::TradeBTOpen) = tradeOpen.lms
#endregion

end

end