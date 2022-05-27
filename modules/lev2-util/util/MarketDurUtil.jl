module MarketDurUtil
using Dates
using DateUtil
using MarketDurs
import MarketDurs:ttFrom,ttTo

export MarketDur, MarketTime, calcDurForDay, calcDurToClose, ttFrom, ttTo

calcDurForDay(tsFrom::DateTime, mt::MarketTime)::MarketDur = calcDurForDay(toTimeMarket(tsFrom), mt)
function calcDurForDay(from::Time, mt::MarketTime)::MarketDur
    closed = timeIn(from, TwoTime(ZERO_TIME, ttFrom(mt.pres))) +
            timeIn(from, TwoTime(ttTo(mt.pres), ttFrom(mt.opens))) +
            timeIn(from, TwoTime(ttTo(mt.opens), ttFrom(mt.posts))) +
            timeIn(from, ttTo(mt.posts))
    # (ttFrom(mt.pres) - DAY_BEGIN) + (ttFrom(mt.open) - ttTo(mt.pre)) + (ttFrom(mt.post) - ttTo(mt.open)) + (DAY_BEGIN - ttTo(mt.post) + Day(1))
    pre = timeIn(from, mt.pres)
    open = timeIn(from, mt.opens)
    post = timeIn(from, mt.posts)
    return MarketDur(closed, pre, open, post)
end

function calcDurToClose(tsFrom::DateTime, mt::MarketTime)::MarketDur
    from = toTimeMarket(tsFrom)
    if from >= ttTo(mt.opens)
        return DURS_ZERO
    elseif from >= ttFrom(mt.opens)
        return MarketDur(ZERO, ZERO, roundur(ttTo(mt.opens) - from), ZERO)
    else
        open = ttTo(mt.opens) - ttFrom(mt.opens)
        closed = roundur((ttFrom(mt.opens) - max(preEnd, from)) + (max(preBegin, from) - from))
        pre = roundur(preEnd - max(preBegin, from))
        return MarketDur(closed, pre, open, ZERO)
    end
end

#region Local
const ROUNDUR = Second
roundur(period::Period)::Second = round(period, ROUNDUR)
#endregion

end