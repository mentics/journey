module MarketDurUtil
using Dates
using DateUtil
using MarketDurTypes

export MarketDur, MarketTime, ZERO_DUR, ttFrom, ttTo
export calcDurForDay, calcDurToClose

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

function calcDurToClose(from::Time, mt::MarketTime)::MarketDur
    if from >= ttTo(mt.opens)
        return ZERO_DUR
    elseif from >= ttFrom(mt.opens)
        return MarketDur(ZERO_SECOND, ZERO_SECOND, roundur(ttTo(mt.opens) - from), ZERO_SECOND)
    else
        closed = roundur((ttFrom(mt.opens) - max(ttTo(mt.pres), from)) + (max(ttFrom(mt.pres), from) - from))
        pre = roundur(ttTo(mt.pres) - max(ttFrom(mt.pres), from))
        open = ttTo(mt.opens) - ttFrom(mt.opens)
        return MarketDur(closed, pre, open, ZERO_SECOND)
    end
end

#region Local
const ROUNDUR = Second
roundur(period::Union{Period,Dates.CompoundPeriod})::Second = round(period, ROUNDUR)
Base.round(p::Dates.CompoundPeriod, ::Type{P}) where P<:Period = round(convert(Nanosecond, p), P)
#endregion

end