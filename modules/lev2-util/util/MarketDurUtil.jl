module MarketDurUtil
using Dates
using DateUtil
using MarketDurTypes

export MarketDur, MarketTime, DUR_ZERO, first, last
export calcDurInDay

function calcDurInDay(from::Time, to::Time, mt::MarketTime)::MarketDur
    closed = timeIn(from, to, InterTime(TIME_ZERO, first(mt.pres))) +
             timeIn(from, to, InterTime(last(mt.pres), first(mt.opens))) +
             timeIn(from, to, InterTime(last(mt.opens), first(mt.posts))) +
             timeIn(from, to, InterTime(last(mt.posts), TIME_EOD))
    pre = timeIn(from, to, mt.pres)
    open = timeIn(from, to, mt.opens)
    post = timeIn(from, to, mt.posts)
    return MarketDur(closed, pre, open, post)
end

# calcDurForDay(tsFrom::DateTime, mt::MarketTime)::MarketDur = calcDurForDay(toTimeMarket(tsFrom), mt)
# function calcDurForDay(from::Time, mt::MarketTime)::MarketDur
#     closed = timeIn(from, InterTime(TIME_ZERO, first(mt.pres))) +
#             timeIn(from, InterTime(last(mt.pres), first(mt.opens))) +
#             timeIn(from, InterTime(last(mt.opens), first(mt.posts))) +
#             timeIn(from, last(mt.posts))
#     # (first(mt.pres) - DAY_BEGIN) + (first(mt.open) - last(mt.pre)) + (first(mt.post) - last(mt.open)) + (DAY_BEGIN - last(mt.post) + Day(1))
#     pre = timeIn(from, mt.pres)
#     open = timeIn(from, mt.opens)
#     post = timeIn(from, mt.posts)
#     return MarketDur(closed, pre, open, post)
# end

# function calcDurToClose(from::Time, mt::MarketTime)::MarketDur
#     if from >= last(mt.opens)
#         return DUR_ZERO
#     elseif from >= first(mt.opens)
#         return MarketDur(SECOND_ZERO, SECOND_ZERO, roundur(last(mt.opens) - from), SECOND_ZERO)
#     else
#         closed = roundur((first(mt.opens) - max(last(mt.pres), from)) + (max(first(mt.pres), from) - from))
#         pre = roundur(last(mt.pres) - max(first(mt.pres), from))
#         open = last(mt.opens) - first(mt.opens)
#         return MarketDur(closed, pre, open, SECOND_ZERO)
#     end
# end

#region Local
const ROUNDUR = Second
roundur(period::Union{Period,Dates.CompoundPeriod})::Second = round(period, ROUNDUR)
Base.round(p::Dates.CompoundPeriod, ::Type{P}) where P<:Period = round(convert(Nanosecond, p), P)
#endregion

end