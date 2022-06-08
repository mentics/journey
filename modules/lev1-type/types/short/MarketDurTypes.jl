module MarketDurTypes
using Dates, EnumX
using DateUtil

export MarketDur, MarketTime
export DUR_ZERO, DUR_CLOSED, DUR_WEND, DUR_HOLIDAY
# export isOpen, isHoliday

@enumx MTStatus open weekend holiday

# TODO: change InterTime to Interval
struct MarketTime
    status::MTStatus.T
    pres::InterTime
    opens::InterTime
    posts::InterTime
end
MarketTime(pres, opens, posts) = MarketTime(MTStatus.open, pres, opens, posts)
function MarketTime(data::Dict{String,Any})::MarketTime
    if data["status"] == "closed"
        date = Date(data["date"])
        return MarketTime(isweekend(date) ? MTStatus.weekend : MTStatus.holiday, INTERTIME_ZERO, INTERTIME_ZERO, INTERTIME_ZERO)
    else
        pres = timesFor(data["premarket"])
        opens = timesFor(data["open"])
        posts = timesFor(data["postmarket"])
        return MarketTime(MTStatus.open, pres, opens, posts)
    end
end
isOpen(mt::MarketTime) = mt.status == MTStatus.open
isHoliday(mt::MarketTime) = mt.status == MTStatus.holiday

struct MarketDur
    closed::Second
    pre::Second
    open::Second
    post::Second
    weekend::Second
    holiday::Second
end
MarketDur(; closed=SECOND_ZERO, pre=SECOND_ZERO, open=SECOND_ZERO, post=SECOND_ZERO, weekend=SECOND_ZERO, holiday=SECOND_ZERO) = MarketDur(closed, pre, open, post, weekend, holiday)
MarketDur(md::MarketDur; closed=md.closed, pre=md.pre, open=md.open, post=md.post, weekend=md.weekend, holiday=md.holiday) = MarketDur(closed, pre, open, post, weekend, holiday)
MarketDur(c, pre, o, post) = MarketDur(c, pre, o, post, SECOND_ZERO, SECOND_ZERO)
const DUR_ZERO = MarketDur(SECOND_ZERO, SECOND_ZERO, SECOND_ZERO, SECOND_ZERO, SECOND_ZERO, SECOND_ZERO)
const DUR_WEND = MarketDur(SECOND_ZERO, SECOND_ZERO, SECOND_ZERO, SECOND_ZERO, Hour(24), SECOND_ZERO)
const DUR_HOLIDAY = MarketDur(SECOND_ZERO, SECOND_ZERO, SECOND_ZERO, SECOND_ZERO, SECOND_ZERO, Hour(24))
function MarketDur(mt::MarketTime)::MarketDur
    if isOpen(mt)
        closed = (first(mt.pres) - TIME_ZERO) + (first(mt.opens) - last(mt.pres)) + (first(mt.posts) - last(mt.opens)) + (TIME_ZERO - last(mt.posts) + Day(1))
        pre = last(mt.pres) - first(mt.pres)
        open = last(mt.opens) - first(mt.opens)
        post = last(mt.posts) - first(mt.posts)
        @assert closed + pre + open + post == Day(1)
        return MarketDur(closed, pre, open, post)
    else
        return isHoliday(mt) ? DUR_HOLIDAY : DUR_WEND
    end
end
Base.:(+)(x1::MarketDur, x2::MarketDur)::MarketDur = MarketDur(x1.closed + x2.closed, x1.pre + x2.pre, x1.open + x2.open, x1.post + x2.post, x1.weekend + x2.weekend, x1.holiday + x2.holiday)

#region Local
timesFor(data::Dict{String,Any})::InterTime = InterTime(Time(data["start"]), Time(data["end"]))
#endregion

end