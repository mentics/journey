module MarketDurTypes
using Dates
using DateUtil

export MarketDur, MarketTime, ttTo, ttFrom
export ZERO_DUR

struct MarketTime
    isOpen::Bool
    pres::TwoTime
    opens::TwoTime
    posts::TwoTime
end
function MarketTime(data::Dict{String,Any})::MarketTime
    if data["status"] == "closed"
        return MarketTime(false, ZERO_TWOTIME, ZERO_TWOTIME, ZERO_TWOTIME)
    else
        pres = timesFor(data["premarket"])
        opens = timesFor(data["open"])
        posts = timesFor(data["postmarket"])
        return MarketTime(true, pres, opens, posts)
    end
end

struct MarketDur
    closed::Second
    pre::Second
    open::Second
    post::Second
end
MarketDur(; closed=ZERO_SECOND, pre=ZERO_SECOND, open=ZERO_SECOND, post=ZERO_SECOND) = MarketDur(closed, pre, open, post)
MarketDur(md::MarketDur; closed=md.closed, pre=md.pre, open=md.open, post=md.post) = MarketDur(closed, pre, open, post)
const ZERO_DUR = MarketDur(ZERO_SECOND, ZERO_SECOND, ZERO_SECOND, ZERO_SECOND)
const DUR_CLOSED = MarketDur(Hour(24), ZERO_SECOND, ZERO_SECOND, ZERO_SECOND)
function MarketDur(mt::MarketTime)::MarketDur
    if mt.isOpen
        # preBegin, preEnd = timesFor(data["premarket"])
        # openBegin, openEnd = timesFor(data["open"])
        # postBegin, postEnd = timesFor(data["postmarket"])
        closed = (ttFrom(mt.pres) - ZERO_TIME) + (ttFrom(mt.opens) - ttTo(mt.pres)) + (ttFrom(mt.posts) - ttTo(mt.opens)) + (ZERO_TIME - ttTo(mt.posts) + Day(1))
        pre = ttTo(mt.pres) - ttFrom(mt.pres)
        open = ttTo(mt.opens) - ttFrom(mt.opens)
        post = ttTo(mt.posts) - ttFrom(mt.posts)
        @assert closed + pre + open + post == Day(1)
        return MarketDur(closed, pre, open, post)
    else
        return DUR_CLOSED
    end
end
Base.:(+)(x1::MarketDur, x2::MarketDur)::MarketDur = MarketDur(x1.closed + x2.closed, x1.pre + x2.pre, x1.open + x2.open, x1.post + x2.post)

#region Local
ttFrom(tt::TwoTime)::Time = tt.first
ttTo(tt::TwoTime)::Time = tt.second

timesFor(data::Dict{String,Any})::TwoTime = TwoTime(Time(data["start"]), Time(data["end"]))
#endregion

end