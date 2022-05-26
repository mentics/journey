module MarketDurs
using Dates
using DateUtil

export MarketDur

struct MarketTime
    isOpen::Bool
    pres::TupTime
    opens::TupTime
    posts::TupTime
end
function MarketTime(data::Dict{String,Any})::MarketTime
    if data["status"] == "closed"
        return MarketTime(false, )
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
const DURS_ZERO = MarketDur(ZERO_SECOND, ZERO_SECOND, ZERO_SECOND, ZERO_SECOND)
Base.:(+)(x1::MarketDur, x2::MarketDur)::MarketDur = Durs(x1.closed + x2.closed, x1.pre + x2.pre, x1.open + x2.open, x1.post + x2.post)

#region Local
const TupTime = NTuple{2,Time}
timesFor(data::Dict{String,Any})::NTuple{2,Time} = (Time(data["start"]), Time(data["end"]))
#endregion

end