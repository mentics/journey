module MarketDurUtil
using Dates
using MarketDurs

export MarketDur, getDursForDay

function getDursForDay(tsFrom::DateTime)::MarketDur
    data = Info[].cal[toDateMarket(tsFrom)]
    timeFrom = toTimeMarket(tsFrom)
    pres = timesFor(data["premarket"])
    opens = timesFor(data["open"])
    posts = timesFor(data["postmarket"])
    closed = timeIn(timeFrom, (DAY_BEGIN, pres[1])) +
            timeIn(timeFrom, (pres[2], opens[1])) +
            timeIn(timeFrom, (opens[2], posts[1])) +
            timeIn(timeFrom, posts[2])

    closed = (preBegin - DAY_BEGIN) + (openBegin - preEnd) + (postBegin - openEnd) + (DAY_BEGIN - postEnd + Day(1))

    pre = timeIn(timeFrom, pres)
    open = timeIn(timeFrom, opens)
    post = timeIn(timeFrom, posts)
    return MarketDur(closed, pre, open, post)
end

end