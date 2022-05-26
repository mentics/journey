module Calendars
using Dates
using Globals, DateUtil, LogUtil, ThreadUtil, MarketDurUtil
using TradierData

export isMarketOpen, nextMarketChange, getMarketOpen, getMarketClose
export getDursToExpr

isMarketOpen() = ( check() ; Info[].isOpen )
# TODO: change name to mktChangeNext
nextMarketChange() = ( check() ; Info[].nextChange )
getMarketOpen(d::Date) = fromMarketTZ(d, Time(Info[].cal[d]["open"]["start"]))
getMarketClose(d::Date) = fromMarketTZ(d, Time(Info[].cal[d]["open"]["end"]))
# DateTime(astimezone(ZonedDateTime(DateTime("$(d)T$(cal[d]["open"]["end"])"), tz"America/New_York"), tz"UTC"))

function getDursToExpr(ts::DateTime, expr::Date)::MarketDur
    dateBegin = toDateMarket(ts)
    if dateBegin == expr
        return getDursToClose(ts)
    else
        durs = getDursForDay(ts)
        for date in (dateBegin + Day(1)):Day(1):(expr - Day(1))
            durs += getDurs(date)
        end
        durs += getDursToClose(ts)
        return durs
    end
end

#region Local
struct CalInfo
    isOpen::Bool
    nextChange::DateTime
    cal::Dict{Date,Dict{String,Any}}
    ts::DateTime
end

const ROUNDUR = Second
const DAY_BEGIN = Time(0,0)
const Info = Ref{CalInfo}(CalInfo(false, DateTime(0), Dict{Date,Dict{String,Any}}(), DateTime(0)))
const Lock = ReentrantLock()

function __init__()
    updateCalendar()
end

function check()
    isnothing(snap()) || error("Session timing doesn't work when snapped")
    Info[].nextChange < now(UTC) && updateCalendar()
end

function updateCalendar(;from=(firstdayofmonth(today()) - Month(1)), to=(lastdayofmonth(today() + Month(3))))::Nothing
    runSync(Lock) do
        @log debug "updateCalendar"
        isOpen, nextChange = tradierClock()
        cal = tradierCalendar(from, to)
        Info[] = CalInfo(isOpen, nextChange, cal, now(UTC))
        @log info "Updated calendar" isOpen nextChange
    end
    return
end

ensureCal(dt::Dates.AbstractDateTime...)::Nothing = ensureCal(Date.(dt)...)
function ensureCal(dt::Date...)::Nothing
    from, to = extrema(dt)
    Info[].ts > DateTime(0) || ( updateCalendar(;from, to) ; return )
    mn, mx = extrema(keys(Info[].cal))
    from = min(from, mn)
    to = max(to, mx)
    (mn === from && mx === to) || updateCalendar(;from, to)
    return
end

roundur(period::Period)::Second = round(period, ROUNDUR)

function getDurs(d::Date)::MarketDur
    data = Info[].cal[d]
    if data["status"] == "closed"
        return MarketDur(Hour(24), ZERO, ZERO, ZERO)
    else
        # closed = ZERO ; pre = ZERO ; open = ZERO ; post = ZERO
        preBegin, preEnd = timesFor(data["premarket"])
        openBegin, openEnd = timesFor(data["open"])
        postBegin, postEnd = timesFor(data["postmarket"])
        closed = (preBegin - DAY_BEGIN) + (openBegin - preEnd) + (postBegin - openEnd) + (DAY_BEGIN - postEnd + Day(1))
        pre = preEnd - preBegin
        open = openEnd - openBegin
        post = postEnd - postBegin
        @assert closed + pre + open + post == Day(1)
        return MarketDur(closed, pre, open, post)
    end
end

function getDursToClose(tsFrom::DateTime)::MarketDur
    markTime = Info[].cal[toDateMarket(tsFrom)]
    timeFrom = toTimeMarket(tsFrom)
    openBegin, openEnd = timesFor(data["open"])
    if timeFrom >= openEnd
        return DURS_ZEROroundur
    elseif timeFrom >= openBegin
        return MarketDur(ZERO, ZERO, roundur(openEnd - timeFrom), ZERO)
    else
        open = openEnd - openBegin
        preBegin, preEnd = timesFor(data["premarket"])
        closed = roundur((openBegin - max(preEnd, timeFrom)) + (max(preBegin, timeFrom) - timeFrom))
        pre = roundur(preEnd - max(preBegin, timeFrom))
        return MarketDur(closed, pre, open, ZERO)
    end
end
#endregion

end