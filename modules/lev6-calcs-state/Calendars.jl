module Calendars
using Dates
using Globals, DateUtil, LogUtil, ThreadUtil, MarketDurUtil
using TradierData

export isMarketOpen, nextMarketChange, getMarketOpen, getMarketClose
export calcDurToExpr

isMarketOpen() = ( check() ; Info[].isOpen )
# TODO: change name to mktChangeNext
nextMarketChange() = ( check() ; Info[].nextChange )
getMarketOpen(d::Date) = fromMarketTZ(d, ttFrom(Info[].markTime[d].opens))
getMarketClose(d::Date) = fromMarketTZ(d, ttTo(Info[].markTime[d].opens))
# DateTime(astimezone(ZonedDateTime(DateTime("$(d)T$(cal[d]["open"]["end"])"), tz"America/New_York"), tz"UTC"))

function calcDurToExpr(ts::DateTime, exp::Date)::MarketDur
    dateBegin = toDateMarket(ts)
    durExp = calcDurToClose(ts, marketTime(exp))
    if dateBegin == exp
        return durExp
    else
        dur = calcDurForDay(ts, marketTime(dateBegin))
        for date in (dateBegin + Day(1)):Day(1):(exp - Day(1))
            dur += marketDur(date)
        end
        dur += durExp
        return dur
    end
end

#region Local
struct CalInfo
    isOpen::Bool
    nextChange::DateTime
    markTime::Dict{Date,MarketTime}
    markDur::Dict{Date,MarketDur}
    ts::DateTime
end

const Info = Ref{CalInfo}(CalInfo(false, ZERO_DATETIME, Dict(), Dict(), ZERO_DATETIME))
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
        markTime = Dict(d => MarketTime(data) for (d, data) in cal)
        markDur = Dict(d => MarketDur(mt) for (d, mt) in markTime)
        Info[] = CalInfo(isOpen, nextChange, markTime, markDur, now(UTC))
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

marketTime(d::Date)::MarketTime = Info[].markTime[d]
marketDur(d::Date)::MarketDur = Info[].markDur[d]
#endregion

end