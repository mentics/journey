module Calendars
using Dates
using Globals, DateUtil, LogUtil, ThreadUtil, MarketDurUtil
using TradierData

export isMarketOpen, nextMarketChange, getMarketOpen, getMarketClose, marketTime

isMarketOpen() = ( check() ; Info[].isOpen )
# TODO: change name to mktChangeNext
nextMarketChange() = ( check() ; Info[].nextChange )
getMarketOpen(d::Date) = fromMarketTZ(d, ttFrom(marketTime(d).opens))
getMarketClose(d::Date) = fromMarketTZ(d, ttTo(marketTime(d).opens))
# DateTime(astimezone(ZonedDateTime(DateTime("$(d)T$(cal[d]["open"]["end"])"), tz"America/New_York"), tz"UTC"))
marketTime(d::Date)::MarketTime = Info[].markTime[d]

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
    mn, mx = extrema(keys(Info[].markTime))
    from = min(from, mn)
    to = max(to, mx)
    (mn === from && mx === to) || updateCalendar(;from, to)
    return
end

marketDur(d::Date)::MarketDur = Info[].markDur[d]

function calcDurToExpr(tsFrom::DateTime, dateTo::Date)::MarketDur
    dateFrom = toDateMarket(tsFrom)
    if dateFrom == dateTo
        return calcDurToClose(toTimeMarket(tsFrom), marketTime(dateTo))
    else
        dur = calcDurForDay(tsFrom, marketTime(dateFrom))
        for date in (dateFrom + Day(1)):Day(1):(dateTo - Day(1))
            dur += marketDur(date)
        end
        dur += calcDurToClose(ZERO_TIME, marketTime(dateTo))
        return dur
    end
end
#endregion

end