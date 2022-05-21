module Calendars
using Dates
using Globals, DateUtil, LogUtil, ThreadUtil
using TradierData

export isMarketOpen, nextMarketChange, getMarketOpen, getMarketClose

struct CalInfo
    isOpen::Bool
    nextChange::DateTime
    cal::Dict{Date,Dict{String,Any}}
    ts::DateTime
end

isMarketOpen() = ( check() ; Info[].isOpen )
# TODO: change name to mktChangeNext
nextMarketChange() = ( check() ; Info[].nextChange )
getMarketOpen(d::Date) = fromMarketTZ(d, Time(Info[].cal[d]["open"]["start"]))
getMarketClose(d::Date) = fromMarketTZ(d, Time(Info[].cal[d]["open"]["end"]))
# DateTime(astimezone(ZonedDateTime(DateTime("$(d)T$(cal[d]["open"]["end"])"), tz"America/New_York"), tz"UTC"))

#region Local
const Info = Ref{CalInfo}(CalInfo(false, DateTime(0), Dict{Date,Dict{String,Any}}(), DateTime(0)))
const Lock = ReentrantLock()

function check()
    isnothing(snap()) || error("Session timing doesn't work when snapped")
    Info[].nextChange < now(UTC) && updateCalendar()
end

function updateCalendar(;from=(firstdayofmonth(today()) - Month(1)), to=(lastdayofmonth(today() + Month(3))))::Nothing
    runSync(Lock) do
        @log debug "updateCalendar"
        # TODO: lock
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
#endregion

end