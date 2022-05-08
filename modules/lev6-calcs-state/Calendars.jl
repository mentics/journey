module Calendars
using Dates
using DateUtil, LogUtil, ThreadUtil
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

check() = Info[].nextChange < now(UTC) && updateCalendar()
function updateCalendar()::Nothing
    runSync(Lock) do
        @log debug "updateCalendar"
        # TODO: lock
        isOpen, nextChange = tradierClock()
        cal = tradierCalendar()
        Info[] = CalInfo(isOpen, nextChange, cal, now(UTC))
        @log info "Updated calendar" isOpen nextChange
    end
    return
end
#endregion

end