module Calendars
using Dates
using DateUtil, LogUtil
using TradierData

export isMarketOpen, nextMarketChange, getMarketOpen, getMarketClose

struct CalInfo
    isOpen::Bool
    nextChange::DateTime
    cal::Dict{Date,Dict{String,Any}}
    ts::DateTime
end

isMarketOpen() = Info[].isOpen
# TODO: change name to mktChangeNext
nextMarketChange() = Info[].nextChange
getMarketOpen(d::Date) = fromMarketTZ(d, Time(Info[].cal[d]["open"]["start"]))
getMarketClose(d::Date) = fromMarketTZ(d, Time(Info[].cal[d]["open"]["end"]))
# DateTime(astimezone(ZonedDateTime(DateTime("$(d)T$(cal[d]["open"]["end"])"), tz"America/New_York"), tz"UTC"))

#region Local
const Info = Ref{CalInfo}()

function updateCalendar()::Nothing
    @log debug "updateCalendar"
    # TODO: lock
    isOpen, nextChange = tradierClock()
    cal = tradierCalendar()
    Info[] = CalInfo(isOpen, nextChange, cal, now(UTC))
    @log info "Updated calendar" Info[].isOpen Info[].nextChange
    return
end
#endregion

end