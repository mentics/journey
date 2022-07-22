module Calendars
using Dates
using Globals, DateUtil, LogUtil, ThreadUtil, MarketDurUtil
using TradierData

export isMarketOpen, nextMarketChange, getMarketOpen, getMarketClose, getMarketTime
export calcTex, calcDurToExpr

function isMarketOpen()
    isnothing(snap()) || return true # snave is not allowed when market is not open
    check()
    return Info[].isOpen
end
# TODO: change name to mktChangeNext
nextMarketChange() = ( check() ; Info[].nextChange )
getMarketOpen(d::Date) = fromMarketTZ(d, first(getMarketTime(d).opens))
getMarketClose(d::Date) = fromMarketTZ(d, last(getMarketTime(d).opens))
# DateTime(astimezone(ZonedDateTime(DateTime("$(d)T$(cal[d]["open"]["end"])"), tz"America/New_York"), tz"UTC"))
getMarketTime(d::Date)::MarketTime = Info[].markTime[d]
# calcTimeToClose(ts::DateTime, d::Date)::Period = ts - getMarketClose(d)

# in hours
function calcTex(tsFrom::DateTime, dateTo::Date)::Float64
    dur = calcDurToExpr(tsFrom, dateTo)
    closed = Dates.value(dur.closed)
    pre = Dates.value(dur.pre)
    open = Dates.value(dur.open)
    post = Dates.value(dur.post)
    other = 0.3 * (Dates.value(dur.weekend) + Dates.value(dur.holiday)) # 0.3, and leaving the others at 1 was calculated by SpreadPricing.optTex()
    tex = (closed + pre + open + post + other)/3600.0
    return tex
end

function calcDurToExpr(tsFrom::DateTime, dateTo::Date)::MarketDur
    dateFrom = toDateMarket(tsFrom)
    if dateFrom == dateTo
        return calcDurToClose(toTimeMarket(tsFrom), getMarketTime(dateTo))
    else
        dur = calcDurForDay(tsFrom, getMarketTime(dateFrom))
        for date in (dateFrom + Day(1)):Day(1):(dateTo - Day(1))
            dur += marketDur(date)
        end
        dur += calcDurToClose(TIME_ZERO, getMarketTime(dateTo))
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

const Info = Ref{CalInfo}(CalInfo(false, DATETIME_ZERO, Dict(), Dict(), DATETIME_ZERO))
const Lock = ReentrantLock()

function __init__()
    updateCalendar()
end

function check()
    isnothing(snap()) || error("Session timing doesn't work when snapped")
    Info[].nextChange < now(UTC) && updateCalendar()
end

function updateCalendar(;from=(firstdayofmonth(today()) - Year(1)), to=(lastdayofmonth(today() + Year(1))))::Nothing
    runSync(Lock) do
        @log debug "updateCalendar"
        isOpen, nextChange = today() != Date(2022,6,20) ? tradierClock() : (false, DateTime("2022-06-20T20:00:00"))
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

function calcDurPerYear()
    dur = marketDur(Date(2022,1,1))
    for day in Date(2022,1,2):Day(1):Date(2022,12,31)
        dur += marketDur(day)
    end
    return dur
end

marketDur(d::Date)::MarketDur = Info[].markDur[d]
#endregion

end