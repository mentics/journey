module DateUtil
using Dates, BusinessDays, TimeZones
# TODO: exports are filtered, but unused functions need to be cleaned up

# export numDays
# export toDate, msMarketOpen, marketClose, msMarketClose, isNowAfterMarketClose, monthOfQuarter, lastTradingDay,
# export bdays, bdaysBefore, isbday
# export nowMs, tims, toms, msToDate,
export isBusDay, bdays, bdaysBefore, nextTradingDay, lastTradingDay
export nowMs, tims, toms, dateToMs, msToDate
export timeToExpir
export strShort
export isNowWithinMarket

#### new stuff

export ZERO_SECOND
export timeIn

export fromMarketTZ, toDateMarket, toTimeMarket
export fromLocal
export formatLocal
export nextLocalTime, nextMarketPeriod
# export toDateLocal
# export isAfterLocal

const MARKET_TZ = tz"America/New_York"
const ZERO_SECOND = Second(0)
const DAY_SECOND = Second(Day(1))

#region Basic
# TODO: maybe could simplify with Intervals
timeIn(timeFrom::Time, in::NTuple{2,Time})::Second = round(max(in[2], timeFrom) - max(in[1], timeFrom), Second)
timeIn(timeFrom::Time, mn::Time)::Second = DAY_SECOND - round(max(mn, timeFrom).instant, Second)
#endregion

#region Conversions
fromMarketTZ(d::Date, t::Time)::DateTime = DateTime(ZonedDateTime(DateTime(d, t), DateUtil.MARKET_TZ), UTC)
# toDateMarket(dt::DateTime)::Date = Date(astimezone(ZonedDateTime(dt, tz"UTC"), MARKET_TZ))
toDateMarket(ts::DateTime)::Date = Date(ZonedDateTime(ts, MARKET_TZ; from_utc=true))
# toDateLocal(ts::DateTime)::Date = Date(ZonedDateTime(ts, localzone(); from_utc=true))
toTimeMarket(ts::DateTime)::Time = Time(ZonedDateTime(ts, MARKET_TZ; from_utc=true))
#endregion

#region Parsing
fromLocal(str::AbstractString, df::DateFormat) = DateTime(ZonedDateTime(DateTime(str, df), localzone()), UTC)
formatLocal(dt::DateTime, format::DateFormat)::String = Dates.format(astimezone(ZonedDateTime(dt, tz"UTC"), localzone()), format)
#endregion

#region Formatting
strShort(d::Date)::String = Dates.format(d, DF_SHORT)
strShort(d1::Date, d2::Date)::String = strShort(d1) * '/' * strShort(d2) # "$(Dates.format(d1, DATEFORMAT_SHORT))/$(Dates.format(d2, DATEFORMAT_SHORT))"
strShort(ts::DateTime)::String = Dates.format(ZonedDateTime(ts, localzone(); from_utc=true), DTF_SHORT)
#endregion

#region Finding
# isAfterLocal(tim::Time) = now(localzone()) > todayat(tim, localzone()) # ZonedDateTime(DateTime(today(), Time(14, 0)), localzone(); from_utc=false)
nextLocalTime(from::DateTime, tim::Time) = ( dt = DateTime(todayat(tim, localzone()), UTC) ; return from < dt ? dt : dt + Day(1) )
function nextMarketPeriod(from::DateTime, isMktOpen::Bool, tsMktChange::DateTime, period::Period, before::Period, after::Period)
    @assert Dates.value(after) > 0
    @assert from <= tsMktChange
    if isMktOpen
        nextPeriod = round(from + before, period, RoundUp)
        timeNext = nextPeriod - before
        timeNext > from && timeNext < tsMktChange && return timeNext
    end
    return tsMktChange + after
end
#endregion

#region Iterating
#endregion

#region Local
const DF_SHORT = dateformat"mm-dd"
const DTF_SHORT = dateformat"mm-dd HH:MM:SS Z"
#endregion

###############################

# function __init__()
    # TODO: need this?
    # BusinessDays.initcache(:USNYSE)
# end

# function numDays() end

toDate(z::ZonedDateTime)::Date = Date(astimezone(z, localzone()))

isBusDay(d::Date) = isbday(:USNYSE, d)

bdays(d1::Date, d2::Date)::Int = bdayscount(:USNYSE, d1, d2)
# bdaysUntil(dt::Date) = days(Dates.today(), dt)
lastTradingDay(d::Date)::Date = tobday(:USNYSE, d; forward=false)
nextTradingDay(d::Date)::Date = tobday(:USNYSE, d; forward=true)
bdaysBefore(d::Date, n::Int)::Date = advancebdays(:USNYSE, lastTradingDay(d), -n)

# NOTE: crossing daylight savings time changes will make this off by an hour, but we don't do much in the middle of the night on a weekend.
# const ZEROT = Time(0,0,0)
# const OFFSET_MC = ZonedDateTime(today(), tz"Z") - ZonedDateTime(DateTime(dt, Time(16,0,0)), MARKET_TZ)
# const CLOSE_OFFSET_MS = (ZonedDateTime(DateTime(today(), Time(16,0,0)), MARKET_TZ) - ZonedDateTime(today(), localzone())).value
# const OPEN_OFFSET_MS = (ZonedDateTime(DateTime(today(), Time(9,30,0)), MARKET_TZ) - ZonedDateTime(today(), localzone())).value
# const LOCAL_OFFSET_MS = -1000 * Dates.value(ZonedDateTime(today(), localzone()).zone.offset)
# msMarketClose(dt::Date)::Int = 1000 * datetime2unix(DateTime(dt)) + CLOSE_OFFSET_MS + LOCAL_OFFSET_MS
# marketClose(dt::Date)::ZonedDateTime = ZonedDateTime(DateTime(dt, Time(16,0,0)), MARKET_TZ)
# msMarketOpen(dt::Date)::Int = 1000 * datetime2unix(DateTime(dt)) + OPEN_OFFSET_MS + LOCAL_OFFSET_MS
# TODO: switch these to use tradierClock
# isNowAfterMarketClose(dt::Date)::Bool = Dates.now(MARKET_TZ) > marketClose(dt)
# isNowAfterMarketClose(zdt::ZonedDateTime)::Bool = Dates.now(timezone(zdt)) > marketClose(Date(zdt))
# isNowWithisnMarket() = isBusDay(today()) && (msMarketOpen(today()) <= nowMs() <= msMarketClose(today()))

# monthOfQuarter(d::Date) =  month(d) - (quarterofyear(d)-1)*3

# tims(ms::Int)::ZonedDateTime = astimezone(ZonedDateTime(Dates.unix2datetime(ms/1000.0), tz"UTC"), localzone())
# tims(ms::Int)::ZonedDateTime = astimezone(TimeZones.unix2zdt(ms/1000), localzone())
toms(zdt::ZonedDateTime)::Int = Int(TimeZones.zdt2unix(zdt)*1000)
toms(dt::DateTime)::Int = Int(datetime2unix(dt)*1000)
msToDate(ms::Int)::Date = Date(tims(ms))
dateToMs(d::Date)::Int = toms(ZonedDateTime(d, localzone()))

# timeToExpir(dt::DateTime, expTo::Date)::Float64 = (msMarketClose(expTo) - Int(datetime2unix(dt)*1000)) / (365*24*60*60*1000)
# timeToExpir(msFrom::Int, expTo::Date)::Float64 = (msMarketClose(expTo) - msFrom) / (365*24*60*60*1000)
# timeToExpir(dtFrom::ZonedDateTime, expTo::Date)::Float64 = timeToExpir(toms(dtFrom), expTo)
# atpTiming(expFrom::Date, expTo::Date)::Float64 = (24.0 * ((expTo - expFrom).value - 1) + 16.0) / (365.0 * 24.0) # To match active trade pro timing

timeToExpir(expFrom::Date, expTo::Date)::Float64 = (expTo - expFrom).value / 365.0
timeToExpir(from::DateTime, to::DateTime)::Float64 = Millisecond(to - from).value / (365*24*60*60*1000)
# TODO: keep reconsidering this: it was messing up charts, so wasn't right?
# timeToExpir(expFrom::Date, expTo::Date)::Float64 = bdays(expFrom, expTo) / 365.0

nowMs() = round(Int, time()*1000)

end