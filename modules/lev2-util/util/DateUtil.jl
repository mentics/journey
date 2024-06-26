module DateUtil
using Dates, BusinessDays, TimeZones, Intervals, Memoization
# TODO: exports are filtered, but unused functions need to be cleaned up

# export numDays
# export toDate, msMarketOpen, marketClose, msMarketClose, isNowAfterMarketClose, monthOfQuarter, lastTradingDay,
# export bdays, bdaysBefore, isbday
# export nowMs, tims, toms, msToDate,
export isBusDay, bdays, bdaysBefore, bdaysAfter, nextTradingDay, lastTradingDay
export nowz, nowMs, tims, toms, dateToMs, msToDate
export timeToExpir
export strShort
export isNowWithinMarket

#### new stuff

export SECOND_ZERO, TIME_ZERO, TIME_EOD, DATE_ZERO, DATETIME_ZERO, INTERTIME_ZERO
export DATE_FUTURE
export InterTime, timeIn

export fromMarketTZ, market_date, toTimeMarket
export fromLocal, toTimeLocal
export formatLocal
export nextLocalTime, nextMarketPeriod
# export toDateLocal
# export isAfterLocal
export isweekend # reexport from BusinessDays
export getDate

export daysinquarter # , daysinyear

#region ConstAndTypes
# TimeZones.build()
export LOCALZONE, MARKET_TZ
const LOCALZONE = localzone()
const MARKET_TZ = tz"America/New_York"

export DateLike
const DateLike = Union{Date,Dates.AbstractDateTime}
#endregion

export timult, calcRate
bdaysPerYear() = 252.0
durRisk(from::DateLike, to::DateLike) = 1 + bdays_t(from, to)
timult(from::DateLike, to::DateLike) = bdaysPerYear() / durRisk(from, to)
riskyears(from::DateLike, to::DateLike) = durRisk(from, to) / bdaysPerYear()
durtimult(from::Date, to::Date) = ( dur = durRisk(from, to) ; (dur, bdaysPerYear() / dur) )
calcRate(from::Date, to::Date, ret, risk)::Float64 = (ret / Float64(risk)) * timult(from, to)
calcRate(tmult, ret, risk)::Float64 = (ret / Float64(risk)) * tmult
# vixToSdev(vix, bdays) = vix / sqrt(bdaysPerYear() / bdays)

#region Basic
const SECOND_ZERO = Second(0)
const SECOND_DAY = Second(Day(1))
const TIME_ZERO = Time(0)
const TIME_EOD = Time(23,59,59,999,999)
const DATETIME_ZERO = DateTime(0)
const DATE_ZERO = Date(0)

const DATE_FUTURE = today() + Year(100)

const InterTime = Interval{Time,Closed,Closed}
const INTERTIME_ZERO = Interval(TIME_ZERO, TIME_ZERO)

getDate(x::NamedTuple) = x.date

# TODO: maybe could simplify with Intervals
# timeIn(from::Time, in::InterTime)::Second = round(max(last(in), from) - max(first(in), from), Second)
timeIn(from::Time, to::Time, in::InterTime)::Second = round(span(intersect(InterTime(from, to), in)), Second) # round(min(last(in), to) - max(first(in), from), Second)
# timeIn(from::Time, to::Time, mn::Time)::Second = SECOND_DAY - round(max(mn, from).instant, Second)
#endregion

#region Conversions
fromMarketTZ(d::Date, t::Time)::DateTime = DateTime(ZonedDateTime(DateTime(d, t), DateUtil.MARKET_TZ), UTC)
toMarketTZ(ts::DateTime) = ZonedDateTime(ts, MARKET_TZ; from_utc=true)
# toDateMarket(dt::DateTime)::Date = Date(astimezone(ZonedDateTime(dt, tz"UTC"), MARKET_TZ))
# toDateLocal(ts::DateTime)::Date = Date(ZonedDateTime(ts, LOCALZONE; from_utc=true))
toTimeMarket(ts::DateTime)::Time = Time(ZonedDateTime(ts, MARKET_TZ; from_utc=true))
toTimeLocal(ts::DateTime)::Time = Time(ZonedDateTime(ts, LOCALZONE; from_utc=true))
to_local(ts::DateTime) = ZonedDateTime(ts, LOCALZONE; from_utc=true)
to_local(s::Int) = ZonedDateTime(unix2datetime(s), LOCALZONE; from_utc=true)
market_now()::ZonedDateTime = toMarketTZ(now(UTC))
market_midnight(date::Date)::DateTime = fromMarketTZ(date, Time(0,0))
market_date(ts::DateTime)::Date = Date(ZonedDateTime(ts, MARKET_TZ; from_utc=true))
market_date(ts::ZonedDateTime)::Date = Date(astimezone(ts, MARKET_TZ))
market_today()::Date = lastTradingDay(market_now())
toDateMarket(ts::DateTime)::Date = Date(ZonedDateTime(ts, MARKET_TZ; from_utc=true)) # deprecated
#endregion

#region Parsing
fromLocal(str::AbstractString, df::DateFormat=ISODateTimeFormat) = DateTime(ZonedDateTime(DateTime(str, df), LOCALZONE), UTC)
formatLocal(dt::DateTime, format::DateFormat=ISODateTimeFormat)::String = Dates.format(astimezone(ZonedDateTime(dt, tz"UTC"), LOCALZONE), format)
#endregion

#region Formatting
strShort(d::Date; dateYear=false)::String = dateYear ? Dates.format(d, DF_SHORT_YEAR) : Dates.format(d, DF_SHORT)
strShort(d1::Date, d2::Date)::String = strShort(d1) * '/' * strShort(d2) # "$(Dates.format(d1, DATEFORMAT_SHORT))/$(Dates.format(d2, DATEFORMAT_SHORT))"
strShort(ts::DateTime)::String = Dates.format(ZonedDateTime(ts, LOCALZONE; from_utc=true), DTF_SHORT)
#endregion

#region Finding
# isAfterLocal(tim::Time) = now(LOCALZONE) > todayat(tim, LOCALZONE) # ZonedDateTime(DateTime(today(), Time(14, 0)), LOCALZONE; from_utc=false)
nextLocalTime(from::DateTime, tim::Time) = ( dt = DateTime(todayat(tim, LOCALZONE), UTC) ; return from < dt ? dt : dt + Day(1) )
function nextMarketPeriod(from::DateTime, isMktOpen::Bool, tsMktChange::DateTime, period::Period, before::Period, after::Period)
    @assert Dates.value(after) > 0
    # @info "nextMarketPeriod" from tsMktChange
    @assert from <= tsMktChange
    @assert before < period
    if isMktOpen
        nextPeriod = round(Millisecond(1) + from + before, period, RoundUp) # add a little in case from is exactly at a period
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
const DF_SHORT_YEAR = dateformat"yy-mm-dd"
const DTF_SHORT = dateformat"mm-dd HH:MM:SS Z"
#endregion

###############################

function __init__()
    BusinessDays.initcache(BusinessDays.USNYSE())
end

# function numDays() end

toDate(z::ZonedDateTime)::Date = Date(astimezone(z, LOCALZONE))


using Memoization, ThreadSafeDicts
export isbd
# TODO: cleanup
isBusDay(d::DateLike) = isbd_t(d)
isbd(d::DateLike) = isbd_t(d)
# @memoize ThreadSafeDicts.ThreadSafeDict isbd_t(d::DateLike) = isbday(BusinessDays.USNYSE(), d)
isbd_t(d::DateTime) = isbd_t(market_date(d))
@memoize isbd_t(d::Date) = isbday(BusinessDays.USNYSE(), d)

bdays(d1::DateLike, d2::DateLike)::Int = bdays_t(d1, d2)
# @memoize ThreadSafeDicts.ThreadSafeDict bdays_t(d1::DateLike, d2::DateLike)::Int = bdayscount(BusinessDays.USNYSE(), Date(d1), Date(d2))
@memoize bdays_t(d1::DateLike, d2::DateLike)::Int = bdayscount(BusinessDays.USNYSE(), Date(d1), Date(d2))
# @memoize bdays(d1::Date, d2::Date)::Int = bdayscount(BusinessDays.USNYSE(), d1, d2)

lastTradingDay(d::Date)::Date = tobday(BusinessDays.USNYSE(), d; forward=false)
lastTradingDay(ts::DateTime)::Date = tobday(BusinessDays.USNYSE(), market_date(ts); forward=false)
lastTradingDay(zdt::ZonedDateTime)::Date = tobday(BusinessDays.USNYSE(), market_date(zdt); forward=false)
nextTradingDay(d::Date)::Date = tobday(BusinessDays.USNYSE(), d; forward=true)
bdaysBefore(d::Date, n::Int)::Date = advancebdays(BusinessDays.USNYSE(), lastTradingDay(d), -n)
bdaysAfter(d::Date, n::Int)::Date = advancebdays(BusinessDays.USNYSE(), lastTradingDay(d), n)

function matchAfter(from::Date, dates, bdays)
    dateMin = bdaysAfter(from, first(bdays))
    dateMax = bdaysAfter(from, last(bdays))
    return filter(x -> dateMin <= x <= dateMax, dates)
end

# NOTE: crossing daylight savings time changes will make this off by an hour, but we don't do much in the middle of the night on a weekend.
# const ZEROT = Time(0,0,0)
# const OFFSET_MC = ZonedDateTime(today(), tz"Z") - ZonedDateTime(DateTime(dt, Time(16,0,0)), MARKET_TZ)
# const CLOSE_OFFSET_MS = (ZonedDateTime(DateTime(today(), Time(16,0,0)), MARKET_TZ) - ZonedDateTime(today(), LOCALZONE)).value
# const OPEN_OFFSET_MS = (ZonedDateTime(DateTime(today(), Time(9,30,0)), MARKET_TZ) - ZonedDateTime(today(), LOCALZONE)).value
# const LOCAL_OFFSET_MS = -1000 * Dates.value(ZonedDateTime(today(), LOCALZONE).zone.offset)
# msMarketClose(dt::Date)::Int = 1000 * datetime2unix(DateTime(dt)) + CLOSE_OFFSET_MS + LOCAL_OFFSET_MS
# marketClose(dt::Date)::ZonedDateTime = ZonedDateTime(DateTime(dt, Time(16,0,0)), MARKET_TZ)
# msMarketOpen(dt::Date)::Int = 1000 * datetime2unix(DateTime(dt)) + OPEN_OFFSET_MS + LOCAL_OFFSET_MS
# TODO: switch these to use tradierClock
# isNowAfterMarketClose(dt::Date)::Bool = Dates.now(MARKET_TZ) > marketClose(dt)
# isNowAfterMarketClose(zdt::ZonedDateTime)::Bool = Dates.now(timezone(zdt)) > marketClose(Date(zdt))
# isNowWithisnMarket() = isBusDay(today()) && (msMarketOpen(today()) <= nowMs() <= msMarketClose(today()))

# monthOfQuarter(d::Date) =  month(d) - (quarterofyear(d)-1)*3

# tims(ms::Int)::ZonedDateTime = astimezone(ZonedDateTime(Dates.unix2datetime(ms/1000.0), tz"UTC"), LOCALZONE)
# tims(ms::Int)::ZonedDateTime = astimezone(TimeZones.unix2zdt(ms/1000), LOCALZONE)
toms(zdt::ZonedDateTime)::Int = Int(TimeZones.zdt2unix(zdt)*1000)
toms(dt::DateTime)::Int = Int(datetime2unix(dt)*1000)
msToDate(ms::Int)::Date = Date(tims(ms))
dateToMs(d::Date)::Int = toms(ZonedDateTime(d, LOCALZONE))

# timeToExpir(dt::DateTime, expTo::Date)::Float64 = (msMarketClose(expTo) - Int(datetime2unix(dt)*1000)) / (365*24*60*60*1000)
# timeToExpir(msFrom::Int, expTo::Date)::Float64 = (msMarketClose(expTo) - msFrom) / (365*24*60*60*1000)
# timeToExpir(dtFrom::ZonedDateTime, expTo::Date)::Float64 = timeToExpir(toms(dtFrom), expTo)
# atpTiming(expFrom::Date, expTo::Date)::Float64 = (24.0 * ((expTo - expFrom).value - 1) + 16.0) / (365.0 * 24.0) # To match active trade pro timing

timeToExpir(expFrom::Date, expTo::Date)::Float64 = (expTo - expFrom).value / 365.0
timeToExpir(from::DateTime, to::DateTime)::Float64 = Millisecond(to - from).value / (365*24*60*60*1000)
# TODO: keep reconsidering this: it was messing up charts, so wasn't right?
# timeToExpir(expFrom::Date, expTo::Date)::Float64 = bdays(expFrom, expTo) / 365.0

nowMs() = round(Int, time()*1000)
nowz() = now(LOCALZONE)

function toMonths(from::Date, to::Date)
    @assert from < to
    firstdayofmonth(from):Month(1):firstdayofmonth(to)
end

daysinquarter(d)::UInt16 = ( q1 = Dates.firstdayofquarter(d) ; (q1 + Dates.Month(3) - q1).value )
# daysinyear(d)::UInt16 = ( q1 = Dates.firstdayofquarter(d) ; (q1 + Dates.Month(3) - q1).value )

const DEFAULT_DATA_START_DATE = Date(2012,6,1)
const DEFAULT_TS_PERIOD = Minute(30)

function year_months(;start_date=DEFAULT_DATA_START_DATE, end_date=market_today())
    [(;year=year(d), month=month(d)) for d in start_date:Month(1):end_date]
end

all_bdays(from, to) = BusinessDays.listbdays(BusinessDays.USNYSE(), from, to)

function all_weekdays(date_from=DEFAULT_DATA_START_DATE, date_to=market_today())
    return Iterators.filter(d -> Dates.dayofweek(d) <= 5, date_from:Day(1):date_to)
end

function all_weekday_ts(;date_from=DEFAULT_DATA_START_DATE, date_to=market_today(), time_from=Time(9, 30), time_to=Time(16,00), period=Minute(30))
    res = DateTime[]
    for date in all_weekdays(date_from, date_to)
        append!(res, [fromMarketTZ(date, t) for t in time_from:period:time_to])
    end
    return res
end

function week_first_ts(ts; time_from=Time(9,30))
    DateUtil.fromMarketTZ(Date(Dates.firstdayofweek(ts)), time_from)
end

function week_last_ts(ts; time=Time(16,0))
    DateUtil.fromMarketTZ(Date(Dates.lastdayofweek(ts) - Day(2)), time)
end

function week_prev_ts(ts; time_from=Time(9,30), time_to=Time(16,0), period=Minute(30))
    if ts <= week_first_ts(ts)
        return week_last_ts(ts - Week(1))
    else
        if toTimeMarket(ts) <= time_from
            # It can't be first day of week or else we would have hit conditional above.
            return fromMarketTZ(Date(ts) - Day(1), time_to)
        else
            return ts - period
        end
    end
end

# function last_close(ts)
#     close_ts = DateUtil.fromMarketTZ(lastTradingDay(ts), time)
# end

# function last_ts(ts=now(UTC); time=Time(16,0))
#     close_ts = DateUtil.fromMarketTZ(lastTradingDay(ts), time)
#     if ts < close_ts
#         prev_close(ts)
#     else
#         return close_ts
#     end
# end

# function get_weeks_tss(last_ts, weeks_count)
#     last_date = Date(last_ts)
#     date_from = Dates.firstdayofweek(last_date) - Week(weeks_count - 1)
#     date_to = Dates.lastdayofweek(last_date)
#     return DateUtil.all_weekday_ts(;date_from, date_to)
# end

const TIMES_PER_DAY = 14 # Dates.value(convert(Minute, Time(16, 0) - Time(9,30))) / 30 + 1, +1 to include endpoint
const DAYS_PER_WEEK = 5
const TIMES_PER_WEEK = TIMES_PER_DAY * DAYS_PER_WEEK

file_ts(ts=now(UTC)) = Dates.format(ts,"yyyymmdd-HHMMSS")

export asof_daily, age_daily
asof_daily()::DateTime = market_midnight(Date(market_now()))
age_daily()::Period = now(UTC) - asof_daily()
age_period()::Period = DEFAULT_TS_PERIOD

const FOREVER2 = Nanosecond(typemax(Int))
const DATETIME_BEFORE = DateTime(0)

# latest_ts(ts) = DateUtil.week_prev_ts(ts; time_from=Time(9,30), time_to=Time(16,0))
const SPY_EX_DATES = Date[]
function next_ex_date(date)
    !isempty(SPY_EX_DATES) || append!(SPY_EX_DATES, calc_ex_dates())
    @assert issorted(SPY_EX_DATES)
    return SPY_EX_DATES[searchsortedfirst(SPY_EX_DATES, date)]
end

calc_ex_dates() =
    filter(x -> (dayofweek(x) == 5) && (dayofweekofmonth(x) == 3) && month(x) in [3,6,9,12],
            DEFAULT_DATA_START_DATE:Day(1):(today() + Year(1)))

# TODO: once these are fixed, rebuild DataOptions
# const BAD_DATA_DATES = [Date(2012,12,17)]
BAD_DATA_DATES() = [
    Date(2012,12,17),
    Date(2018,1,18)
]
# Date(2019,2,2) should be filtered out by isBusDay

function ts_in(range::Interval{Date, Closed, Closed})
    in(Interval{Closed,Open}(market_midnight(first(range)), market_midnight(last(range) + Day(1))))
end

calc_days_to_xpir(ts, xpirts) = DateUtil.market_date.(xpirts) .- DateUtil.market_date.(ts)

# function prev_weekday(date)
#     date = date - Day(1)
#     while !(Dates.dayofweek(date) in 1:5)
#         date = date - Day(1)
#     end
#     return date
# end

end