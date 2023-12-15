module DataTs
using Dates
using Paths, FilesJLD2
import DateUtil, DataCheck, DataRead
import Calendars as cal

#=
Data modules pattern:
  get_*: data both from files and query recent if necessary
  make_*: save historical data to files
  update_*: update files with recent historical data
=#

#region Standard Api
function make_ts(;sym="SPY")
    tss = all_bdays_ts()
    filter_expected_missing!(tss)
    Paths.save_data(DataRead.file_ts(;sym); tss)
    return tss
end

function update_ts(;sym="SPY")
    # -Minute(15) is to deal with ThetaData's 15 minute delayed quotes
    tss = all_bdays_ts(; ts_to=(now(UTC) - Minute(15)))
    filter_expected_missing!(tss)
    ts_last = DataRead.get_ts(;age=DateUtil.FOREVER2)[end]
    if tss[end] > ts_last
        Paths.save_data(DataRead.file_ts(;sym); tss)
    else
        println("DataTs already up to date $(ts_last)")
        touch(DataRead.file_ts(;sym))
    end
end
#endregion Standard Api

#region Local
const DEFAULT_MARKET_START_TIME = Time(9, 30)
const DEFAULT_MARKET_CLOSE_TIME = Time(16,0)

function all_bdays_itr(;date_from=Date(2012,6,1), date_to=market_today())
    return Iterators.filter(d -> Dates.dayofweek(d) <= 5 && DateUtil.isBusDay(d), date_from:Day(1):date_to)
end

# bad dates/ts are filtered out in DataTs
function all_bdays_ts(;date_from=DateUtil.DEFAULT_DATA_START_DATE, ts_to=now(UTC), time_from=DEFAULT_MARKET_START_TIME, time_to=DEFAULT_MARKET_CLOSE_TIME, period=DateUtil.DEFAULT_TS_PERIOD)
    # TODO: consider using Calendars here so don't have to filter for cal.getMarketClose afterwards?
    res = DateTime[]
    zts_to = DateUtil.toMarketTZ(ts_to)
    date_to = DateUtil.market_date(zts_to)
    for date in all_bdays_itr(;date_from, date_to=(date_to - Day(1)))
        append!(res, [DateUtil.fromMarketTZ(date, t) for t in time_from:period:time_to])
    end
    if (DateUtil.isBusDay(ts_to))
        append!(res, filter!(ts -> ts < ts_to, [DateUtil.fromMarketTZ(date_to, t) for t in time_from:period:time_to]))
    end
    return res
end

function filter_expected_missing!(tss)
    filter!(tss) do ts
        date = Date(ts)
        # ThetaData has bad data for a date (at least one found so far)
        # and some day-before-holidays close early
        return !(date in DateUtil.BAD_DATA_DATES()) &&
               ts <= cal.getMarketClose(date)
    end
end
#endregion Local

end