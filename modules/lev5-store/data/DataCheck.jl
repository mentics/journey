module DataCheck
using Dates
using DateUtil
import DataConst:DATE_START

export combine_dfs, check_ts

function combine_dfs(dfs...; keycol=:ts)
    df = vcat(dfs...)
    sort!(df, [keycol])
    unique!(df, [keycol])
    @assert size(df, 1) > 10000
    @assert issorted(df, keycol)
    @assert allunique(df[!,keycol])
    return df
end

# function check_dates(dates; date_to=market_today())
#     dates_all = DateUtil.all_bdays_itr(;
#         date_from=DATE_START, date_to)
#     return symdiff(tss, dates_all)
# end

function missing_allowed()
    # These might be because day before holiday, short day
    # TODO: could maybe use Calendars MktTime to filter these out
    # TODO: are any of these on expiration days? If so, somewhere is probably messed up because of it
    # DataRead.get_xpirts() gives an earlier xpirts time for these dates, so probably ok
    exceptions = ["2020-11-27T19:00:00",
     "2020-11-27T19:30:00",
     "2020-11-27T20:00:00",
     "2020-12-24T19:00:00",
     "2020-12-24T19:30:00",
     "2020-12-24T20:00:00"]
     return DateTime.(exceptions)
end

end