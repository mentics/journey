module DataCheck
using Dates
using DateUtil
import DataConst:DATE_START

function combine_dfs(dfs...; keycols=[:ts])
    df = vcat(dfs...)
    sort!(df, keycols)
    unique!(df, keycols)
    # @assert size(df, 1) > 10000
    @assert issorted(df, keycols)
    @assert allunique(df[!,keycols])
    return df
end

# function check_dates(dates; date_to=market_today())
#     dates_all = DateUtil.all_bdays_itr(;
#         date_from=DATE_START, date_to)
#     return symdiff(tss, dates_all)
# end

# Should be handled by <= cal.getMarketClose() in DataTs
# function missing_allowed()
#     # These might be because day before holiday, short day
#     # TODO: could maybe use Calendars MktTime to filter these out
#     # TODO: are any of these on expiration days? If so, somewhere is probably messed up because of it
#     # DataRead.get_xpirts() gives an earlier xpirts time for these dates, so probably ok
#     exceptions = ["2020-11-27T19:00:00",
#      "2020-11-27T19:30:00",
#      "2020-11-27T20:00:00",
#      "2020-12-24T19:00:00",
#      "2020-12-24T19:30:00",
#      "2020-12-24T20:00:00"]
#      return DateTime.(exceptions)
# end

end