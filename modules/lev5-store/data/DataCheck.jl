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
    @assert allunique(df[keycol])
    return df
end

function check_ts(tss; ts_to=now(UTC))
    tss_all = DateUtil.all_bdays_ts(;
        date_from=DATE_START, ts_to,
        time_from=Time(9,30), time_to=Time(16,0))
    # @assert isempty(symdiff(tss, tss_expected))

    diff = symdiff(tss, tss_all)
    exceptions = missing_allowed()
    return filter!(diff) do d
        !(d in exceptions)
    end
end

function check_dates(dates; date_to=market_today())
    dates_all = DateUtil.all_bdays_itr(;
        date_from=DATE_START, date_to)
    return symdiff(tss, dates_all)
end

function missing_allowed()
    # These might be because day before holiday, short day
    # TODO: could maybe use Calendars MktTime to filter these out
    exceptions = ["2020-11-27T19:00:00",
     "2020-11-27T19:30:00",
     "2020-11-27T20:00:00",
     "2020-12-24T19:00:00",
     "2020-12-24T19:30:00",
     "2020-12-24T20:00:00"]
     return DateTime.(exceptions)
end

end