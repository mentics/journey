module DataPrices
using Dates, DataFrames
using BaseTypes
using ThetaData, Paths, FilesArrow, Caches
import DateUtil

const DATE_START = ThetaData.EARLIEST_OPTIONS_DATE

#region Standard Api
function get_prices(;sym="SPY", age=DateUtil.age_daily())
    return cache!(DataFrame, Symbol("prices-$(sym)"), age) do
        load_prices(;sym, age)
    end
end

function make_prices(;sym="SPY", save_override=false)
    # get from DataFiles and exceptions
    # query under for since then
    # merge
    df1 = Paths.load_data(path_ts_optionsdx(;sym), DataFrame)
    select!(df1, :ts => remove_10s => :ts, :under => tof32 => :price)
    filter!(:ts => keep_ts_optionsdx, df1)

    df2 = ThetaData.query_prices(;sym)

    missing_ts_prices = optionsdx_missing_ts_prices()
    tss = first.(missing_ts_prices)
    prices = Float32.(last.(missing_ts_prices))
    df3 = DataFrame([tss, prices], [:ts, :price])

    df = combnie_dfs(df1, df2, df3)

    # @assert maximum(df.ts) == DateUtil.lastTradingDate(now(UTC))
    diff = check_ts(df.ts; ts_to=DateUtil.market_midnight(DateUtil.market_today()))
    if !isempty(diff)
        println("ERROR: not all ts found. Not saved.")
        if save_override
            println("Saving anyway")
            save_data(file_prices(;sym), df)
        end
        return diff
    end

    save_data(file_prices(;sym), df)
    return df
end

function update_prices(;sym="SPY")
    # TODO: deal with that thetadata is 15 minute delayed
    df1 = load_prices(;sym, age=DateUtil.FOREVER2)
    ts_last = df1.ts[end]
    if now(UTC) - ts_last < Minute(30)
        println("Already up to date: $(ts_last)")
        return
    end
    df2 = ThetaData.query_prices(
            DateUtil.market_date(ts_last),
            DateUtil.market_today()
            ;sym, age=Minute(15))
    df = combine_dfs(df1, df2)
    diff = check_ts(df.ts)
    if !isempty(diff)
        println("ERROR: not all ts found. Not saved.")
        return diff
    end
    save_data(file_prices(;sym), df)
    return
end
#endregion Standard Api

#region Local
path_ts_optionsdx(;sym) = joinpath(Paths.db("market", "incoming", "optionsdx", sym), "ts.arrow")
file_prices(;sym="SPY") = joinpath(db_incoming("prices"; sym), "prices-$(sym).arrow")

load_prices(;sym, age)::DataFrame = load_data(file_prices(;sym), DataFrame; age)

function combine_dfs(dfs...)
    df = vcat(dfs...)
    sort!(df, [:ts])
    unique!(df, [:ts])
    @assert size(df, 1) > 10000
    @assert issorted(df, :ts)
    @assert allunique(df.ts)
    return df
end

remove_10s(tss) = map(tss) do ts
    second(ts) == 0 ? ts : ts - Second(10)
end
tof32(p) = Float32.(p ./ 1000)
function keep_ts_optionsdx(ts)
    return DateUtil.isBusDay(ts) &&
        ts >= DATE_START &&
        ts < ThetaData.EARLIEST_SPY_DATE &&
        (minute(ts) == 0 || minute(ts) == 30)
end

function check_ts(tss; ts_to=now(UTC))
    tss_all = DateUtil.all_bdays_ts(;
        date_from=DATE_START, ts_to,
        time_from=Time(9,30), time_to=Time(16,0))
    # @assert isempty(symdiff(tss, tss_expected))
    return symdiff(tss, tss_all)
end

# latest_ts(ts) = DateUtil.week_prev_ts(ts; time_from=Time(9,30), time_to=Time(16,0))
#endregion Local

#region Fixes
#=
Missing times I ran into:
2012-10-24 after 2
under missing for 2013-06-13T14:00:00
under missing for 2014-01-02T20:00:00
under missing for 2014-03-17T15:30:00
under missing for 2014-08-04T14:00:00
under missing for 2015-12-22T18:30:00
under missing for 2016-03-31T18:00:00
under missing for 2016-04-18T14:00:00
Downloaded these from barchart.com
=#
optionsdx_missing_ts_prices() = [
    # DateTime("2012-10-24T13:30:10") => C(141.93),
    # DateTime("2012-10-24T14:00:00") => C(141.74),
    DateTime("2012-10-24T14:30:00") => C(141.48),
    DateTime("2012-10-24T15:00:00") => C(141.67),
    DateTime("2012-10-24T15:30:00") => C(141.59),
    DateTime("2012-10-24T16:00:00") => C(141.3),
    DateTime("2012-10-24T16:30:00") => C(141.43),
    DateTime("2012-10-24T17:00:00") => C(141.66),
    DateTime("2012-10-24T17:30:00") => C(141.6),
    DateTime("2012-10-24T18:00:00") => C(141.55),
    DateTime("2012-10-24T18:30:00") => C(141.66),
    DateTime("2012-10-24T19:00:00") => C(141.27),
    DateTime("2012-10-24T19:30:00") => C(141.11),
    DateTime("2012-10-24T20:00:00") => C(141.01),
    ##
    DateTime("2013-06-13T14:00:00") => C(162.01),
    ##
    DateTime("2014-01-02T20:00:00") => C(182.76),
    DateTime("2014-01-02T20:30:00") => C(182.99),
    DateTime("2014-01-02T21:00:00") => C(182.95),
    ##
    DateTime("2014-03-17T15:30:00") => C(185.97),
    ##
    DateTime("2014-08-04T13:30:00") => C(192.87),
    DateTime("2014-08-04T14:00:00") => C(192.96),
    DateTime("2014-08-04T14:30:00") => C(192.8),
    DateTime("2014-08-04T15:00:00") => C(192.15),
    DateTime("2014-08-04T15:30:00") => C(192.71),
    DateTime("2014-08-04T16:00:00") => C(192.62),
    DateTime("2014-08-04T16:30:00") => C(192.96),
    DateTime("2014-08-04T17:00:00") => C(192.95),
    DateTime("2014-08-04T17:30:00") => C(192.96),
    DateTime("2014-08-04T18:00:00") => C(193.29),
    DateTime("2014-08-04T18:30:00") => C(193.31),
    DateTime("2014-08-04T19:00:00") => C(193.81),
    DateTime("2014-08-04T19:30:00") => C(194.0),
    DateTime("2014-08-04T20:00:00") => C(193.88),
    ##
    DateTime("2015-12-22T18:30:00") => C(202.88),
    ##
    DateTime("2016-03-31T18:00:00") => C(205.82),
    ##
    DateTime("2016-04-18T13:30:00") => C(207.14),
    DateTime("2016-04-18T14:00:00") => C(207.805),
    DateTime("2016-04-18T14:30:00") => C(208.54),
    DateTime("2016-04-18T15:00:00") => C(208.65),
    DateTime("2016-04-18T15:30:00") => C(208.69),
    DateTime("2016-04-18T16:00:00") => C(208.71),
    DateTime("2016-04-18T16:30:00") => C(208.9),
    DateTime("2016-04-18T17:00:00") => C(208.92),
    DateTime("2016-04-18T17:30:00") => C(208.82),
    DateTime("2016-04-18T18:00:00") => C(208.95),
    DateTime("2016-04-18T18:30:00") => C(208.94),
    DateTime("2016-04-18T19:00:00") => C(209.1),
    DateTime("2016-04-18T19:30:00") => C(209.04),
    DateTime("2016-04-18T20:00:00") => C(209.28),
]

# 2015-01-30T14:30:00
# 01/30/2015
function missing_days()
    df = get_prices()
    unique(Date.(check_ts(df.ts; ts_to=DateUtil.market_midnight(DateUtil.market_today()))))
end
#endregion Fixes

end