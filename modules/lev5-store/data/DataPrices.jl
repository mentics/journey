module DataPrices
using Dates, DataFrames
using ThetaData, Paths, FilesArrow
import DateUtil

#region Standard Api
function get_prices_all_days(;sym="SPY", age=age_daily())
    return cache!(Dict{DateTime,Float32}, Symbol("under-$(sym)"), age) do
        load_prices(;sym, age)
    end
end

function make_prices(;sym="SPY")
    # get from DataFiles and exceptions
    # query under for since then
    # merge
    df1 = Paths.load_data(path_ts_optionsdx(;sym), DataFrame)
    select!(df1, :ts => remove_10s => :ts, :under => tof32 => :price)
    filter!(:ts => (ts -> DateUtil.isBusDay(ts) && ts < ThetaData.EARLIEST_SPY_DATE), df1)

    df2 = ThetaData.query_prices(;sym)
    df = vcat(df1, df2)
    @assert issorted(df, :ts)
    @assert allunique(df.ts)
    # @assert maximum(df.ts) == DateUtil.lastTradingDate(now(UTC))
    # save_data(file_prices(;sym); under)
    return df
end

function update_prices(;sym="SPY")
    df = load_prices(;sym, age=DateUtil.FOREVER2)

end

remove_10s(tss) = map(tss) do ts
    second(ts) == 0 ? ts : ts - Second(10)
end
tof32(p) = Float32.(p ./ 1000)
#endregion Standard Api

#region Local
path_ts_optionsdx(;sym) = joinpath(Paths.db("market", "incoming", "optionsdx", sym), "ts.arrow")
file_prices(;sym="SPY") = joinpath(db_incoming("prices"; sym), "prices-$(sym).jld2")

load_prices(;sym, age)::DataFrame = load_data(file_prices(;sym), DataFrame; age)

import DataFiles as dat
function check_ts()
    df = dat.ts_df()
    ts1 = filter(df.ts) do ts
        minute(ts) == 0 || minute(ts) == 30
    end
    ts1 = map(ts1) do ts
        second(ts) == 0 ? ts : ts - Second(10)
    end
    ts2 = filter(DateUtil.all_weekday_ts(;time_from=Time(9,30), time_to=Time(16,0))) do ts
        DateUtil.isbd(Date(ts))
    end
    return ts1, ts2
end
#endregion Local

end