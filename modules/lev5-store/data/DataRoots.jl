module DataRoots
using Dates, DataFrames
using ThetaData, Paths, FilesArrow
import DateUtil

#region Standard Api
function get_root_all_days(; sym="SPY", age=age_daily())
    return cache!(Dict{DateTime,Float32}, Symbol("under-$(sym)"), age) do
        load_roots(;sym, age)
    end
end

function make_roots(;sym="SPY")
    # get from DataFiles and exceptions
    # query under for since then
    # merge
    df1 = dat.ts_df()
    df2 = ThetaData.query_roots(;sym)
    save_data(file_under(;sym); under)
end

function update_roots(;sym="SPY")
    df = load_roots(;sym, age=DateUtil.FOREVER2)
end
#endregion Standard Api

#region Local
path_ts() = joinpath(PATHS.db_old(), "market", "spy", "ts.arrow")
ts_table() = Arrow.Table(path_ts())

load_roots(;sym, age)::DataFrame = load_data(file_under(;sym), DataFrame; age)

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