module ReturnProbData
using Dates, DataFrames, SearchSortedNearest
import DateUtil, Paths
import DataRead, ModelUtil
import HistShapeData
import Calendars as cal

const NAME = replace(string(@__MODULE__), "Data" => "")

#region Public
params_data() = (;
    weeks_count = 5,
    intraday_period = Minute(30),
    xpirs_within = Day(40),
    bins_count = 200,
    ret_min = -0.15,
    ret_max = 0.15,
)

function make_input(params=params_data())
    # Add xtq
    # TODO: fix age when data is loaded ready
    df = DataRead.get_tsx(; age=DateUtil.FOREVER2)

    # Add duration and temporal
    transform!(df, [:ts, :expir] => make_dur(params.xpirs_within) => [:closed, :pre, :open, :post, :weekend, :holiday])
    transform!(df, [:ts] => (ts -> broadcast.(Float32, ModelUtil.to_temporal.(ts))) => [:week_day, :month_day, :quarter_day, :year_day, :hour])

    # Add hist
    df_hist, params_hist = Paths.load_data_params(Paths.db_encoded(HistShapeData.NAME), DataFrame)
    @show (df.ts == df_hist.key)

    # Bin the returns
    bins = make_bins(params)
    transform!(df, :ret => (r -> searchsortednearest.(Ref(bins), r)) => :y_bin)
    return df

    # cleanup
    select!(df, :y_bin, Not(:y_bin, :ts, :expiration, :ret, :tex, :extrin, :extrindt, :iv_mean, :logret))

    # sort!(df, :ts) TODO
    @assert issorted(df.ts)
    @assert allunique(df.ts)
    @assert size(df, 1) == size(df_hist, 1)

    # save_data(db_input(NAME), params, df)
    return df
end
#endregion Public

#region Util
function make_bins(params)
    (;bins_count, ret_min, ret_max) = params
    ModelUtil.make_bins(bins_count, ret_min, ret_max)
end

make_dur(max_days) = (ts, xpir) -> dur_to_input.(cal.calcDur.(ts, xpir), max_days)
dur_to_input(dur, max_days) = Float32.(([getfield(dur, nam) for nam in propertynames(dur)]) ./ max_days)
#endregion Util

end
