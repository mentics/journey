module ReturnProbData
using Dates, Intervals, DataFrames, SearchSortedNearest
import DateUtil, Paths
import DataConst, DataRead, ModelUtil
import HistShapeData
import Calendars as cal
using ProbMeta

const NAME = replace(string(@__MODULE__), "Data" => "")

#region Public
params_data() = (;
    # weeks_count = 5,
    # intraday_period = Minute(30),
    xpirs_within = DataConst.XPIRS_WITHIN,
    bins_count = Bins.VNUM,
    train_date_range = DateUtil.DEFAULT_DATA_START_DATE..Date(2023,6,30),
    # ProbMeta.BIN_PARAMS...
)

function make_input(params=params_data())
    # Get xtqs and ret
    df_tsx = DataRead.get_tsx()
    filter(:ts => DateUtil.ts_in(params.train_date_range), df_tsx)
    # Near the end, we don't have returns because they are in the future so filter them out.
    df_tsx = filter(:ret => isfinite, df_tsx)

    # Add temporal for ts
    transform!(df_tsx, [:ts] => (ts -> ModelUtil.to_temporal_ts.(ts)) => prefix_sym.([:week_day, :month_day, :quarter_day, :year_day, :hour], :ts_))
    # temporal for expiration
    transform!(df_tsx, [:expir] => (xpir -> ModelUtil.to_temporal_date.(Date.(xpir))) => prefix_sym.([:week_day, :month_day, :quarter_day, :year_day], :xpir_))
    # duration to expiration
    transform!(df_tsx, [:ts, :expir] => make_dur(params.xpirs_within) => prefix_sym.([:closed, :pre, :open, :post, :weekend, :holiday], :xpir_dur_))
    # duration to dividend
    transform!(df_tsx, [:ts] => (ts -> dur_to_div.(ts)) => prefix_sym.([:closed, :pre, :open, :post, :weekend, :holiday], :div_dur_))

    # Add hist
    df_hist, params_hist = Paths.load_data_params(Paths.db_output(HistShapeData.NAME), DataFrame)
    rename!(df_hist, :key => :ts)
    params = merge(params, (;hist=params_hist))
    df = innerjoin(df_tsx, df_hist; on=:ts)

    # TODO: should match eventually. so change to assert when ready
    # except for skip_back and near current time where ret is in the future
    # @show (df.ts == df_hist.ts)

    # Bin the returns
    transform!(df, :ret => (r -> Bins.nearest.(r .+ 1)) => :y_bin)

    # cleanup
    select!(df, :ts, :expir, :y_bin, Not([:ts, :expir, :y_bin, :ret]))

    @assert issorted(df, [:ts, :expir])
    @assert allunique(df, [:ts, :expir])
    # TODO: fix this to ignore the skip_back difference
    # @assert size(df, 1) == size(df_tsx, 1) "size(df, 1) $(size(df, 1)) == $(size(df_tsx, 1)) size(df_tsx, 1)"
    # TODO: assert same size for df and df_hist

    Paths.save_data_params(Paths.db_input(NAME), params, df)
    return df
end
#endregion Public

#region Util
make_dur(max_days) = (ts, xpir) -> dur_to_input.(cal.calcDur.(ts, xpir), max_days)
dur_to_input(dur, max_days) = Float32.(([getfield(dur, nam) for nam in propertynames(dur)]) ./ max_days)

function dur_to_div(ts)
    # divs are every quarter, so 3 months of days for max days
    dur_to_input(cal.calcDur(ts, DateUtil.next_ex_date(ts)), Day(3 * 31))
end

prefix_sym(sym, pre) = Symbol(string(pre) * string(sym))
#endregion Util

end
