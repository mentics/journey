module ReturnProbData
using Dates, Intervals, DataFrames, SearchSortedNearest
import DateUtil, Paths
import DataConst, DataRead, ModelUtil
import HistShapeData as hsd
import Calendars as cal
using ProbMeta

const NAME = replace(string(@__MODULE__), "Data" => "")

#region Public
params_data() = (;
    xpirs_within = DataConst.XPIRS_WITHIN_CALC2,
    bins_count = Bins.VNUM,
)

function make_input(params=params_data(); age=DateUtil.age_daily())
    # Get xtqs and ret
    df_tsx = DataRead.get_tsx(;age)
    # Near the end, we don't have returns because they are in the future so filter them out.
    # We can't even backtest on this because we won't know the performance of it anyway
    df_tsx = filter(:ret => isfinite, df_tsx)

    # Add temporal for ts
    transform!(df_tsx, [:ts] => (ts -> ModelUtil.to_temporal_ts.(ts)) => prefix_sym.([:week_day, :month_day, :quarter_day, :year_day, :hour], :ts_))
    # temporal for expiration
    transform!(df_tsx, [:expir] => (xpir -> ModelUtil.to_temporal_date.(Date.(xpir))) => prefix_sym.([:week_day, :month_day, :quarter_day, :year_day], :xpir_))
    # duration to expiration
    transform!(df_tsx, [:ts, :expir] => make_dur(params.xpirs_within) => prefix_sym.([:closed, :pre, :open, :post, :weekend, :holiday], :xpir_dur_))
    # duration to dividend
    transform!(df_tsx, [:ts] => (ts -> dur_to_div.(ts)) => prefix_sym.([:closed, :pre, :open, :post, :weekend, :holiday], :div_dur_))
    # Add absolute time cycle encoding
    transform!(df_tsx, [:ts] => (ts -> to_cycles.(ts)) => cycle_syms())
    # Add treasury rate
    treasury_lookup = DataRead.get_treasury_lookup()
    transform!(df_tsx, [:ts] => (tss -> [treasury_lookup(DateUtil.market_date(ts)) for ts in tss]) => :treasury_rate)

    # Add hist
    df_hist, params_hist = Paths.load_data_params(Paths.db_output(hsd.NAME), DataFrame)
    # rename!(df_hist, :key => :ts)
    df_hist_ts = DataFrame(:ts => df_hist.ts)
    df_hist_enc = DataFrame([[df_hist.output[row][col] for row in eachindex(df_hist.output)] for col in eachindex(df_hist.output[1])], :auto)
    df_hist = hcat(df_hist_ts, df_hist_enc)
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

    Paths.save_data_params(Paths.db_input(NAME), params, df; update=true)
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

const CYCLE_START_DATE = Date(2000,1,1)
to_cycles(ts) = cycles(CYCLE_START_DATE, Date(ts))
import OutputUtil
cycle_syms() = [Symbol(string("cycle", replace(string(OutputUtil.rd2(c)), '.' => '_'))) for c in cycle_lengths()]

cycle_lengths() = [1/12, 1/4, 1/3, 1/2, 1, 2, 5, 10, 100]
function cycles(start_date, end_date)
    dur = diff_in_years(start_date, end_date)
    return [abs(sin(π * dur / cycle)) for cycle in cycle_lengths()]
end

function diff_in_years(start_date, end_date)
    @assert end_date >= start_date
    years_whole = year(end_date) - year(start_date)
    diy_start = daysinyear(start_date)
    diy_end = daysinyear(end_date)
    years_start = (diy_start - dayofyear(start_date) + 1) / diy_start
    years_end = (diy_end - dayofyear(end_date)) / diy_end
    return years_whole + years_start - years_end
end
#endregion Util

end
