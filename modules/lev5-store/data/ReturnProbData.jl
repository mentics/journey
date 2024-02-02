module ReturnProbData
using Dates, Intervals, DataFrames, SearchSortedNearest
import Flux
import DateUtil, Paths
import DataConst, DataRead, ModelUtil
import OhlcShapeData as osd
import Calendars as cal
using ProbMeta, MLyze
import DataFrameUtil as DF

const NAME = replace(string(@__MODULE__), "Data" => "")

#region Public
params_data() = (;
    xpirs_within = DataConst.XPIRS_WITHIN_CALC2,
    # bins_count = Bins.num_edges?,
    # skip_cols = 4, # skip ts, expir, y_bin, ce_compare for actual input
    skip_cols = 3, # skip ts, expir, y_bin
)

function make_input(params=params_data(); age=DateUtil.FOREVER2) # DateUtil.age_daily())
    # Get xtqs and ret
    df_tsx = filtered_tsx(;age)
    df_tsx.date = Date.(df_tsx.ts)

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
    treasury_lookup = DataRead.get_treasury_lookup(;age)
    transform!(df_tsx, [:ts] => (tss -> [treasury_lookup(DateUtil.market_date(ts)) for ts in tss]) => :treasury_rate)

    # Add hist
    df_hist, params_hist = Paths.load_data_params(Paths.db_output(osd.NAME), DataFrame)
    # rename!(df_hist, :key => :ts)
    df_hist_date = DataFrame(:date => df_hist.date)
    df_hist_enc = DataFrame([[df_hist.output[row][i] for row in eachindex(df_hist.output)] for i in eachindex(df_hist.output[1])], :auto)
    df_hist = hcat(df_hist_date, df_hist_enc)
    params = merge(params, (;hist=params_hist))
    df = leftjoin(df_tsx, df_hist; on=:date)
    sort!(df, [:ts, :expir])

    date_range = df_hist.date[1]..last(params_hist.data.date_range)
    filter!(:date => (date -> date in date_range), df)
    # expected_dates = DateUtil.all_bdays(df_hist.date[1], last(date_range))
    # println(symdiff(expected_dates, unique(df.date)))
    # @assert expected_dates == unique(df.date)
    disallowmissing!(df)

    # Bin the returns
    transform!(df, :ret => (r -> Bins.nearest.(r .+ 1)) => :y_bin)

    # Calc pmf and cross entropy by days to xpir for comparison
    transform!(df, [:ts, :expir] => DateUtil.calc_days_to_xpir => :days_to_xpir)
    # pmfk_lookup = load_pmfk_lookup()
    # bins = 1:params.bins_count
    # transform!(df, [:y_bin,:days_to_xpir] => (
    #         (y_bins, days_to_xpirs) ->
    #             [Flux.crossentropy(pmfk_lookup[days_to_xpirs[i]], Flux.onehot(y_bins[i], bins)) for i in eachindex(y_bins)]
    #     ) => :ce_compare)

    # cleanup
    # select!(df, :ts, :expir, :y_bin, :ce_compare, Not([:ts, :date, :expir, :y_bin, :ce_compare, :ret, :days_to_xpir]))
    select!(df, :ts, :expir, :y_bin, Not([:ts, :date, :expir, :y_bin, :ret, :days_to_xpir]))

    @assert issorted(df, [:ts, :expir])
    @assert allunique(df, [:ts, :expir])
    # TODO: fix this to ignore the skip_back difference
    # @assert size(df, 1) == size(df_tsx, 1) "size(df, 1) $(size(df, 1)) == $(size(df_tsx, 1)) size(df_tsx, 1)"
    # TODO: assert same size for df and df_hist

    Paths.save_data_params(Paths.db_input(NAME), params, df; update=true)
    return df
end
#endregion Public

#region Pmfk
# function setup_ce_all()
#     # setup_y()
#     ky = make_y_pmfk()
#     return [Flux.crossentropy(ky, Flux.onehot(y, eachindex(ky))) for y in eachindex(ky)]
#     # return [Flux.crossentropy(ky, get_y(y) |> cpu) for y in 1:nbins]
# end

function make_pmfk_lookup()
    df = filtered_tsx()
    _, params_hist = Paths.load_data_params(Paths.db_output(osd.NAME), DataFrame)
    range = params_hist.data.date_range
    df_train = DF.in(df, range)
    @assert df_train.ts[end] < (now(UTC) - Month(3))
    # return MLyze.calc_pmf_kde(df_train.y_bin; nbins)

    transform!(df, [:ts, :expir] => ((ts, xpirts) -> DateUtil.market_date.(xpirts) .- DateUtil.market_date.(ts)) => :days_to_xpir)
    transform!(df, :ret => (r -> Bins.nearest.(r .+ 1)) => :y_bin)

    gdf = groupby(df, :days_to_xpir)
    df3 = combine(gdf, [:y_bin] => (ybins -> [MLyze.calc_pmf_kde(ybins)]) => :pmfk)
    return Dict(zip(df3.days_to_xpir, df3.pmfk))
end

# TODO: age?
load_pmfk_lookup() = Paths.load_data(file_pmfk_lookup(), "pmfk_lookup")
save_pmfk_lookup() = Paths.save_data(file_pmfk_lookup(); pmfk_lookup=make_pmfk_lookup())
file_pmfk_lookup() = Paths.db_incoming("pmfk_lookup", "pfmk.jld2")

# # TODO: age?
# load_ce_all() = Paths.load_data(Paths.db_output("ce_all"), "ce_all")
# save_ce_all() = Paths.save_data(Paths.db_output("ce_all"); ce_all=setup_ce_all())
#endregion Pmfk

#region Util
function filtered_tsx(;age=DateUtil.FOREVER2) # DateUtil.age_daily())
    # Near the end, we don't have returns because they are in the future so filter them out.
    # We can't even backtest on this because we won't know the performance of it anyway
    df1 = filter(:ret => isfinite, DataRead.get_tsx(;age))
    df2 = filter([:ts,:expir] => ((ts, xpirts) -> DateUtil.calc_days_to_xpir(ts, xpirts) >= Day(1)), df1)
    return df2
end


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
