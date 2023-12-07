module ReturnProbData
import HistShapeData

const NAME = replace(string(@__MODULE__), "Data" => "")

#region Public
params_data() = (;
    weeks_count = 5,
    intraday_period = Minute(30),
)

function make_input(params=params_data())
    df_hist, params_hist = load_data_params(Paths.db_encoded(HistShapeData.NAME), DataFrame)

    bins = bins()

    sort!(df, :ts)
    @assert issorted(df.ts)
    @assert allunique(df.ts)
    @assert size(df, 1) == size(df_hist, 1)

    save_data(db_input(NAME), params, df)
    return df
end

function make_data_input()
    bins = bins()

    # Restrict to ts's for which we have encoded hist data
    hist = hm.load_data_encoded()
    hist_valid_ts = Set(hist.ts)

    df = dat.tsx_df()
    filter!(:ts => (ts -> ts in hist_valid_ts), df)
    dropmissing!(df, [:ret, :extrin])
    len_before = length(df[!,1])

    # TODO: consider including extrin directly in the future
    transform!(df, [:extrin, :tex] => ((xtrin, tex) -> xtrin ./ sqrt.(tex)) => :vol)
    # transform!(df, :ret => (r -> Float32.(r .- 1.0)) => :ret)
    transform!(df, :ret => (r -> searchsortednearest.(Ref(bins), r .- 1)) => :y_bin)

    # Add temporal columns
    transform!(df, [:ts, :expiration] => ((ts, xpir) -> dur_to_input.(cal.calcDur.(ts, xpir))) => [:closed, :pre, :open, :post, :weekend, :holiday])
    transform!(df, [:ts] => (ts -> broadcast.(Float32, ModelUtil.to_temporal.(ts))) => [:week_day, :month_day, :quarter_day, :year_day, :hour])

    # Add historical columns
    dfhist = DataFrame(hist)
    leftjoin!(df, dfhist; on=:ts)

    select!(df, :y_bin, Not(:y_bin, :ts, :expiration, :ret, :tex, :extrin, :extrindt, :iv_mean, :logret))
    dropmissing!(df)

    @assert len_before == length(df[!,1]) "len_before == length(df[!,1]): $(len_before) == $(length(df[!,1]))"
    dat.save(COMBINED_INPUT_PATH, df)
    return df
end

#endregion Public

#region Util
function bins()
    (;bins_count, ret_min, ret_max) = config()
    ModelUtil.make_bins(bins_count, ret_min, ret_max)
end
#endregion Util


end
