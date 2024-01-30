module OhlcShapeData
using Intervals, Dates, DataFrames, StatsBase
using DateUtil, DataConst, Paths, FilesArrow
# , DataRead
import VectorCalcUtil as vcu
import HistData

const OHLC_COLS = (:open, :high, :low, :close)

const NAME = replace(string(@__MODULE__), "Data" => "")

# Can't use 0 because if we don't filter until later, there is a time when 0 becomes valid.
# but we do need it to be 0 later for the input, so just make sure to calc mask before we get to the 0-is-valid point.
const MISSING_FLOAT2 = 0f0

#region Public
params_data() = (;
    weeks_count = 5,
    train_date_range = DateUtil.DEFAULT_DATA_START_DATE..Date(2023,12,31),
)

#=
TODO: What's the typical file format for other frameworks for their input data?

make_input acquires all the data it needs from wherever, creates a single dataframe representing the input to the model,
and saves that in the input area.

Format for input files is an Arrow table with columns:
    obs key: unique identifier representing the observation (eg. :ts for timestamp)
    x: the datastructure for the observation that will be passed into the model for training and inference.
       Typically a vector of predictors, or a tuple of (predictors, meta, mask)
    y: the datastructure that is the expected output of the model for this observation
       Typically a vector of outputs, or a single value
=#

#=
For this one:
date, prices_seq, prices_mask, prices_scaling, vix_seq, vix_mask, vix_scaling
=#
function make_input(params=params_data())
    global kprice = prices = make_df_seq("SPY", (OHLC_COLS..., :volume), params)
    # global kprice = prices = make_df_seq("SPY", OHLC_COLS, params)
    global kvix = vix = make_df_seq("VIX", OHLC_COLS, params)
    global kdf = df = leftjoin(prices, vix; on=:date, renamecols=f_prefix("price_") => f_prefix("vix_"))
    disallowmissing!(df)
    scaling_width = length(df.vix_scaling[1]) + length(df.price_scaling[1])
    transform!(df, [:price_scaling, :vix_scaling] => ((ps, vs) -> vcat.(ps, vs)) => :scaling)
    select!(df, Not([:price_scaling, :vix_scaling]))
    @assert length(df.scaling[1]) == scaling_width
    sort!(df, :date)
    params = merge(params, (;widths=(;
        price_seq=length(df.price_seq[1]),
        vix_seq=length(df.vix_seq[1]),
        scaling=scaling_width,
    )))
    Paths.save_data_params(Paths.db_input(NAME), params, df)
    return df
end
f_prefix(s) = name -> s * name
#endregion Public

#=
vix.date is the last date included in the sequence.
So, for a given date, you should use prev_weekday(date) to avoid using future data.
=#

#region make data
const CACHE = Dict{String,DataFrame}()
function make_df_seq(sym, cols, params=params_data())
    println("make seq $(sym)")
    # get!(CACHE, sym) do
        cols_per_entry = length(cols)
        weeks_count = params.weeks_count
        entry_count = DateUtil.DAYS_PER_WEEK * weeks_count
        seq_len = cols_per_entry * entry_count
        # scaling_len = (length(cols) - 3) * 32

        df = make_input_raw(sym, cols)
        first_ind = find_first_ind_date(df, entry_count)
        inds = [ind for ind in first_ind:length(df.date) if (!iszero(df[ind,2]) && !iszero(df[ind-1,end]))]
        rows = mapreduce(vcat, inds) do ind
            seq, scales = make_sequence_date(df, ind, weeks_count, cols)
            us = unique(scales)
            if any(isnan, seq)
                global knan = (;sym, ind, seq, scales, us)
                error("Found nan: $(sym) $(ind)")
            end
            return (seq, encode_scales(us))
        end
        sequences = [row[1] for row in rows]
        scaling = [row[2] for row in rows]
        masks = [(!iszero).(seq) for seq in sequences]
        @assert typeof(masks[1]) == BitVector
        @assert length(masks[1]) == seq_len "length(masks[1]) $(length(masks[1])) == $(seq_len) seq_len"
        df_seq = DataFrame(
            :date => df.date[inds],
            # (col => df[!,col][inds] for col in cols)...,
            # :open => df.open[inds],
            # :high => df.high[inds],
            # :low => df.low[inds],
            # :close => df.close[inds],
            # :volume => df.volume[inds],
            :mask => masks,
            :seq => sequences,
            :scaling => scaling,
        )
        @assert length(df_seq.mask[1]) == seq_len "length(df_seq.mask[1]) $(length(df_seq.mask[1])) == $(seq_len) seq_len"
        # global kf = filter(collect(cols) => ((cs...) -> all(!iszero, cs)), df_seq)
        # @assert df_seq.seq[1][findlast(!iszero, df_seq.seq[1])] == df_seq[!,cols[end]][1]
        # @assert df_seq.seq[end][findlast(!iszero, df_seq.seq[end])] == df_seq[!,cols[end]][end]

        println("$(sym) working with $(size(df,1)) rows")
        return df_seq
    # end
end

function make_input_raw(sym, cols)
    df_daily = select!(DataFrame(HistData.dataDaily(sym)), :date, cols...)
    if :volume in cols
        df_daily.volume = [Float32(iszero(x) ? x : log(x)) for x in df_daily.volume]
        # replace!(log, df_daily.volume)
    end
    if sym == "VIX"
        for col in cols
            replace!(x -> iszero(x) ? x : log(x), df_daily[!,col])
        end
    end
    df_dates_all = DataFrame(:date => collect(DateUtil.all_weekdays()))
    df = leftjoin(df_dates_all, df_daily; on=:date)
    transform!(df, tf_missing_cols(cols)...)
    # df[!, :volume] = Int32.(df.volume)
    sort!(df, :date)
    @assert issorted(df.date)
    @assert allunique(df.date)
    for ind in axes(df, 2)[2:end]
        @assert !any(isnan, df[!,ind])
    end
    return df
end

tf_missing_cols(cols) = (col => (p -> Float32.(replace(p, missing => MISSING_FLOAT2))) => col for col in cols)
#endregion make data

#region util
function make_sequence_date(df, ind, weeks_count, cols)
    global kmsd = (;df, ind, weeks_count, cols)
    seq_len = length(cols) * DateUtil.DAYS_PER_WEEK * weeks_count
    include_ind = ind - 1
    include_date = df.date[include_ind]
    left_date = Dates.firstdayofweek(include_date - Week(weeks_count - 1))
    left_ind = searchsortedfirst(df.date, left_date)
    # @show left_ind include_ind left_date include_date

    seq = fill(MISSING_FLOAT2, seq_len)
    vws = [(@view v[left_ind:include_ind]) for v in eachcol(df)[2:end]]

    op = df[ind,2]
    @assert !iszero(op)
    vol = df[ind-1,end]
    @assert !iszero(vol)
    scales = Tuple(col == :volume ? vol : op for col in cols[1:end])
    vcu.interleave!(seq, vws, scales)
    replace!(x -> iszero(x) ? x : x - 1f0, seq)
    return seq, scales
end

function find_first_ind_date(df, seq_len)
    if Dates.dayofweek(df.date[1]) == 1
        first_ind = seq_len
    else
        first_left_date = Dates.firstdayofweek(df.date[1] + Week(1))
        first_left_ind = searchsortedfirst(df.date, first_left_date)
        @assert Dates.dayofweek(df.date[first_left_ind]) == 1
        first_ind = first_left_ind + seq_len - 1
        @assert Dates.dayofweek(df.date[first_ind]) == 5
    end
    return first_ind
end

function encode_scales(ss)
    # return log.(ss)
    return ss
    mapreduce(encode_scale, vcat, ss)
end
encode_scale(s) = Float32.(bits(s))

function bits(x::Integer)
    res = BitVector(undef, sizeof(x)*8)
    res.chunks[1] = x % UInt64
    res
end

function bits(x::Float32)
    x = isinteger(x) ? UInt32(x) : round(UInt32, x * 1000)
    res = BitVector(undef, sizeof(x)*8)
    res.chunks[1] = x % UInt64
    res
end

# import Distributions as DIST
# function proc_seq!(seq)
#     # μ, σ = mean_and_std(filter(!iszero ∘ isfinite, seq))
#     # zscore!(seq, μ, σ)
#     # return [μ, σ]
#     distri = DIST.fit(DIST.InverseGaussian, filter(!iszero, seq))
#     for i in eachindex(seq)
#         if !iszero(seq[i])
#             seq[i] = DIST.cdf(distri, seq[i])
#         end
#     end
#     return distri
# end


function ms_estimators(data)
    @assert isnothing(findfirst(!isfinite, data))
    μ, σ = mean_and_std(filter(!iszero, data))
    res = (data .- μ) ./ σ
    return res
end

# https://stackoverflow.com/a/68214067/315734
function tanh_estimators(data)
    @assert isnothing(findfirst(!isfinite, data))
    v = filter(!iszero, data)
    μ, σ = mean_and_std(v)
    res = 0.5 .* (tanh.(0.01 .* ((data .- μ) / σ)) .+ 1)
    return res
end

function median_and_mad(data)
    @assert isnothing(findfirst(!isfinite, data))
    v = filter(!iszero, data)
    med = median!(v)
    m = mad!(v; center=med)
    return med, m
end

function mm_estimators(data)
    med, m = median_and_mad(data)
    return (data .- med) ./ m, med, m
end

function test_estimators(data)
    draw(:scatter, osd.ms_estimators(data))
    draw!(:scatter, osd.mm_estimators(data))
    draw!(:scatter, osd.tanh_estimators(data) .* 100 .- 45)
end
#endregion util

end