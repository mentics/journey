module HistShapeData
using Dates, DataFrames, StatsBase
using DateUtil, DataConst, DataRead, Paths, FilesArrow
import VectorCalcUtil as vcu

# TODO: consider dividing meta by sqrt time

const NAME = replace(string(@__MODULE__), "Data" => "")

#region Public
params_data() = (;
    weeks_count = 5,
    intraday_period = Minute(30),
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
ts, prices_seq, prices_mask, prices_meta, vix_seq, vix_mask, vix_meta
=#

function make_input(params=params_data())
    prices = make_input_prices(params)
    vix = make_input_vix(params)
    prices.date = Date.(prices.ts)
    df = leftjoin(prices, vix; on=:date, renamecols=f_prefix("prices_") => f_prefix("vix_"))
    disallowmissing!(df)
    rename!(df, :prices_ts => :ts)
    select!(df, Not(:date))
    sort!(df, :ts)
    save_data(db_input(NAME), params, df)
    return df
end
f_prefix(s) = name -> s * name
#endregion Public

#region make data
function make_input_prices(params)
    weeks_count = params[:weeks_count]
    seq_len = DateUtil.TIMES_PER_WEEK * weeks_count

    res = DataFrame(ts = DateTime[], seq = Vector{Float32}[], mask=BitVector[], meta=Vector{Float32}[])

    df_prices = DataRead.get_prices(;age=Day(10))
    df_tss_all = DataFrame(:ts => DateUtil.all_weekday_ts(;period=params[:intraday_period]))
    df = leftjoin(df_tss_all, df_prices; on=:ts)
    # The data itself never includes 0, so we can use 0 for missing
    transform!(df, :price => (p -> replace(p, missing => 0f0)) => :price)
    sort!(df, :ts)
    @assert issorted(df.ts)
    @assert allunique(df.ts)

    first_ind = find_first_ind_ts(df, seq_len)
    inds = first_ind:length(df.ts)
    for ind in inds
        obs_price = df.price[ind]
        !iszero(obs_price) || continue
        obs_ts = df.ts[ind]
        seq = make_sequence_ts(df, ind, weeks_count, seq_len)
        mask = (!iszero).(seq)
        @assert typeof(mask) == BitVector
        to_returns!(seq, obs_price)
        meta = proc_seq!(seq)
        μ, σ = meta
        @assert -2f0 < μ < 2f0 "-2f0 < μ ($(μ)) < 2f0"
        @assert 0.001f0 < σ < 1f0 "0.01f0 < σ ($(σ)) < 1f0"
        seq .*= mask # restore 0's for missing
        # println(findlast(!iszero, seq))
        # if !(findlast(!iszero, seq) > (seq_len - DateUtil.TIMES_PER_WEEK))
        #     global kseq = seq
        #     @show seq obs_ts ind seq_len
        #     return df
        # end
        # This doesn't work for monday holidays
        # @assert findlast(!iszero, seq) > (seq_len - DateUtil.TIMES_PER_WEEK) "findlast(!iszero, seq) $(findlast(!iszero, seq)) > $(seq_len - DateUtil.TIMES_PER_WEEK) (seq_len - DateUtil.TIMES_PER_WEEK)" # shouldn't have a full week of zeros trailing

        push!(res, (;ts = obs_ts, seq, mask, meta))
    end
    return res
end

function make_input_vix(params)
    weeks_count = params[:weeks_count]
    seq_len = DateUtil.DAYS_PER_WEEK * weeks_count

    res = DataFrame(date = Date[], seq = Vector{Float32}[], mask=BitVector[], meta=Vector{Float32}[])

    df_vix = DataRead.get_vix()
    # Don't include today because we don't have data for it
    df_days_all = DataFrame(:date => collect(DateUtil.all_weekdays(;date_to=(DateUtil.market_today() - Day(1)))))
    df = leftjoin(df_days_all, df_vix; on=:date)
    mappings = map(names(df)[2:end]) do colname
        s = Symbol(colname)
        return s => (col -> replace(col, missing => 0f0)) => s
    end
    transform!(df, mappings...)
    sort!(df, :date)
    @assert issorted(df.date)
    @assert allunique(df.date)

    first_ind = find_first_ind_date(df, seq_len)
    inds = first_ind:length(df.date)
    for ind in inds
        obs_date = df.date[ind]
        !is_any_col(df, ind, 0f0) || continue
        seq = make_sequence_date(df, ind, weeks_count, seq_len)
        mask = (!iszero).(seq)
        @assert typeof(mask) == BitVector
        meta = proc_seq!(seq)
        μ, σ = meta
        @assert 1f0 < μ < 100f0 "1f0 < μ ($(μ)) < 100f0"
        @assert 0.1f0 < σ < 30f0 "0.1f0 < σ ($(σ)) < 30f0" # TODO: check what's that high
        seq .*= mask # restore 0's for missing
        @assert findlast(!iszero, seq) > (seq_len - DateUtil.DAYS_PER_WEEK) # shouldn't have a full week of zeros trailing

        push!(res, (;date = obs_date, seq, mask, meta))
    end
    return res
end
#endregion make data

#region prices util
function make_sequence_ts(df, ind, weeks_count, seq_len)
    include_ind = ind - 1
    include_ts = df.ts[include_ind]
    left_ts = DateUtil.week_first_ts(include_ts - Week(weeks_count - 1))
    left_ind = searchsortedfirst(df.ts, left_ts)
    seq = fill(0f0, seq_len) # 0 for missing
    copyto!(seq, 1, df.price, left_ind, include_ind - left_ind + 1)
    return seq
end

function find_first_ind_ts(df, seq_len)
    if df.ts[1] == DateUtil.week_first_ts(df.ts[1])
        first_ind = 1 + seq_len
    else
        first_left_ts = DateUtil.week_first_ts(df.ts[1] + Week(1))
        first_left_ind = searchsortedfirst(df.ts, first_left_ts)
        @assert df.ts[first_left_ind] == DateUtil.week_first_ts(df.ts[first_left_ind])
        first_ind = first_left_ind + seq_len
        @assert df.ts[first_ind] == DateUtil.week_first_ts(df.ts[first_ind]) "df.ts[first_ind] $(df.ts[first_ind]) == $(DateUtil.week_first_ts(df.ts[first_ind])) DateUtil.week_first_ts(df.ts[first_ind])"
    end
    return first_ind
end
#endregion prices util

#region vix util
function make_sequence_date(df, ind, weeks_count, seq_len)
    include_ind = ind - 1
    include_date = df.date[include_ind]
    left_date = Dates.firstdayofweek(include_date - Week(weeks_count - 1))
    left_ind = searchsortedfirst(df.date, left_date)

    seq = fill(0f0, 4 * seq_len) # 0 for missing
    vws = [(@view v[left_ind:include_ind]) for v in eachcol(df)[2:end]]
    vcu.interleave!(seq, vws)
    return seq
end

function find_first_ind_date(df, seq_len)
    if Dates.dayofweek(df.date[1]) == 1
        first_ind = 1 + seq_len
    else
        first_left_date = Dates.firstdayofweek(df.date[1] + Week(1))
        first_left_ind = searchsortedfirst(df.date, first_left_date)
        @assert Dates.dayofweek(df.date[first_left_ind]) == 1
        first_ind = first_left_ind + seq_len
        @assert Dates.dayofweek(df.date[first_ind]) == 1
    end
    return first_ind
end

function is_any_col(df, ind, val)
    for col in eachcol(df)
        if col[ind] == val
            return true
        end
    end
    return false
end
#endregion vix util

#region util
@inline to_returns!(seq, val) = seq .= val ./ seq .- 1

function proc_seq!(seq)
    μ, σ = mean_and_std(filter(!iszero ∘ isfinite, seq))
    zscore!(seq, μ, σ)
    return [μ, σ]
end
#endregion util

end