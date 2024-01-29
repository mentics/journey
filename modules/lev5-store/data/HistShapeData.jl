module HistShapeData
using Intervals, Dates, DataFrames, StatsBase
using DateUtil, DataConst, DataRead, Paths, FilesArrow
import VectorCalcUtil as vcu

# TODO: convert bool vector to bitvector for mask?

prices_seq_len(params_data) = DateUtil.TIMES_PER_WEEK * params_data.weeks_count
vix_seq_len(params_data) = 4 * DateUtil.DAYS_PER_WEEK * params_data.weeks_count

# TODO: consider dividing meta by sqrt time

const NAME = replace(string(@__MODULE__), "Data" => "")

# Can't use 0 because if we don't filter until later, there is a time when 0 becomes valid.
# but we do need it to be 0 later for the input, so just make sure to calc mask before we get to the 0-is-valid point.
const MISSING_FLOAT2 = 0f0

#region Public
params_data() = (;
    weeks_count = 5,
    intraday_period = Minute(30),
    train_date_range = DateUtil.DEFAULT_DATA_START_DATE..Date(2023,6,30),
    price_σ = 0.032887075f0,
    vix_offset = 5f0,
    vix_σ = 7.1653824,
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
    global kprice = prices = make_input_prices(params)
    global kvix = vix = make_input_vix(params)
    # prices.date = DateUtil.prev_weekday.(Date.(prices.ts))
    prices.date = DateUtil.bdaysBefore.(Date.(prices.ts), 1)
    # global kdf = df = leftjoin(prices, vix; on=:date, renamecols=f_prefix("prices_") => f_prefix("vix_"))
    global kdf = df = leftjoin(prices, vix; on=:date)
    disallowmissing!(df)
    # rename!(df, :prices_ts => :ts)
    select!(df, Not([:date, :price, :open, :high, :low, :close]))
    sort!(df, :ts)
    # Paths.save_data_params(Paths.db_input(NAME), params, df)
    return df
end
# f_prefix(s) = name -> s * name
#endregion Public

#region make data
# return dataframe with cols: ts::DateTime, seq::Vector{Float32}, mask::BitVector
function make_input_prices(params=params_data())
    df1 = make_prices_seq1(params)
    println("Working with $(size(df1,1)) rows")
    if iszero(params.price_σ)
        σ = std_for_train(df2, params.train_date_range)
        println("Update params with price_σ = $(σ)")
    else
        σ = params.price_σ
    end
    scale = 1 / (2 * σ)
    df2 = make_prices_seq2(df1, scale)
    return df2
end

function make_input_prices_raw(intraday_period)
    df_prices = DataRead.get_prices(;age=DateUtil.FOREVER2) # (;age=Day(10))
    df_tss_all = DataFrame(:ts => DateUtil.all_weekday_ts(;period=intraday_period))
    df = leftjoin(df_tss_all, df_prices; on=:ts)
    transform!(df, :price => (p -> replace(p, missing => MISSING_FLOAT2)) => :price)
    sort!(df, :ts)
    @assert issorted(df.ts)
    @assert allunique(df.ts)
    return df
end

const PS1 = Ref{Union{Nothing,DataFrame}}(nothing)
function make_prices_seq1(params=params_data())
    if !isnothing(PS1[])
        return PS1[]
    end
    weeks_count = params.weeks_count
    seq_len = prices_seq_len(params)

    df_prices = make_input_prices_raw(params.intraday_period)
    first_ind = find_first_ind_ts(df_prices, seq_len)
    inds = first_ind:length(df_prices.ts)
    m = mapreduce(hcat, inds) do ind
        make_sequence_ts(df_prices, ind, weeks_count, seq_len)
    end
    masks = [(!iszero).(m[:,c]) for c in axes(m, 2)]
    @assert typeof(masks[1]) == BitVector
    @assert length(masks[1]) == seq_len "length(masks[1]) $(length(masks[1])) == $(seq_len) seq_len"
    df_seq = DataFrame(
        :ts => df_prices.ts[first_ind:end],
        :price => df_prices.price[first_ind:end],
        :prices_mask => masks,
        :prices_seq => [m[:,i] for i in axes(m,2)],
    )
    # df_seq = DataFrame(permutedims(m), :auto)
    # masks = [(!iszero).(collect(row)) for row in eachrow(df_seq)]
    # insertcols!(df_seq, 1, :mask => masks)
    # insertcols!(df_seq, 1, :price => df_prices.price[first_ind:end])
    # insertcols!(df_seq, 1, :ts => df_prices.ts[first_ind:end])
    filter!(:price => !iszero, df_seq)
    PS1[] = df_seq
    return df_seq
end

function make_prices_seq2(df, scale)
    df2 = deepcopy(df)
    for i in eachindex(df2.prices_seq)
        v = df2.prices_seq[i]
        v .= df2.prices_mask[i] .* ((v ./ df2.price[i]) .- 1f0) .* scale
    end
    return df2
end

function std_for_train(df, train_date_range)
    df_std = filter(:1 => DateUtil.ts_in(train_date_range), df)
    return std(Iterators.flatten(df_std.prices_seq))
end

# function make_input_prices(params)
#     weeks_count = params[:weeks_count]
#     seq_len = DateUtil.TIMES_PER_WEEK * weeks_count
#     res = DataFrame(ts = DateTime[], seq = Vector{Float32}[], mask=BitVector[], meta=Vector{Float32}[])
#     df = make_input_prices_raw(params)

#     first_ind = find_first_ind_ts(df, seq_len)
#     inds = first_ind:length(df.ts)
#     for ind in inds
#         obs_price = df.price[ind]
#         !iszero(obs_price) || continue
#         obs_ts = df.ts[ind]
#         seq = make_sequence_ts(df, ind, weeks_count, seq_len)
#         return seq
#         mask = (!iszero).(seq)
#         @assert typeof(mask) == BitVector
#         to_returns!(seq, obs_price)
#         meta = proc_seq!(seq)
#         μ, σ = meta
#         @assert -2f0 < μ < 2f0 "-2f0 < μ ($(μ)) < 2f0"
#         @assert 0.001f0 < σ < 1f0 "0.01f0 < σ ($(σ)) < 1f0"
#         seq .*= mask # restore 0's for missing
#         # println(findlast(!iszero, seq))
#         # if !(findlast(!iszero, seq) > (seq_len - DateUtil.TIMES_PER_WEEK))
#         #     global kseq = seq
#         #     @show seq obs_ts ind seq_len
#         #     return df
#         # end
#         # This doesn't work for monday holidays
#         # @assert findlast(!iszero, seq) > (seq_len - DateUtil.TIMES_PER_WEEK) "findlast(!iszero, seq) $(findlast(!iszero, seq)) > $(seq_len - DateUtil.TIMES_PER_WEEK) (seq_len - DateUtil.TIMES_PER_WEEK)" # shouldn't have a full week of zeros trailing

#         push!(res, (;ts = obs_ts, seq, mask, meta))
#     end
#     return res
# end

# function make_input_vix(params)
#     weeks_count = params[:weeks_count]
#     seq_len = DateUtil.DAYS_PER_WEEK * weeks_count

#     res = DataFrame(date = Date[], seq = Vector{Float32}[], mask=BitVector[], meta=Vector{Float32}[])

#     df_vix = DataRead.get_vix()
#     # Don't include today because we don't have data for it
#     df_days_all = DataFrame(:date => collect(DateUtil.all_weekdays(;date_to=(DateUtil.market_today() - Day(1)))))
#     df = leftjoin(df_days_all, df_vix; on=:date)
#     mappings = map(names(df)[2:end]) do colname
#         s = Symbol(colname)
#         return s => (col -> replace(col, missing => 0f0)) => s
#     end
#     transform!(df, mappings...)
#     sort!(df, :date)
#     @assert issorted(df.date)
#     @assert allunique(df.date)

#     first_ind = find_first_ind_date(df, seq_len)
#     inds = first_ind:length(df.date)
#     for ind in inds
#         obs_date = df.date[ind]
#         !is_any_col(df, ind, 0f0) || continue
#         seq = make_sequence_date(df, ind, weeks_count, seq_len)
#         mask = (!iszero).(seq)
#         @assert typeof(mask) == BitVector
#         meta = proc_seq!(seq)
#         μ, σ = meta
#         @assert 1f0 < μ < 100f0 "1f0 < μ ($(μ)) < 100f0"
#         @assert 0.1f0 < σ < 30f0 "0.1f0 < σ ($(σ)) < 30f0" # TODO: check what's that high
#         seq .*= mask # restore 0's for missing
#         @assert findlast(!iszero, seq) > (seq_len - DateUtil.DAYS_PER_WEEK) # shouldn't have a full week of zeros trailing

#         push!(res, (;date = obs_date, seq, mask, meta))
#     end
#     return res
# end

#=
vix.date is the last date included in the sequence.
So, for a given date, you should use prev_weekday(date) to avoid using future data.
=#
function make_input_vix(params=params_data())
    df1 = make_vix_seq1(params)
    println("Vix working with $(size(df1,1)) rows")
    if iszero(params.vix_σ)
        σ = std_for_train(df1, params.train_date_range)
        println("Update params with vix_σ = $(σ)")
    else
        σ = params.vix_σ
    end

    scale = 1 / (4*σ)
    for i in eachindex(df1.vix_seq)
        v = df1.vix_seq[i]
        v .= df1.vix_mask[i] .* (v .- params.vix_offset) .* scale
    end
    return df1
end

function make_vix_raw()
    df_vix = DataRead.get_vix(;age=DateUtil.FOREVER2)
    # df_days_all = DataFrame(:date => collect(DateUtil.all_weekdays(;date_to=(DateUtil.market_today() - Day(1)))))
    df_days_all = DataFrame(:date => collect(DateUtil.all_weekdays(;date_to=(DateUtil.market_today()))))
    df = leftjoin(df_days_all, df_vix; on=:date)
    mappings = map(names(df)[2:end]) do colname
        s = Symbol(colname)
        return s => (col -> replace(col, missing => MISSING_FLOAT2)) => s
    end
    transform!(df, mappings...)
    sort!(df, :date)
    @assert issorted(df.date)
    @assert allunique(df.date)
    return df
end

function make_vix_seq1(params=params_data())
    weeks_count = params.weeks_count
    vix_count = vix_seq_len(params) ÷ 4

    df = make_vix_raw()
    first_ind = find_first_ind_date(df, vix_count)
    inds = first_ind:lastindex(df.date)
    m = mapreduce(hcat, inds) do ind
        make_sequence_date(df, ind, weeks_count, vix_count)
    end
    df_seq = DataFrame(
        :date => df.date[inds],
        :open => df.open[inds],
        :high => df.high[inds],
        :low => df.low[inds],
        :close => df.close[inds],
        :vix_mask => [(!iszero).(m[:,c]) for c in axes(m, 2)],
        :vix_seq => [m[:,i] for i in axes(m,2)],
    )
    @assert typeof(df_seq.vix_mask[1]) == BitVector
    @assert length(df_seq.vix_mask[1]) == 4*vix_count "length(df_seq.vix_mask[1]) $(length(df_seq.vix_mask[1])) == $(4*vix_count) 4*seq_len"

    # df_seq = DataFrame(permutedims(m), :auto)
    # insertcols!(df_seq, 1, :mask => masks)
    # insertcols!(df_seq, 1, :close => df.close[first_ind:end])
    # insertcols!(df_seq, 1, :low => df.low[first_ind:end])
    # insertcols!(df_seq, 1, :high => df.high[first_ind:end])
    # insertcols!(df_seq, 1, :open => df.open[first_ind:end])
    # insertcols!(df_seq, 1, :date => df.date[first_ind:end])
    # # TODO: search all 0 col vals and check if the :date for that row is bday?
    global kdf_seq = df_seq
    filter!([:open, :high, :low, :close] => ((o, h, l, c) -> all(!iszero, (o, h, l, c))), df_seq)
    @assert df_seq.vix_seq[1][findlast(!iszero, df_seq.vix_seq[1])] == df_seq.close[1]
    @assert df_seq.vix_seq[end][findlast(!iszero, df_seq.vix_seq[end])] == df_seq.close[end]
    return df_seq
end

# function std_for_train_vix(df, train_date_range)
#     df2 = filter(:date => (date -> date in train_date_range), df)
#     xcol = columnindex(df2,:x1)
#     itr = Iterators.flatten(collect(df2[i,xcol:end])[df2.mask[i]] for i in axes(df2, 1))
#     return std(itr)
# end
#endregion make data

#region prices util
function make_sequence_ts(df, ind, weeks_count, seq_len)
    include_ind = ind - 1
    include_ts = df.ts[include_ind]
    left_ts = DateUtil.week_first_ts(include_ts - Week(weeks_count - 1))
    left_ind = searchsortedfirst(df.ts, left_ts)
    seq = fill(MISSING_FLOAT2, seq_len)
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
    include_ind = ind
    include_date = df.date[include_ind]
    left_date = Dates.firstdayofweek(include_date - Week(weeks_count - 1))
    left_ind = searchsortedfirst(df.date, left_date)

    seq = fill(MISSING_FLOAT2, 4 * seq_len)
    vws = [(@view v[left_ind:include_ind]) for v in eachcol(df)[2:end]]
    vcu.interleave!(seq, vws)
    return seq
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

# function is_any_col(df, ind, val)
#     for col in eachcol(df)
#         if col[ind] == val
#             return true
#         end
#     end
#     return false
# end
#endregion vix util

#region util
# @inline to_returns!(seq, val) = seq .= val ./ seq .- 1

# function proc_seq!(seq)
#     μ, σ = mean_and_std(filter(!iszero ∘ isfinite, seq))
#     zscore!(seq, μ, σ)
#     return [μ, σ]
# end
#endregion util

end