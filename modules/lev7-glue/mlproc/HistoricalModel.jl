module HistoricalModel
using Dates, DataFrames
using StatsBase
using Flux, MLUtils, CUDA
import NNlib
# using FLoops, FoldsThreads
using DateUtil, CollUtil, IndexUtil
import DataFiles as dat
import DataFiles:missing_to_zero_float
using ModelUtil, TrainUtil, FileUtil
using MLRun
import VectorCalcUtil as vcu

NAME = string(@__MODULE__)
MOD_VERSION = "latent32mult2block1drop4z"

#region Inference and combined training
CACHE_INFERENCE = nothing
function load_inference(version=MOD_VERSION)
    if isnothing(CACHE_INFERENCE)
        # TODO: cache? versioning?
        inference_model = model_encoder() |> gpu
        ModelUtil.load_inference(NAME, MOD_VERSION, inference_model)
        global CACHE_INFERENCE = (;
            model = inference_model,
            run = batch -> run_encoder(inference_model, batch),
        )
    end
    return CACHE_INFERENCE
end

ENCODED_CACHE = nothing
ENCODED_PATH = joinpath(FileUtil.root_shared(), "mlrun", "HistoricalModel", "data", "HistoricalModel-encoded.arrow")
function load_data_encoded()
    if isnothing(ENCODED_CACHE)
        global ENCODED_CACHE = Arrow.Table(ENCODED_PATH)
    end
    return ENCODED_CACHE
end

function make_data_encoded()
    (;model, run) = load_inference()
    data = load_data_input()
    @assert unique(data.ts) == data.ts
    batch_size = config().batch_size
    width = config().encoded_width + 4
    df = DataFrame([DateTime[], [Float32[] for _ in 1:width]...], [:ts, [Symbol("c$(i)") for i in 1:width]...])
    for inds in IndexUtil.batch_inds_all(eachindex(data.ts), batch_size)
        enc = run(batch_from_inds(data, inds)) |> cpu
        # m = hcat(data.ts[inds], vcat(v, meta_under, meta_vix)')
        m = hcat(data.ts[inds], enc')
        push!.(Ref(df), [m[i,:] for i in 1:size(m, 1)])
    end
    @assert unique(df.ts) == df.ts
    dat.save(ENCODED_PATH, df)
    return df
end
#endregion Inference and combined training

#region Config
data_params() = (;
)

model_params() = (;
    data_weeks_count = 5,
    encoded_width = 32,
    block_count = 1,
    layers_per_block = 2,
    hidden_width_mult = 2,
    dropout = 0.2f0,
    activation = NNlib.swish,
    use_bias = false,
)

train_hypers() = (;
    batch_size = 32,
    holdout = 0.1,
)

function config()
    (;data_weeks_count, hidden_width_mult, encoded_width) = model_params()
    input_width_under = DateUtil.TIMES_PER_WEEK * data_weeks_count
    vix_count = DateUtil.DAYS_PER_WEEK * data_weeks_count
    input_width_vix = vix_count * 4
    input_width_meta = 2
    input_width = input_width_under + input_width_vix
    hidden_width_under = hidden_width_mult * input_width_under
    hidden_width_vix = hidden_width_mult * input_width_vix
    hidden_width = hidden_width_mult * input_width
    encoded_with_meta = encoded_width + 4
    return (;
        data_params()...,
        model_params()...,
        train_hypers()...,
        vix_count,
        input_width_under, input_width_vix,
        input_width_meta,
        # input_width,
        hidden_width_under, hidden_width_vix, hidden_width,
        encoded_with_meta,
    )
end
#endregion Config

#region MLRun Interface
function mlrun()
    inference_model = model_encoder() |> gpu
    training_model = Chain(; encoder=inference_model, decoder=model_decoder()) |> gpu

    global state = Trainee7(;
        name=NAME,
        version=MOD_VERSION,
        training_model,
        inference_model,
        run_model = batch -> autoencoder(training_model, batch),
        infer = batch -> run_encoder(inference_model, batch),
        batches = make_data(),
        get_learning_rate = TrainUtil.learning_rate_linear_decay(), # TrainUtil.lr_cycle_decay(),
        get_loss = calc_loss,
        cfg = config(),
        mod = @__MODULE__
    )
    return state
end

to_draw_x(batch, ind) = batch.under.v[1:end,ind]
to_draw_y(batch, ind) = batch.under.v[1:end,ind]
to_draw_yh(yhat, ind) = yhat.under[1:end,ind]
# to_draw_x(batch, ind) = batch.vix.v[1:end,ind]
# to_draw_yh(yhat, ind) = yhat.vix[1:end,ind]
#endregion MLRun Interface

#region mlrun impl
function autoencoder(model, batch)
    encoded = run_encoder(model.layers.encoder, batch)
    decoded = run_decoder(model.layers.decoder, encoded, batch)
    return decoded

    # (;under, under_mask, under_meta, vix, vix_mask, vix_meta) = batch
    # (yhat_under_raw, yhat_vix_raw) = model((vcat(under, under_meta), vcat(vix, vix_meta)))
    # yhat_under = vcat(yhat_under_raw .* under_mask, under_meta)
    # yhat_vix = vcat(yhat_vix_raw .* vix_mask, vix_meta)
    # return (vcat(yhat_under, under_meta), vcat(yhat_vix, vix_meta))
end

function run_encoder(encoder, batch)
    # It's expected that the input is already masked, so no need to multiply it by the mask
    v = encoder((vcat(batch.under.v, batch.under.meta), vcat(batch.vix.v, batch.vix.meta)))
    return vcat(v, batch.under.meta, batch.vix.meta)
end

function run_decoder(decoder, encoded, batch)
    (dec_under_raw, dec_vix_raw) = decoder(encoded)
    under = dec_under_raw .* batch.under.mask
    vix = dec_vix_raw .* batch.vix.mask
    return (;under, vix)
end

function calc_loss(model, batch)
    (;under, vix) = autoencoder(model, batch)
    # l = Flux.mae(decoded.under.v, under) + Flux.mae(decoded.vix.v, vix)
    l = Flux.mse(under, batch.under.v) + Flux.mae(vix, batch.vix.v)
    # global kloss_vals = (;under, under_mask, vix, vix_mask, yhat_under_1, yhat_vix_1, yhat_under, yhat_vix, loss=l)
    return l
end
#endregion mlrun impl

#region Data
const CACHE_DF_UNDER2 = Ref{DataFrame}()
const CACHE_DF_VIX2 = Ref{DataFrame}()
const CACHE_OBS = Ref{Vector{DateTime}}()

function get_data(;reset=false)
    cfg = config()
    if reset || !isassigned(CACHE_OBS)
        under = dat.ts_allperiods_df()
        dat.convert_cols!(missing_to_zero_float, under, :under)
        CACHE_DF_UNDER2[] = under

        vix = dat.vix_alldates_df()
        dat.convert_cols!(missing_to_zero_float, vix, :open, :high, :low, :close)
        CACHE_DF_VIX2[] = vix

        bad_dates = vix.date[findall(iszero, vix.open)] # open or close?
        # println(bad_dates)

        skip_back_count = DateUtil.TIMES_PER_WEEK * cfg.data_weeks_count + 1 # +1 for the current ts
        # obs_ts = filter(:under => !ismissing, df_ts).ts[skip_back_count:end]
        # obs = filter(:under => !iszero, under).ts[skip_back_count:end]
        obs = filter!(ts -> !(Date(ts) in bad_dates), filter(:under => !iszero, under).ts[skip_back_count:end])
        CACHE_OBS[] = obs
    end
    return (;
        under = CACHE_DF_UNDER2[],
        vix = CACHE_DF_VIX2[],
        obs = CACHE_OBS[]
    )
end

function make_data_input()
    under = make_data_under()
    vix = make_data_vix()
    valid_dates = Set(vix.date)
    filter!(:ts => ts -> Date(ts) in valid_dates, under)
    # transform(under,
    #     :ts => (ts -> Date.(ts)) => :date,
    #     :v -> :under_v
    # )
    # under[!,:date] = Date.(under[!,:ts])
    len_before = length(under.ts)
    combined = select(under, :ts, :v => :under_v, :meta => :under_meta, :mask => :under_mask, :ts => (ts -> Date.(ts)) => :date)
    leftjoin!(combined, vix; on=:date)
    dropmissing!(combined; disallowmissing=true) # fix column types to not have union missing
    @assert length(combined.ts) == len_before
    rename!(combined, :v => :vix_v, :meta => :vix_meta, :mask => :vix_mask)
    select!(combined, Not(:date))

    # [(;ts=p.first, v=p.second.v, meta=p.second.meta, mask=p.second.mask) for p in pairs(under)]
    dat.save(INPUT_PATH, combined)
    return combined
end

INPUT_PATH = joinpath(FileUtil.root_shared(), "mlrun", "HistoricalModel", "data", "HistoricalModel-input.arrow")

INPUT_CACHE = nothing
import Arrow
function load_data_input()
    if isnothing(INPUT_CACHE)
        global INPUT_CACHE = Arrow.Table(INPUT_PATH)
    end
    return INPUT_CACHE
end

function make_data()
    cfg = config()
    input = load_data_input()
    # obs_count = size(input.ts, 1)
    # holdout_count = floor(Int, obs_count * cfg.holdout)
    # train_count = obs_count - holdout_count
    # batch_count = train_count ÷ cfg.batch_size
    # all_inds = Random.randperm(obs_count)
    # inds_train = all_inds[1:(end - holdout_count)]
    # inds_holdout = all_inds[(end - holdout_count + 1):end]
    # inds_batches = batch_inds(inds_train, cfg.batch_size, batch_count)
    (;batch_count, inds_batches, inds_holdout) = IndexUtil.inds_for_batches(size(input.ts, 1), cfg.batch_size, cfg.holdout)
    # TODO: write directly to CuArray's?
    get_batch = function(iepoch, ibatch)
        inds = inds_batches[ibatch]
        return batch_from_inds(input, inds)

        # vw = @view input[inds_batches[ibatch],:]
        # return (;
        #     under = (;v=reduce(hcat, vw.under_v), meta=reduce(hcat, vw.under_meta), mask=reduce(hcat, vw.under_mask)),
        #     vix = (;v=reduce(hcat, vw.vix_v), meta=reduce(hcat, vw.vix_meta), mask=reduce(hcat, vw.vix_mask)),
        # )
    end
    holdout = batch_from_inds(input, inds_holdout)
    return Batches2(;get=get_batch, count=batch_count, holdout)
end

function batch_from_inds(input, inds)
    return (;
        under = (;v=reduce(hcat, input.under_v[inds]), meta=reduce(hcat, input.under_meta[inds]), mask=reduce(hcat, input.under_mask[inds])),
        vix = (;v=reduce(hcat, input.vix_v[inds]), meta=reduce(hcat, input.vix_meta[inds]), mask=reduce(hcat, input.vix_mask[inds])),
    ) |> gpu
end

function make_data_under()
    cfg = config()
    weeks_count = cfg.data_weeks_count

    under = dat.ts_allperiods_df()
    unders = replace(under.under, missing => 0f0)
    # res = Dict{DateTime,DataMetaMask2}()
    # sizehint!(res, 40000)
    res = DataFrame(ts = DateTime[], v = Vector{Float32}[], meta=Vector{Float32}[], mask=BitVector[])

    len = DateUtil.TIMES_PER_WEEK * weeks_count
    skip_back_count = len + 1 # +1 for the current ts
    # tss = under.ts[skip_back_count:end]
    inds = skip_back_count:length(under.ts)
    # prev_ts = DateTime(0)
    # ex = WorkStealingEx()
    # @floop ex for ind in inds
    # @floop for ind in inds
    for ind in inds
        cur_under = unders[ind]
        if iszero(cur_under)
            # println("skipping due to 0 cur $(ind)")
            continue
        end
        # !iszero(cur_under) || continue
        cur_ts = under.ts[ind]
        # @assert cur_ts > prev_ts

        include_ind = ind - 1
        if iszero(unders[include_ind])
            # TODO: maybe not skip these? these will skip the first ts after holidays
            # println("skipping due to 0 include_ind $(include_ind)")
            continue
        end
        include_ts = under.ts[include_ind]
        from_ts = DateUtil.week_first_ts(include_ts - Week(weeks_count - 1))
        from_ind = searchsortedfirst(under.ts, from_ts)
        # to_ts = DateUtil.week_last_ts(include_ts)
        # to_ind = searchsortedfirst(under.ts, to_ts)
        include_inds = from_ind:include_ind

        # weeks_tss = DateUtil.get_weeks_tss(ts, weeks_count)
        # inds = inds_for_sorted(under.ts, weeks_tss[1], weeks_tss[end])
        # include_ts = DateUtil.prev_weekday_ts(ts)
        # @assert under.ts[inds] == weeks_tss
        @assert length(include_inds) <= len

        vw = @view unders[include_inds]
        !too_many_zeros(vw) || continue

        v = fill(0f0, len)
        v[axes(vw, 1)] .= vw
        mask = (!iszero).(v)
        @assert typeof(mask) == BitVector
        v .= cur_under ./ v .* mask

        μ, σ = mean_and_std(filter(!iszero, v))
        @assert -2f0 < μ < 2f0 "-2f0 < μ ($(μ)) < 2f0"
        @assert 0.001f0 < σ < 1f0 "0.01f0 < σ ($(σ)) < 1f0"
        zscore!(v, μ, σ)
        v .*= mask # restore 0's for missing

        @assert iszero(@view v[(length(include_inds) + 1):end])
        @assert findlast(!iszero, v) > (len - DateUtil.TIMES_PER_WEEK) # shouldn't have a full week of zeros trailing

        # res[cur_ts] = DataMetaMask2(v, [μ, σ], mask)
        push!(res, (;ts = cur_ts, v, meta=[μ, σ], mask))

        # p = cur_ts => DataMetaMask2(v, [μ, σ], mask)
        # @reduce(res = vcat(Pair{DateTime,DataMetaMask2}[], p))


        # @reduce(res = push!(Dict{DateTime,DataMetaMask2}(), p))
        # @reduce(res = push!(Dict{DateTime,DataMetaMask2}(), cur_ts => DataMetaMask2(v, [μ, σ], mask)))
        # @reduce() do (res = Dict{DateTime,DataMetaMask2}(); p)
        #     # res[cur_ts] = DataMetaMask2(v, [μ, σ], mask)
        #     res[p.first] = p.second
        #     # push!(res, p)
        # end

        # prev_ts = cur_ts
    end

    # return Dict(res)
    return res
end

function make_data_vix()
    cfg = config()
    weeks_count = cfg.data_weeks_count

    vix = dat.vix_alldates_df()
    vixs = map(names(vix)[2:end]) do colname
        replace(vix[!,colname], missing => 0f0)
    end
    # res = Dict{Date,DataMetaMask2}()
    # sizehint!(res, 4000)
    res = DataFrame(date = Date[], v = Vector{Float32}[], meta=Vector{Float32}[], mask=BitVector[])

    len = 5 * weeks_count
    skip_back_count = len + 1 # +1 for the current date
    inds = skip_back_count:length(vix.date)
    prev_date = Date(0)
    # ex = WorkStealingEx()
    # @floop ex for ind in inds
    for ind in inds
        if is_any_zero(vixs, ind)
            # println("skipping due to 0 cur $(ind)")
            continue
        end
        # !iszero(cur_under) || continue
        cur_date = vix.date[ind]
        @assert cur_date > prev_date

        include_ind = ind - 1
        if is_any_zero(vixs, include_ind)
            # TODO: maybe not skip these? these will skip the first ts after holidays
            # println("skipping due to 0 include_ind $(include_ind)")
            continue
        end
        include_date = vix.date[include_ind]
        from_date = Dates.firstdayofweek(include_date - Week(weeks_count - 1))
        from_ind = searchsortedfirst(vix.date, from_date)
        include_inds = from_ind:include_ind
        row_len = length(include_inds)
        @assert row_len <= len

        vws = [(@view v[include_inds]) for v in vixs]
        !any(too_many_zeros, vws) || continue

        v = fill(0f0, 4 * len)
        global kv = (;v, vws, include_inds)
        vcu.interleave!(v, vws)
        mask = (!iszero).(v)
        @assert typeof(mask) == BitVector
        # v .= cur_under ./ v .* mask

        μ, σ = mean_and_std(filter(!iszero, v))
        @assert 1f0 < μ < 100f0 "1f0 < μ ($(μ)) < 100f0"
        @assert 0.1f0 < σ < 30f0 "0.1f0 < σ ($(σ)) < 30f0" # TODO: check what's that high
        zscore!(v, μ, σ)
        v .*= mask # restore 0's for missing

        @assert iszero(@view v[(4 * length(include_inds) + 1):end])
        @assert findlast(!iszero, v) > 4 * (len - DateUtil.TIMES_PER_WEEK) # shouldn't have a full week of zeros trailing
        # res[cur_date] = DataMetaMask2(v, [μ, σ], mask)
        push!(res, (;date = cur_date, v, meta = [μ, σ], mask))
        prev_date = cur_date
    end

    return res
end
#endregion Data

#region Model
function model_encoder()
    cfg = config()
    input_under = Dense(cfg.input_width_under + cfg.input_width_meta => cfg.hidden_width_under; bias=cfg.use_bias)
    input_vix = Dense(cfg.input_width_vix + cfg.input_width_meta => cfg.hidden_width_vix; bias=cfg.use_bias)
    layer_input = Parallel(vcat; input_under, input_vix)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks1_count]
    dropout = Dropout(cfg.dropout)
    blocks2 = [SkipConnection(ModelUtil.make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks2_count]

    layer_output = Dense(through_width => cfg.encoded_width; bias=false)
    return Chain(;encoder_input=layer_input, encoder_blocks=Chain(blocks1..., dropout, blocks2...), encoder_output=layer_output)
end

function model_decoder()
    cfg = config()
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix
    layer_input = Dense(cfg.encoded_with_meta => through_width, cfg.activation; bias=false)

    # blocks = [SkipConnection(ModelUtil.make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:cfg.block_count]

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks1_count]
    # dropout = Dropout(0.1)
    blocks2 = [SkipConnection(ModelUtil.make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks2_count]

    output_under = Dense(cfg.hidden_width_under => cfg.input_width_under; bias=cfg.use_bias)
    output_vix = Dense(cfg.hidden_width_vix => cfg.input_width_vix; bias=cfg.use_bias)
    layer_output = SplitLayer((output_under, output_vix), cfg.hidden_width_under)
    # layer_output = in -> (output_under(in[1:cfg.hidden_width_under,:]), output_vix(in[cfg.hidden_width_under+1:cfg.hidden_width]))
    return Chain(decoder_input=layer_input, decoder_blocks=Chain(blocks1..., blocks2...), decoder_output=layer_output)
end
#endregion Model

#region util
too_many_zeros(v; ratio=0.3) = count(iszero, v) >= (ratio * length(v))
# too_many_missing(v; ratio=0.3) = count(ismissing, v) >= (ratio * length(v))

is_any_zero(vs, ind) = any(iszero, (v[ind] for v in vs))

# function inds_for_sorted(v, left, right)
#     ind1 = searchsortedfirst(v, left)
#     ind2 = searchsortedfirst(v, right)
#     return ind1:ind2
# end

# using BenchmarkTools
# function test()
#     ca = rand(Float32, 10, 5) |> gpu
#     v = rand(Float32, 10)
#     @time cavw = @view ca[:,1]
#     # Using a view avoided allocations during the copyto
#     @btime copyto!($cavw, $v)
# end
#endregion util



#region old make data
# data_sizes(cfg) = (;
#     under = (DateUtil.TIMES_PER_WEEK * cfg.data_weeks_count, cfg.batch_size),
#     under_meta = (2, cfg.batch_size),
#     vix = (4 * DateUtil.DAYS_PER_WEEK * cfg.data_weeks_count, cfg.batch_size),
#     vix_meta = (2, cfg.batch_size)
# )


# function make_data()
#     cfg = config()
#     dfs = get_data()

#     obs_count = size(dfs.obs, 1)
#     batch_count = (obs_count - cfg.input_width_under) ÷ cfg.batch_size
#     lfsr = IndexUtil.lfsr(obs_count)
#     sizes = data_sizes(cfg)

#     bufs_row = make_bufs_row(sizes)
#     under = Array{Float32, 2}(undef, sizes.under...)
#     under_mask = Array{Float32, 2}(undef, sizes.under...)
#     under_meta = Array{Float32, 2}(undef, sizes.under_meta...)
#     vix = Array{Float32, 2}(undef, sizes.vix...)
#     vix_mask = Array{Float32, 2}(undef, sizes.vix...)
#     vix_meta = Array{Float32, 2}(undef, sizes.vix_meta...)

#     make_batch = function(epochi, batchi)
#         # TODO: use random variation instead of epochi?
#         starti = epochi + batchi * cfg.batch_size
#         for i in 1:cfg.batch_size
#             ind = IndexUtil.lfsr_next(lfsr, starti + i)

#             make_seq!(bufs_row, dfs, ind)

#             under[:,i] .= bufs_row.under.v
#             under_mask[:,i] .= bufs_row.under.mask
#             under_meta[:,i] .= bufs_row.under.meta
#             vix[:,i] .= bufs_row.vix.v
#             vix_mask[:,i] .= bufs_row.vix.mask
#             vix_meta[:,i] .= bufs_row.vix.meta
#         end

#         return (;
#             under = (;v=under, mask=under_mask, meta=under_meta),
#             vix = (;v=vix, mask=vix_mask, meta=vix_meta)
#         )
#     end

#     get_inds = function(epochi, batchi)
#         starti = epochi + batchi * cfg.batch_size
#         return map(1:cfg.batch_size) do i
#             IndexUtil.lfsr_next(lfsr, starti + i)
#         end
#     end

#     # if preload...
#     println("Making all batches...")
#     # batches = [map(copy, make_batch(0, i)) for i in 1:batch_count]
#     batches = [deepcopy(make_batch(0, i)) for i in 1:batch_count]
#     println(" done.")
#     get_batch = (epochi, ibatch) -> batches[ibatch] |> gpu
#     # return (;get_batch, batch_count, cfg.batch_size, get_inds, obs_count)
#     return (;get_batch, batch_count)
# end

# function make_bufs_row(sizes)
#     under_len, under_meta_len, vix_len, vix_meta_len = sizes.under[1], sizes.under_meta[1], sizes.vix[1], sizes.vix_meta[1]
#     return (;
#         under = (;
#             v = Vector{Float32}(undef, under_len),
#             mask = Vector{Float32}(undef, under_len),
#             meta = Vector{Float32}(undef, under_meta_len)
#         ),
#         vix = (;
#             v = Vector{Float32}(undef, vix_len),
#             mask = Vector{Float32}(undef, vix_len),
#             meta = Vector{Float32}(undef, vix_meta_len)
#         )
#     )
# end

# function make_seq!(bufs, dfs, ind)
#     cfg = config()
#     (;under, vix, obs) = dfs
#     cur_ts = obs[ind]
#     cur_ind = searchsortedfirst(under.ts, cur_ts)
#     to_ind = cur_ind - 1
#     to_ts = under.ts[to_ind]
#     # to_date = Date(to_ts)

#     from_ts = DateUtil.first_weekday_ts(to_ts - Week(cfg.data_weeks_count - 1))
#     from_ind = searchsortedfirst(under.ts, from_ts)
#     end_ind = from_ind + cfg.data_weeks_count * DateUtil.TIMES_PER_WEEK - 1

#     inds = from_ind:end_ind

#     @assert Week(cfg.data_weeks_count-1) <= round(cur_ts - under.ts[inds[1]], Week) <= Week(cfg.data_weeks_count)

#     missing_max = 0.3 * length(inds)
#     missing_count = count(iszero, (@view under.under[inds]))
#     # println("missing: $(missing_count) / $(missing_max)")
#     if missing_count >= missing_max
#         println("missing count $(missing_count) > missing max $(missing_max) for ind $(ind). Returning 0 row")
#         fill!(bufs.under.v, 0.0)
#         fill!(bufs.under.mask, 0.0)
#         fill!(bufs.under.meta, 0.0)
#         fill!(bufs.vix.v, 0.0)
#         fill!(bufs.vix.mask, 0.0)
#         fill!(bufs.vix.meta, 0.0)
#         return inds
#     end

#     # make_row_under!(bufs.under, under, inds, cur_ind)
#     # make_row_vix!(bufs.vix, vix, Date(from_ts), Date(cur_ts))
#     wrap_row!(bufs.under, make_row_under!, under, inds, cur_ind)
#     wrap_row!(bufs.vix, make_row_vix!, vix, Date(from_ts), Date(cur_ts))
#     return inds
# end

# function test_seq(ind)
#     cfg = config()
#     dfs = get_data()
#     bufs = make_bufs_row(data_sizes(cfg))
#     inds = make_seq!(bufs, dfs, ind)
#     tss = dfs.under.ts[inds]
#     return (;bufs, inds, tss)
# end

# function wrap_row!(bufs, f, args...)
#     # global kwrap_row = (;bufs, f, args)
#     # views = (;v=(@view bufs.v[1:end-2]), mask=(@view bufs.mask[1:end-2]))
#     f(bufs, args...)
#     bufs_orig = (;v=copy(bufs.v), mask=copy(bufs.mask))

#     if iszero(bufs.v)
#         throw("what!")
#     end
#     if !are_all_finite(bufs.v)
#         error("f returned bad data")
#     end

#     μ, σ = mean_and_std(filter(!iszero, bufs.v))
#     @assert -2f0 < μ < 80f0 "-2f0 < μ ($(μ)) < 80f0"
#     @assert 0.001f0 < σ < 32f0 "0.001f0 < σ ($(σ)) < 32f0" # TODO: too low? check which date this is for?
#     zscore!(bufs.v, μ, σ)
#     # mask_count = count(iszero, views.mask)
#     bufs.v .*= bufs.mask
#     bufs.meta[1] = μ
#     bufs.meta[2] = σ
#     # bufs.v[end-1] = μ
#     # bufs.v[end] = σ
#     # bufs.mask[end-1] = 1f0
#     # bufs.mask[end] = 1f0
#     @assert are_all_finite(bufs.v)
#     # @assert count(iszero, bufs.v) == count(iszero, bufs.mask) "$(f) count(iszero, bufs.v) == count(iszero, bufs.mask): $(count(iszero, bufs.v)) == $(count(iszero, bufs.mask)) ; $(mask_count)"
#     if count(iszero, bufs.v) != count(iszero, bufs.mask)
#         vz = findall(iszero, bufs.v)
#         mz = findall(iszero, bufs.mask)
#         inds = setdiff(vz, mz)
#         for ind in inds
#             if bufs_orig.v[ind] != μ
#                 println("found zero that wasn't mean")
#                 @show ind bufs.v[ind] bufs_orig.v[ind] μ
#             end
#         end
#     end
# end

# function make_row_under!(bufs, df, inds, cur_ind)
#     # global kmru_args = (;bufs=map(copy, bufs), df, inds, cur_ind)
#     buf, buf_mask = bufs.v, bufs.mask

#     cur = df.under[cur_ind]
#     @assert 50f0 < cur < 1000f0
#     i_buf = 1
#     for i_df in inds
#         @assert -DateUtil.TIMES_PER_WEEK <= ((cur_ind-1) - inds[1]) < size(buf, 1) "assert i:$(i_df): $(-DateUtil.TIMES_PER_WEEK) <= $(cur_ind) - $(ind) ($(cur_ind - ind)) < $(size(buf, 1))"
#         x = df.under[i_df]
#         if i_df >= cur_ind || x <= 0f0
#             # println("skipping $(dfi) > $(targeti) || $(x) <= 0f0")
#             buf[i_buf] = 0f0
#             buf_mask[i_buf] = 0f0
#         else
#             buf[i_buf] = cur / x - 1f0
#             buf_mask[i_buf] = 1f0
#             # @show cur x buf[i_buf] i_df i_buf cur_ind inds
#         end
#         i_buf += 1
#     end
#     if any(!isfinite, buf)
#         println("ERROR: bad under data")
#         @show buf
#         global krowunder = (;buf, buf_mask, df, i, ts_start, ts, ind_start, cur)
#     end
#     if iszero(buf) || iszero(buf_mask)
#         println("ERROR: row all zeros")
#         global krowunder = (;buf, buf_mask, df, inds, cur_ind, cur)
#     end
# end

# function make_row_vix!(bufs, df, from_date, cur_date)
#     # global kmake_row_vix_args = (;bufs, df, from_date, cur_date)
#     buf, buf_mask = bufs.v, bufs.mask

#     ind_start = searchsortedfirst(df.date, from_date)
#     cur_ind = searchsortedfirst(df.date, cur_date)
#     cur_row = df[cur_ind,:]
#     @assert 2f0 < cur_row.open < 100f0
#     for i in 0:(size(buf, 1) ÷ 4 - 1)
#         ind = ind_start + i
#         open = df.open[ind]
#         if ismissing(open) || open <= 0f0 || df.date[ind] >= cur_date
#             buf[i*4+1] = 0f0
#             buf[i*4+2] = 0f0
#             buf[i*4+3] = 0f0
#             buf[i*4+4] = 0f0
#             buf_mask[i*4+1] = 0f0
#             buf_mask[i*4+2] = 0f0
#             buf_mask[i*4+3] = 0f0
#             buf_mask[i*4+4] = 0f0
#         else
#             # buf[i*4+1] = cur_row.open / open
#             # buf[i*4+2] = cur_row.high / df.high[ind]
#             # buf[i*4+3] = cur_row.low / df.low[ind]
#             # buf[i*4+4] = cur_row.close / df.close[ind]
#             buf[i*4+1] = open
#             buf[i*4+2] = df.high[ind]
#             buf[i*4+3] = df.low[ind]
#             buf[i*4+4] = df.close[ind]
#             @assert !iszero(df.close[ind])
#             @assert all(!iszero, buf[(i*4+1):(i*4+4)]) "all(iszero, buf[(i*4+1):(i*4+4)]): $(buf[(i*4+1):(i*4+4)])"
#             buf_mask[i*4+1] = 1f0
#             buf_mask[i*4+2] = 1f0
#             buf_mask[i*4+3] = 1f0
#             buf_mask[i*4+4] = 1f0
#             # if i*4+4 == 64
#             #     @show i (i*4+4) buf[i*4+4] buf_mask[i*4+4]
#             # end
#         end
#     end
#     if !isnothing(findfirst(!isfinite, buf))
#         error("bad vix data")
#     end
#     if any(!isfinite, buf)
#         error("bad vix data")
#     end
#     if iszero(buf) || iszero(buf_mask)
#         error("vix ERROR: row all zeros for date: $(cur_date)")
#     end
# end
#endregion old make data


#region old old make data
# function times_for_date_to(cfg, date_to)
#     date_from = firstdayofweek(date_to) - Week(cfg.data_weeks_count - 1)
#     date_to = lastdayofweek(date_to) - Day(2)
#     times = DateUtil.all_weekday_ts(; date_from, date_to)

#     global ktimes = times
#     @assert round(times[end] - times[1], Day) == Day((cfg.data_weeks_count - 1) * 7 + 4)
#     # daylight savings time makes it a range
#     base = (cfg.data_weeks_count - 1) * Dates.value(convert(Minute, Week(1))) + 6090
#     @assert base-60 <= Dates.value(convert(Minute, times[end] - times[1])) <= base+60 "$(Dates.value(convert(Minute, times[end] - times[1])))"

#     return times
# end

# function get_inds_under(cfg, df, date, ts_start)
#     ind_start = searchsortedfirst(df.ts, ts_start)
#     ind_end = ind_start + cfg.data_weeks_count * DateUtil.TIMES_PER_WEEK - 1
#     # @assert (ind_end - ind_start) + 1 == DateUtil.TIMES_PER_WEEK * cfg.data_weeks_count
#     inds = ind_start:ind_end
#     # asserts
#     times = times_for_date_to(cfg, date)
#     if df.ts[inds] != times
#         global kget_inds_under = (;cfg, df, date, ts_start, inds, times, dfts=df.ts[inds])
#         error("times didn't match")
#     end
#     return inds
# end

# function make_row_under!(buf, buf_mask, df, targeti, ts_start, ts)
#     cur = df.under[targeti]
#     # week_start = Date(Dates.firstdayofweek(ts))
#     # date_from = week_start - Week(cfg.data_weeks_count - 1)
#     # date_to = week_start + Day(4)
#     # tss = DateUtil.all_weekday_ts(;date_from, date_to)

#     ind_start = searchsortedfirst(df.ts, ts_start)
#     for i in axes(buf, 1)
#         ind = ind_start + i - 1
#         @assert -DateUtil.TIMES_PER_WEEK <= (targeti - ind) < size(buf, 1) "assert i:$(i): $(-DateUtil.TIMES_PER_WEEK) <= $(targeti) - $(ind) ($(targeti - ind)) < $(size(buf, 1))"
#         x = df.under[ind]
#         if ind > targeti || x <= 0f0
#             println("skipping $(ind) > $(targeti) || $(x) <= 0f0")
#             buf[i] = 0f0
#             buf_mask[i] = 0f0
#         else
#             buf[i] = cur / x
#             buf_mask[i] = 1f0
#         end
#     end
#     if !isnothing(findfirst(!isfinite, buf))
#         println("ERROR: bad vix data")
#         @show buf
#         global krowunder = (;buf, buf_mask, df, i, ts_start, ts, ind_start, cur)
#     end
#     if isnothing(findfirst(!iszero, buf)) || isnothing(findfirst(!iszero, buf_mask))
#         println("ERROR: row all zeros")
#         # @show buf
#         global krowunder = (;buf, buf_mask, df, i, ts_start, ts, ind_start, cur)
#     end
# end
#endregion old old
end
