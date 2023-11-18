module HistoricalModel
using Dates, DataFrames
using StatsBase
using Flux, MLUtils, CUDA
import NNlib
using DateUtil, CollUtil, IndexUtil
import DataFiles as dat
import DataFiles:missing_to_zero_float
using ModelUtil, TrainUtil

function trained_model()
    return
end

#region Config
model_hypers() = (;
    version = "latent32mult2drop1zp",
    data_weeks_count = 5,
    encoded_width = 32,
    block_count = 2,
    layers_per_block = 2,
    hidden_width_mult = 2,
    activation = NNlib.swish,
    use_bias = false,
    # skip_layer = true,
)

train_hypers() = (;
    batch_size = 128,
)

function config()
    (;data_weeks_count, hidden_width_mult, encoded_width) = model_hypers()
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
        model_hypers()...,
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
make_data() = _make_data(config())

function make_model()
    cfg = config()
    return Chain(; encoder=model_encoder(cfg), decoder=model_decoder(cfg))
end

learning_rate_func = TrainUtil.learning_rate_linear_decay()

function run_model(model, batch)
    encoded = run_encode(model, batch)
    decoded = run_decode(model, encoded)
    return decoded

    # (;under, under_mask, under_meta, vix, vix_mask, vix_meta) = batch
    # (yhat_under_raw, yhat_vix_raw) = model((vcat(under, under_meta), vcat(vix, vix_meta)))
    # yhat_under = vcat(yhat_under_raw .* under_mask, under_meta)
    # yhat_vix = vcat(yhat_vix_raw .* vix_mask, vix_meta)
    # return (vcat(yhat_under, under_meta), vcat(yhat_vix, vix_meta))
end

function run_encode(model, batch)
    # It's expected that the input is already masked, so no need to multiply it by the mask
    encoded = model.layers.encoder((vcat(batch.under.v, batch.under.meta), vcat(batch.vix.v, batch.vix.meta)))
    return (;v=encoded, batch)
        # under=(;batch.under..., v=enc_under),
        # vix=(;batch.vix..., v=enc_vix)
        # under=(;v=enc_under, meta=batch.under.meta, mask=batch.under.mask),
        # vix=(;v=enc_vix, meta=batch.vix.meta, mask=batch.vix.mask)
    # )
end

function run_decode(model, encoded)
    (dec_under_raw, dec_vix_raw) = model.layers.decoder(vcat(encoded.v, encoded.batch.under.meta, encoded.batch.vix.meta))
    dec_under = dec_under_raw .* encoded.batch.under.mask
    dec_vix = dec_vix_raw .* encoded.batch.vix.mask
    return (;
        under=(;encoded.batch.under..., v=dec_under),
        vix=(;encoded.batch.vix..., v=dec_vix)
    )
end

make_loss_func() = function(model, batch)
    decoded = run_model(model, batch)
    # l = Flux.mae(decoded.under.v, under) + Flux.mae(decoded.vix.v, vix)
    l = Flux.mse(decoded.under.v, batch.under.v) + Flux.mae(decoded.vix.v, batch.vix.v)
    # global kloss_vals = (;under, under_mask, vix, vix_mask, yhat_under_1, yhat_vix_1, yhat_under, yhat_vix, loss=l)
    return l
end

# to_draw_x(batch, ind) = batch.under[1:end-2,ind]
# to_draw_yh(yhat, ind) = yhat[1][1:end-2,ind]
to_draw_x(batch, ind) = batch.under.v[1:end,ind]
to_draw_yh(yhat, ind) = yhat.under.v[1:end,ind]
#endregion MLRun Interface

const CACHE_DF_UNDER2 = Ref{DataFrame}()
const CACHE_DF_VIX2 = Ref{DataFrame}()
const CACHE_OBS = Ref{Vector{DateTime}}()

function get_data()
    cfg = config()
    if !isassigned(CACHE_OBS)
        under = dat.ts_allperiods_df()
        dat.convert_cols!(missing_to_zero_float, under, :under)
        CACHE_DF_UNDER2[] = under

        vix = dat.vix_alldates_df()
        dat.convert_cols!(missing_to_zero_float, vix, :open, :high, :low, :close)
        CACHE_DF_VIX2[] = vix

        bad_dates = vix.date[findall(iszero, vix.close)]

        skip_back_count = DateUtil.TIMES_PER_WEEK * cfg.data_weeks_count + 1 # +1 for the current ts
        # obs_ts = filter(:under => !ismissing, df_ts).ts[skip_back_count:end]
        obs = filter(:under => !iszero, under).ts[skip_back_count:end]
        CACHE_OBS[] = obs
    end
    return (;
        under = CACHE_DF_UNDER2[],
        vix = CACHE_DF_VIX2[],
        obs = CACHE_OBS[]
    )
end

function _make_data(cfg)
    dfs = get_data()

    obs_count = size(dfs.obs, 1)
    batch_count = (obs_count - cfg.input_width_under) ÷ cfg.batch_size
    lfsr = IndexUtil.lfsr(obs_count)
    sizes = data_sizes(cfg)

    bufs_row = make_bufs_row(sizes)
    under = Array{Float32, 2}(undef, sizes.under...)
    under_mask = Array{Float32, 2}(undef, sizes.under...)
    under_meta = Array{Float32, 2}(undef, sizes.under_meta...)
    vix = Array{Float32, 2}(undef, sizes.vix...)
    vix_mask = Array{Float32, 2}(undef, sizes.vix...)
    vix_meta = Array{Float32, 2}(undef, sizes.vix_meta...)

    make_batch = function(epochi, batchi)
        # TODO: use random variation instead of epochi?
        starti = epochi + batchi * cfg.batch_size
        for i in 1:cfg.batch_size
            ind = IndexUtil.lfsr_next(lfsr, starti + i)

            make_seq!(bufs_row, dfs, ind)

            under[:,i] .= bufs_row.under.v
            under_mask[:,i] .= bufs_row.under.mask
            under_meta[:,i] .= bufs_row.under.meta
            vix[:,i] .= bufs_row.vix.v
            vix_mask[:,i] .= bufs_row.vix.mask
            vix_meta[:,i] .= bufs_row.vix.meta
        end

        return (;
            under = (;v=under, mask=under_mask, meta=under_meta),
            vix = (;v=vix, mask=vix_mask, meta=vix_meta)
        )
    end

    get_inds = function(epochi, batchi)
        starti = epochi + batchi * cfg.batch_size
        return map(1:cfg.batch_size) do i
            IndexUtil.lfsr_next(lfsr, starti + i)
        end
    end

    # if preload...
    println("Making all batches...")
    # batches = [map(copy, make_batch(0, i)) for i in 1:batch_count]
    batches = [deepcopy(make_batch(0, i)) for i in 1:batch_count]
    println(" done.")
    get_batch = (epochi, batchi) -> (batches[batchi] |> gpu)
    return (;get_batch, batch_count, cfg.batch_size, get_inds, obs_count)
end

data_sizes(cfg) = (;
    under = (DateUtil.TIMES_PER_WEEK * cfg.data_weeks_count, cfg.batch_size),
    under_meta = (2, cfg.batch_size),
    vix = (4 * DateUtil.DAYS_PER_WEEK * cfg.data_weeks_count, cfg.batch_size),
    vix_meta = (2, cfg.batch_size)
)

data_sizes_orig(cfg) = (;
    under = (DateUtil.TIMES_PER_WEEK * cfg.data_weeks_count, cfg.batch_size),
    vix = (4 * DateUtil.DAYS_PER_WEEK * cfg.data_weeks_count, cfg.batch_size)
)

function make_bufs_row(sizes)
    under_len, under_meta_len, vix_len, vix_meta_len = sizes.under[1], sizes.under_meta[1], sizes.vix[1], sizes.vix_meta[1]
    return (;
        under = (;
            v = Vector{Float32}(undef, under_len),
            mask = Vector{Float32}(undef, under_len),
            meta = Vector{Float32}(undef, under_meta_len)
        ),
        vix = (;
            v = Vector{Float32}(undef, vix_len),
            mask = Vector{Float32}(undef, vix_len),
            meta = Vector{Float32}(undef, vix_meta_len)
        )
    )
end

function make_seq!(bufs, dfs, ind)
    cfg = config()
    (;under, vix, obs) = dfs
    cur_ts = obs[ind]
    cur_ind = searchsortedfirst(under.ts, cur_ts)
    to_ind = cur_ind - 1
    to_ts = under.ts[to_ind]
    # to_date = Date(to_ts)

    from_ts = DateUtil.week_start_market(to_ts - Week(cfg.data_weeks_count - 1))
    from_ind = searchsortedfirst(under.ts, from_ts)
    end_ind = from_ind + cfg.data_weeks_count * DateUtil.TIMES_PER_WEEK - 1

    inds = from_ind:end_ind

    @assert Week(cfg.data_weeks_count-1) <= round(cur_ts - under.ts[inds[1]], Week) <= Week(cfg.data_weeks_count)

    missing_max = 0.3 * length(inds)
    missing_count = count(iszero, (@view under.under[inds]))
    # println("missing: $(missing_count) / $(missing_max)")
    if missing_count >= missing_max
        println("missing count $(missing_count) > missing max $(missing_max) for ind $(ind). Returning 0 row")
        fill!(bufs.under.v, 0.0)
        fill!(bufs.under.mask, 0.0)
        fill!(bufs.under.meta, 0.0)
        fill!(bufs.vix.v, 0.0)
        fill!(bufs.vix.mask, 0.0)
        fill!(bufs.vix.meta, 0.0)
        return inds
    end

    # make_row_under!(bufs.under, under, inds, cur_ind)
    # make_row_vix!(bufs.vix, vix, Date(from_ts), Date(cur_ts))
    wrap_row!(bufs.under, make_row_under!, under, inds, cur_ind)
    wrap_row!(bufs.vix, make_row_vix!, vix, Date(from_ts), Date(cur_ts))
    return inds
end

function test_seq(ind)
    cfg = config()
    dfs = get_data()
    bufs = make_bufs_row(data_sizes(cfg))
    inds = make_seq!(bufs, dfs, ind)
    tss = dfs.under.ts[inds]
    return (;bufs, inds, tss)
end

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

function wrap_row!(bufs, f, args...)
    # global kwrap_row = (;bufs, f, args)
    # views = (;v=(@view bufs.v[1:end-2]), mask=(@view bufs.mask[1:end-2]))
    f(bufs, args...)
    bufs_orig = (;v=copy(bufs.v), mask=copy(bufs.mask))

    if iszero(bufs.v)
        throw("what!")
    end
    if !are_all_finite(bufs.v)
        error("f returned bad data")
    end

    μ, σ = mean_and_std(filter(!iszero, bufs.v))
    @assert -2f0 < μ < 80f0 "-2f0 < μ ($(μ)) < 80f0"
    @assert 0.001f0 < σ < 32f0 "0.001f0 < σ ($(σ)) < 32f0" # TODO: too low? check which date this is for?
    zscore!(bufs.v, μ, σ)
    # mask_count = count(iszero, views.mask)
    bufs.v .*= bufs.mask
    bufs.meta[1] = μ
    bufs.meta[2] = σ
    # bufs.v[end-1] = μ
    # bufs.v[end] = σ
    # bufs.mask[end-1] = 1f0
    # bufs.mask[end] = 1f0
    @assert are_all_finite(bufs.v)
    # @assert count(iszero, bufs.v) == count(iszero, bufs.mask) "$(f) count(iszero, bufs.v) == count(iszero, bufs.mask): $(count(iszero, bufs.v)) == $(count(iszero, bufs.mask)) ; $(mask_count)"
    if count(iszero, bufs.v) != count(iszero, bufs.mask)
        vz = findall(iszero, bufs.v)
        mz = findall(iszero, bufs.mask)
        inds = setdiff(vz, mz)
        for ind in inds
            if bufs_orig.v[ind] != μ
                println("found zero that wasn't mean")
                @show ind bufs.v[ind] bufs_orig.v[ind] μ
            end
        end
    end
end

function make_row_under!(bufs, df, inds, cur_ind)
    # global kmru_args = (;bufs=map(copy, bufs), df, inds, cur_ind)
    buf, buf_mask = bufs.v, bufs.mask

    cur = df.under[cur_ind]
    @assert 50f0 < cur < 1000f0
    i_buf = 1
    for i_df in inds
        @assert -DateUtil.TIMES_PER_WEEK <= ((cur_ind-1) - inds[1]) < size(buf, 1) "assert i:$(i_df): $(-DateUtil.TIMES_PER_WEEK) <= $(cur_ind) - $(ind) ($(cur_ind - ind)) < $(size(buf, 1))"
        x = df.under[i_df]
        if i_df >= cur_ind || x <= 0f0
            # println("skipping $(dfi) > $(targeti) || $(x) <= 0f0")
            buf[i_buf] = 0f0
            buf_mask[i_buf] = 0f0
        else
            buf[i_buf] = cur / x - 1f0
            buf_mask[i_buf] = 1f0
            # @show cur x buf[i_buf] i_df i_buf cur_ind inds
        end
        i_buf += 1
    end
    if any(!isfinite, buf)
        println("ERROR: bad under data")
        @show buf
        global krowunder = (;buf, buf_mask, df, i, ts_start, ts, ind_start, cur)
    end
    if iszero(buf) || iszero(buf_mask)
        println("ERROR: row all zeros")
        global krowunder = (;buf, buf_mask, df, inds, cur_ind, cur)
    end
end


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

function make_row_vix!(bufs, df, from_date, cur_date)
    # global kmake_row_vix_args = (;bufs, df, from_date, cur_date)
    buf, buf_mask = bufs.v, bufs.mask

    ind_start = searchsortedfirst(df.date, from_date)
    cur_ind = searchsortedfirst(df.date, cur_date)
    cur_row = df[cur_ind,:]
    @assert 2f0 < cur_row.open < 100f0
    for i in 0:(size(buf, 1) ÷ 4 - 1)
        ind = ind_start + i
        open = df.open[ind]
        if ismissing(open) || open <= 0f0 || df.date[ind] >= cur_date
            buf[i*4+1] = 0f0
            buf[i*4+2] = 0f0
            buf[i*4+3] = 0f0
            buf[i*4+4] = 0f0
            buf_mask[i*4+1] = 0f0
            buf_mask[i*4+2] = 0f0
            buf_mask[i*4+3] = 0f0
            buf_mask[i*4+4] = 0f0
        else
            # buf[i*4+1] = cur_row.open / open
            # buf[i*4+2] = cur_row.high / df.high[ind]
            # buf[i*4+3] = cur_row.low / df.low[ind]
            # buf[i*4+4] = cur_row.close / df.close[ind]
            buf[i*4+1] = open
            buf[i*4+2] = df.high[ind]
            buf[i*4+3] = df.low[ind]
            buf[i*4+4] = df.close[ind]
            @assert !iszero(df.close[ind])
            @assert all(!iszero, buf[(i*4+1):(i*4+4)]) "all(iszero, buf[(i*4+1):(i*4+4)]): $(buf[(i*4+1):(i*4+4)])"
            buf_mask[i*4+1] = 1f0
            buf_mask[i*4+2] = 1f0
            buf_mask[i*4+3] = 1f0
            buf_mask[i*4+4] = 1f0
            # if i*4+4 == 64
            #     @show i (i*4+4) buf[i*4+4] buf_mask[i*4+4]
            # end
        end
    end
    if !isnothing(findfirst(!isfinite, buf))
        error("bad vix data")
    end
    if any(!isfinite, buf)
        error("bad vix data")
    end
    if iszero(buf) || iszero(buf_mask)
        error("vix ERROR: row all zeros for date: $(cur_date)")
    end
end

# function model_encoder(cfg)
#     input_under = Dense(cfg.input_width_under => cfg.hidden_width_under; bias=cfg.use_bias)
#     input_vix = Dense(cfg.input_width_vix => cfg.hidden_width_vix; bias=cfg.use_bias)
#     layer_input = Parallel(vcat; input_under, input_vix)
#     through_width = cfg.hidden_width_under + cfg.hidden_width_vix

#     encinner1 = Dense(through_width => 2 * through_width, cfg.activation; bias=false)
#     encinner2 = Dense(2 * through_width => through_width, cfg.activation; bias=false)

#     layer_output = Dense(through_width => cfg.encoded_width; bias=false)
#     return Chain(;encoder_input=layer_input, encinner1, encinner2, encoder_output=layer_output)
# end

# function model_decoder(cfg)
#     through_width = cfg.hidden_width_under + cfg.hidden_width_vix
#     layer_input = Dense(cfg.encoded_width => through_width, cfg.activation; bias=false)

#     decinner1 = Dense(through_width => 2 * through_width, cfg.activation; bias=false)
#     decinner2 = Dense(2 * through_width => through_width, cfg.activation; bias=false)

#     output_under = Dense(cfg.hidden_width_under => cfg.input_width_under; bias=cfg.use_bias)
#     output_vix = Dense(cfg.hidden_width_vix => cfg.input_width_vix; bias=cfg.use_bias)
#     layer_output = SplitLayer((output_under, output_vix), cfg.hidden_width_under)
#     return Chain(;decoder_input=layer_input, decinner1, decinner2, decoder_output=layer_output)
# end

function model_encoder(cfg)
    input_under = Dense(cfg.input_width_under + cfg.input_width_meta => cfg.hidden_width_under; bias=cfg.use_bias)
    input_vix = Dense(cfg.input_width_vix + cfg.input_width_meta => cfg.hidden_width_vix; bias=cfg.use_bias)
    layer_input = Parallel(vcat; input_under, input_vix)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks1_count]
    dropout = Dropout(0.1)
    blocks2 = [SkipConnection(make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks2_count]

    layer_output = Dense(through_width => cfg.encoded_width; bias=false)
    return Chain(;encoder_input=layer_input, encoder_blocks=Chain(blocks1..., dropout, blocks2...), encoder_output=layer_output)
end

function model_decoder(cfg)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix
    layer_input = Dense(cfg.encoded_with_meta => through_width, cfg.activation; bias=false)

    # blocks = [SkipConnection(make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:cfg.block_count]

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks1_count]
    # dropout = Dropout(0.1)
    blocks2 = [SkipConnection(make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks2_count]

    output_under = Dense(cfg.hidden_width_under => cfg.input_width_under; bias=cfg.use_bias)
    output_vix = Dense(cfg.hidden_width_vix => cfg.input_width_vix; bias=cfg.use_bias)
    layer_output = SplitLayer((output_under, output_vix), cfg.hidden_width_under)
    # layer_output = in -> (output_under(in[1:cfg.hidden_width_under,:]), output_vix(in[cfg.hidden_width_under+1:cfg.hidden_width]))
    return Chain(decoder_input=layer_input, decoder_blocks=Chain(blocks1..., blocks2...), decoder_output=layer_output)
end

function make_block(cfg, through_width, hidden_width, num_layers)
    layers = Dense[]
    push!(layers, Dense(through_width => hidden_width, cfg.activation; bias=false))
    for _ in 1:(num_layers-2)
        push!(layers, Dense(hidden_width => hidden_width, cfg.activation; bias=false))
    end
    push!(layers, Dense(hidden_width => through_width, cfg.activation; bias=false))
    return Chain(layers)
end

end
