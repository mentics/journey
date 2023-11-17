module HistoricalModel
using Dates, DataFrames
using Flux, MLUtils, CUDA
import NNlib
import DateUtil, IndexUtil
import DataFiles as dat
using ModelUtil, TrainUtil

function trained_model()
    return
end

#region Config
model_hypers() = (;
    data_weeks_count = 4,
    encoded_width = 64,
    block_count = 4,
    layers_per_block = 4,
    hidden_width_mult = 4,
    activation = NNlib.swish,
    use_bias = false,
    # skip_layer = true,
)

train_hypers() = (;
    batch_size = 1024,
)

function config()
    (;data_weeks_count, hidden_width_mult) = model_hypers()
    input_width_under = DateUtil.TIMES_PER_WEEK * data_weeks_count
    vix_count = DateUtil.DAYS_PER_WEEK * data_weeks_count
    input_width_vix = vix_count * 4
    input_width = input_width_under + input_width_vix
    hidden_width_under = hidden_width_mult * input_width_under
    hidden_width_vix = hidden_width_mult * input_width_vix
    hidden_width = hidden_width_mult * input_width
    return (;
        model_hypers()...,
        train_hypers()...,
        vix_count,
        input_width_under, input_width_vix, input_width,
        hidden_width_under, hidden_width_vix, hidden_width,
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
    (;under, under_mask, vix, vix_mask) = batch
    (yhat_under_raw, yhat_vix_raw) = model((under, vix))
    yhat_under = yhat_under_raw .* under_mask
    yhat_vix = yhat_vix_raw .* vix_mask
    return (yhat_under, yhat_vix)
end

function run_encode(model, batch)
    # It's expected that the input is already masked, so no need to multiple it by the mask
    (;under, vix) = batch
    return model.layers.encoder((under, vix))
end

make_loss_func() = function(model, batch)
    (yhat_under, yhat_vix) = run_model(model, batch)

    # l = Flux.mse(yhat_under, under) + Flux.mse(yhat_vix, vix)
    (;under, vix) = batch
    l = Flux.mae(yhat_under, under) + Flux.mae(yhat_vix, vix)
    # global kloss_vals = (;under, under_mask, vix, vix_mask, yhat_under_1, yhat_vix_1, yhat_under, yhat_vix, loss=l)
    return l
end
#endregion MLRun Interface

to_float_mz(x) = ismissing(x) ? 0f0 : Float32(x)

const CACHE_DF_UNDER2 = Ref{DataFrame}()
const CACHE_DF_VIX2 = Ref{DataFrame}()
const CACHE_OBS = Ref{Vector{DateTime}}()

function get_data()
    cfg = config()
    if !isassigned(CACHE_OBS)
        under = dat.ts_allperiods_df()
        dat.convert_cols!(to_float_mz, under, :under)
        CACHE_DF_UNDER2[] = under

        vix = dat.vix_alldates_df()
        dat.convert_cols!(to_float_mz, vix, :open, :high, :low, :close)
        CACHE_DF_VIX2[] = vix

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
    vix = Array{Float32, 2}(undef, sizes.vix...)
    vix_mask = Array{Float32, 2}(undef, sizes.vix...)

    make_batch = function(epochi, batchi)
        # TODO: use random variation instead of epochi?
        starti = epochi + batchi * cfg.batch_size
        for i in 1:cfg.batch_size
            ind = IndexUtil.lfsr_next(lfsr, starti + i)

            make_seq!(bufs_row, dfs, ind)

            under[:,i] .= bufs_row.under
            under_mask[:,i] .= bufs_row.under_mask
            vix[:,i] .= bufs_row.vix
            vix_mask[:,i] .= bufs_row.vix_mask
        end

        return (;under, under_mask, vix, vix_mask)
    end

    get_inds = function(epochi, batchi)
        starti = epochi + batchi * cfg.batch_size
        return map(1:cfg.batch_size) do i
            IndexUtil.lfsr_next(lfsr, starti + i)
        end
    end

    # if preload...
    println("Making all batches...")
    batches = [make_batch(0, i) |> gpu for i in 1:batch_count]
    println(" done.")
    get_batch = (epochi, batchi) -> batches[batchi]
    return (;get_batch, batch_count, cfg.batch_size, get_inds, obs_count)
end

data_sizes(cfg) = (;
    under = (DateUtil.TIMES_PER_WEEK * cfg.data_weeks_count, cfg.batch_size),
    vix = (4 * DateUtil.DAYS_PER_WEEK * cfg.data_weeks_count, cfg.batch_size)
)

function make_bufs_row(sizes)
    under_len, vix_len = sizes.under[1], sizes.vix[1]
    return (;
        under = Vector{Float32}(undef, under_len),
        under_mask = Vector{Float32}(undef, under_len),
        vix = Vector{Float32}(undef, vix_len),
        vix_mask = Vector{Float32}(undef, vix_len)
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

    make_row_under!(bufs, under, inds, cur_ind)
    make_row_vix!(bufs, vix, Date(from_ts), Date(cur_ts))
    return inds
end

function test_seq(ind)
    cfg = config()
    dfs = get_data()
    bufs = make_bufs_row(sizes(cfg))
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

function make_row_under!(bufs, df, inds, cur_ind)
    buf, buf_mask = (;under, under_mask) = bufs

    cur = df.under[cur_ind]
    @assert cur != 0f0
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
    if !isnothing(findfirst(!isfinite, buf))
        println("ERROR: bad vix data")
        @show buf
        global krowunder = (;buf, buf_mask, df, i, ts_start, ts, ind_start, cur)
    end
    if isnothing(findfirst(!iszero, buf)) || isnothing(findfirst(!iszero, buf_mask))
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

function make_row_vix!(bufs, df, date_from, date_lessthan)
    buf, buf_mask = bufs.vix, bufs.vix_mask

    ind_start = searchsortedfirst(df.date, date_from)
    cur = df[ind_start,:]
    for i in 0:(size(buf, 1) ÷ 4 - 1)
        ind = ind_start + i
        open = df.open[ind]
        if ismissing(open) || open <= 0f0 || df.date[ind] >= date_lessthan
            buf[i*4+1] = 0f0
            buf[i*4+2] = 0f0
            buf[i*4+3] = 0f0
            buf[i*4+4] = 0f0
            buf_mask[i*4+1] = 0f0
            buf_mask[i*4+2] = 0f0
            buf_mask[i*4+3] = 0f0
            buf_mask[i*4+4] = 0f0
        else
            buf[i*4+1] = cur.open / open
            buf[i*4+2] = cur.high / df.high[ind]
            buf[i*4+3] = cur.low / df.low[ind]
            buf[i*4+4] = cur.close / df.close[ind]
            buf_mask[i*4+1] = 1f0
            buf_mask[i*4+2] = 1f0
            buf_mask[i*4+3] = 1f0
            buf_mask[i*4+4] = 1f0
        end
    end
    if !isnothing(findfirst(!isfinite, buf))
        println("ERROR: bad vix data")
        @show buf
        global krowvix = buf
    end
end

function model_encoder(cfg)
    input_under = Dense(cfg.input_width_under => cfg.hidden_width_under; bias=cfg.use_bias)
    input_vix = Dense(cfg.input_width_vix => cfg.hidden_width_vix; bias=cfg.use_bias)
    layer_input = Parallel(vcat; input_under, input_vix)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix
    blocks = [SkipConnection(make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:cfg.block_count]
    layer_output = Dense(through_width => cfg.encoded_width; bias=false)
    return Chain(;encoder_input=layer_input, encoder_blocks=Chain(blocks...), encoder_output=layer_output)
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

function model_decoder(cfg)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix
    layer_input = Dense(cfg.encoded_width => through_width, cfg.activation; bias=false)
    blocks = [SkipConnection(make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:cfg.block_count]
    output_under = Dense(cfg.hidden_width_under => cfg.input_width_under; bias=cfg.use_bias)
    output_vix = Dense(cfg.hidden_width_vix => cfg.input_width_vix; bias=cfg.use_bias)
    layer_output = SplitLayer((output_under, output_vix), cfg.hidden_width_under)
    # layer_output = in -> (output_under(in[1:cfg.hidden_width_under,:]), output_vix(in[cfg.hidden_width_under+1:cfg.hidden_width]))
    return Chain(decoder_input=layer_input, decoder_blocks=Chain(blocks...), decoder_output=layer_output)
end

end