module HistoricalModel
using Flux, MLUtils, CUDA
import Dates:Dates,Date
import NNlib
import DateUtil, IndexUtil
import DataFiles as dat
import MLRun

const TIMES_PER_DAY = 14
const DAYS_PER_WEEK = 5
const TIMES_PER_WEEK = TIMES_PER_DAY * DAYS_PER_WEEK

#region MLRun Interface
function config_all end

# MLRun.make_data(m::typeof(@__MODULE__)) = println("it worked! $(m)")

MLRun.make_data(config::typeof(config_all)) = _make_data(config())

function MLRun.make_model(config::typeof(config_all))
    cfg = config()
    return Chain(; encoder=model_encoder(cfg), decoder=model_decoder(cfg))
end

MLRun.make_loss_func(config::typeof(config_all)) = function(model, batch)
    (;under, under_mask, vix, vix_mask) = batch
    (yhat_under_1, yhat_vix_1) = model((under, vix))
    yhat_under = yhat_under_1 .* under_mask
    yhat_vix = yhat_vix_1 .* vix_mask
    l = Flux.mse(yhat_under, under) + Flux.mse(yhat_vix, vix)
    global kloss_vals = (;under, under_mask, vix, vix_mask, yhat_under_1, yhat_vix_1, yhat_under, yhat_vix, loss=l)
    return l
end
#endregion MLRun Interface

#region Config
structure() = (;
    data_weeks_count = 8,
    encoded_width = 128,
    block_count = 2,
    layers_per_block = 4,
    hidden_width_mult = 2,
    activation = NNlib.swish,
    # skip_layer = true,
    batch_size = 64,
)

lr_const(_...) = 1e-3

hypers() = (;
    learning_rate_func = lr_const,
)

function config_all()
    (;data_weeks_count, hidden_width_mult) = structure()
    input_width_under = TIMES_PER_WEEK * data_weeks_count
    vix_count = DAYS_PER_WEEK * data_weeks_count
    input_width_vix = vix_count * 4
    input_width = input_width_under + input_width_vix
    hidden_width_under = hidden_width_mult * input_width_under
    hidden_width_vix = hidden_width_mult * input_width_vix
    hidden_width = hidden_width_mult * input_width
    return (;
        structure()...,
        hypers()...,
        vix_count,
        input_width_under, input_width_vix, input_width,
        hidden_width_under, hidden_width_vix, hidden_width,
    )
end
#endregion Config

#region Types
struct SplitLayer{L,W}
    layers::L
    split::W
end
Flux.@functor SplitLayer

function (m::SplitLayer)(x)
    return (
        m.layers[1](x[1:m.split,:]),
        m.layers[2](x[(m.split+1):end,:])
    )
end
#endregion

to_float_mz(x) = ismissing(x) ? 0f0 : Float32(x)

function _make_data(cfg)
    df_under = dat.ts_allperiods_df()
    dat.convert_cols!(to_float_mz, df_under, :under)
    df_vix = dat.vix_alldates_df()
    dat.convert_cols!(to_float_mz, df_vix, :open, :high, :low, :close)
    batch_count = (size(df_under, 1) - cfg.input_width_under) ÷ cfg.batch_size

    offset_under = DAYS_PER_WEEK * TIMES_PER_DAY * cfg.data_weeks_count

    lfsr = IndexUtil.lfsr(size(df_under, 1) - offset_under)

    buf_under_row = Vector{Float32}(undef, DAYS_PER_WEEK * TIMES_PER_DAY * cfg.data_weeks_count)
    buf_under_mask_row = Vector{Float32}(undef, DAYS_PER_WEEK * TIMES_PER_DAY * cfg.data_weeks_count)
    buf_vix_row = Vector{Float32}(undef, 4 * DAYS_PER_WEEK * cfg.data_weeks_count)
    buf_vix_mask_row = Vector{Float32}(undef, 4 * DAYS_PER_WEEK * cfg.data_weeks_count)

    under = Array{Float32, 2}(undef, DAYS_PER_WEEK * TIMES_PER_DAY * cfg.data_weeks_count, cfg.batch_size)
    under_mask = Array{Float32, 2}(undef, DAYS_PER_WEEK * TIMES_PER_DAY * cfg.data_weeks_count, cfg.batch_size)
    vix = Array{Float32, 2}(undef, 4 * DAYS_PER_WEEK * cfg.data_weeks_count, cfg.batch_size)
    vix_mask = Array{Float32, 2}(undef, 4 * DAYS_PER_WEEK * cfg.data_weeks_count, cfg.batch_size)

    get_batch = function(epochi, batchi)
        for i in 1:cfg.batch_size
            # TODO: use random variation instead of epochi?
            ind = IndexUtil.lfsr_next(lfsr, epochi + batchi * cfg.batch_size + i)
            ts = df_under.ts[ind]
            ts_start = DateUtil.week_start_market(ts) - Dates.Week(cfg.data_weeks_count - 1)
            make_row_under!(buf_under_row, buf_under_mask_row, df_under, ind, ts_start, ts)
            make_row_vix!(buf_vix_row, buf_vix_mask_row, df_vix, Date(ts_start), Date(ts))
            under[:,i] .= buf_under_row
            under_mask[:,i] .= buf_under_mask_row
            vix[:,i] .= buf_vix_row
            vix_mask[:,i] .= buf_vix_mask_row
        end

        return (;under, under_mask, vix, vix_mask)
    end
    return (;get_batch, batch_count, cfg.batch_size)
end

function make_row_under!(buf, buf_mask, df, i, ts_start, ts)
    cur = df.under[i]
    # week_start = Date(Dates.firstdayofweek(ts))
    # date_from = week_start - Week(cfg.data_weeks_count - 1)
    # date_to = week_start + Day(4)
    # tss = DateUtil.all_weekday_ts(;date_from, date_to)

    ind_start = searchsortedfirst(df.ts, ts_start)
    for i in axes(buf, 1)
        ind = ind_start + i - 1
        x = df.under[ind]
        if ind > i || x <= 0f0
            buf[i] = 0f0
            buf_mask[i] = 0f0
        else
            buf[i] = cur / x
            buf_mask[i] = 1f0
        end
    end
    if !isnothing(findfirst(!isfinite, buf))
        println("ERROR: bad vix data")
        @show buf
        global krowunder = buf
    end
end

function make_row_vix!(buf, buf_mask, df, date_from, date_before)
    ind_start = searchsortedfirst(df.date, date_from)
    cur = df[ind_start,:]
    for i in 0:(size(buf, 1) ÷ 4 - 1)
        ind = ind_start + i
        open = df.open[ind]
        if ismissing(open) || open <= 0f0 || df.date[ind] >= date_before
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
    input_under = Dense(cfg.input_width_under => cfg.hidden_width_under; bias=true)
    input_vix = Dense(cfg.input_width_vix => cfg.hidden_width_vix; bias=true)
    layer_input = Parallel(vcat; input_under, input_vix)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix
    blocks = [SkipConnection(make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:cfg.block_count]
    layer_output = Dense(through_width => cfg.encoded_width)
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
    output_under = Dense(cfg.hidden_width_under => cfg.input_width_under; bias=true)
    output_vix = Dense(cfg.hidden_width_vix => cfg.input_width_vix; bias=true)
    layer_output = SplitLayer((output_under, output_vix), cfg.hidden_width_under)
    # layer_output = in -> (output_under(in[1:cfg.hidden_width_under,:]), output_vix(in[cfg.hidden_width_under+1:cfg.hidden_width]))
    return Chain(decoder_input=layer_input, decoder_blocks=Chain(blocks...), decoder_output=layer_output)
end

end