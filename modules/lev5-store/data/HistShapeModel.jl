module HistShapeModel
using Intervals, Dates, StatsBase, DataFrames, Base64
using Flux, NNlib, MLUtils
using DateUtil, Paths
using ModelUtil, TrainUtil, MLTrain
import HistShapeData as hsd
import CudaUtil

const NAME = replace(string(@__MODULE__), "Model" => "")

params_model() = (;
    encoded_width = 32,
    block_count = 2,
    layers_per_block = 2,
    hidden_width_mult = 2,
    dropout = 0f0,
    activation = NNlib.swish,
    use_bias = false,
    batchnorm = false,
)

#region MLTrain Interface
function make_trainee(params_m=params_model())
    df, params_data = Paths.load_data_params(Paths.db_input(NAME), DataFrame)
    # input_width = size(df, 2) - 3 # 2 key cols and a y col
    params = (;data=params_data, model=params_m)
    batchnorm = params_m.batchnorm

    global state = Trainee(;
        name=NAME,
        version=ModelUtil.encode_version(1, params),
        make_model = () -> make_model(params; batchnorm),
        get_inference_model,
        run_train = (m, batch) -> run_train(m, batch; batchnorm),
        run_infer = (m, batch) -> run_infer(m, batch; batchnorm),
        prep_data = params_train -> make_data(df, merge(params, (;train=params_train))),
        get_learning_rate = TrainUtil.learning_rate_linear_decay(), # TrainUtil.lr_cycle_decay(),
        get_loss = batchnorm ? calc_loss_batchnorm : calc_loss,
        params,
        mod = @__MODULE__
    )
    return state
end

# to_draw_x(batch, ind) = (batch.ts, batch.prices_seq[1:end,ind])
# to_draw_y(batch, ind) = (batch.ts, batch.prices_seq[1:end,ind])
# to_draw_yh(batch, yhat, ind) = (batch.ts, yhat.prices[1:end,ind])

# to_draw_x(batch, ind) = batch.prices_seq[1:end,ind]
# to_draw_y(batch, ind) = batch.prices_seq[1:end,ind]
# to_draw_yh(yhat, ind) = yhat.prices[1:end,ind]

# to_draw_x(batch, ind) = batch.vix_seq[1:end,ind]
# to_draw_y(batch, ind) = batch.vix_seq[1:end,ind]
# to_draw_yh(yhat, ind) = yhat.vix[1:end,ind]

to_draw_x(batch, ind) = vcat(batch.prices_seq[1:end,ind], batch.vix_seq[1:end,ind])
to_draw_y(batch, ind) = vcat(batch.prices_seq[1:end,ind], batch.vix_seq[1:end,ind])
to_draw_yh(yhat, ind) = vcat(yhat.prices[1:end,ind], yhat.vix[1:end,ind])

# function to_draw_y(model, batch, ind)
#     y_prices, y_vix = y_batchnorm(model, batch)
#     vcat(y_prices[1:end,ind], y_vix[1:end,ind])
# end
# to_draw_yh(yhat, ind) = vcat(yhat.prices[1:end,ind], yhat.vix[1:end,ind])

# to_draw_y(batch, ind) = (batch.ts, batch.y[:,ind])
# to_draw_yh(yhat, ind) = (bins(), softmax(yhat[:,ind]))

# to_draw_x(batch, ind) = batch.vix.v[1:end,ind]
# to_draw_yh(yhat, ind) = yhat.vix[1:end,ind]
#endregion MLTrain Interface

#region Data
# date_range_train() = (;date_range = DateUtil.DEFAULT_DATA_START_DATE..Date(2023,6,30))
# date_range_train() = (;date_range = Date(2016,1,1)..Date(2023,6,30))
function make_data(df, params)
    # Hold out some of the most recent data so we can backtest on untrained data
    df_train = filter(:ts => DateUtil.ts_in(params.data.train_date_range), df)
    df_holdout = filter(:ts => (ts -> ts >= last(params.data.train_date_range) + Day(1)), df)
    @assert df_holdout.ts[1] > df_train.ts[end] "df_holdout.ts[1] $(df_holdout.ts[1]) > $(df_train.ts[end]) df_train.ts[end]"

    InputData(;
        all_data = df,
        data_for_epoch = () -> shuffleobs(df_train),
        prep_input = (obss, args...) -> prep_input(obss, args...),
        get_input_keys,
        holdout=df_holdout,
        single = ind -> single(df, ind)
    )
end

# Can call on input batch after prep_input
function get_input_keys(batch)
    return batch.ts
end
get_input_keys() = [:ts]

single(df, ind) = prep_input(df[ind:ind,:])

# function prep_input(obss)
#     # dataframe of batch_size rows of same columns as input data
#     # need to make matrices that can be pushed to gpu and used in model
#     # TODO: find better way
#     data = map(eachcol(obss)) do col
#         reduce(hcat, col)
#     end
#     return NamedTuple{Tuple(Symbol.(names(obss)))}(data)
# end

#=
Note: I thought to use copyto!(dest, doff, src, soff, n) to copy directly from these vectors into GPU buffers, but...
That will work for normal arrays, but at this point, they're still Arrow primitive arrays, so doesn't work.
=#
get_lengths(obss) = (;
    prices_len = length(obss.prices_seq[1]),
    vix_len = length(obss.vix_seq[1]),
    batch_size = size(obss, 1),
)
make_buffers(obss) = make_buffers(get_lengths(obss)...)
import Flux
function make_buffers(prices_len, vix_len, batch_size)
    cpu = (;
        prices_seq = Matrix{Float32}(undef, prices_len, batch_size),
        # Note: Using a BitArray makes it very slow to copy data to the GPU.
        # prices_mask = BitArray(undef, prices_len, batch_size),
        prices_mask = Matrix{Bool}(undef, prices_len, batch_size),
        vix_seq = Matrix{Float32}(undef, vix_len, batch_size),
        # vix_mask = BitArray(undef, vix_len, batch_size),
        vix_mask = Matrix{Bool}(undef, vix_len, batch_size),
    )
    gpu = Flux.gpu(cpu)
    return (;cpu, gpu)
end

function prep_input(obss)
    prep_input(obss, make_buffers(get_lengths(obss)...))
end
function prep_input(obss, bufs_2)
    bufs = bufs_2.cpu
    # dataframe of batch_size rows of same columns as input data
    # need to make matrices that can be pushed to gpu and used in model
    for i in eachindex(obss.prices_seq)
        bufs.prices_seq[:,i] .= obss.prices_seq[i]
    end
    for i in eachindex(obss.prices_mask)
        bufs.prices_mask[:,i] .= obss.prices_mask[i]
    end
    for i in eachindex(obss.vix_seq)
        bufs.vix_seq[:,i] .= obss.vix_seq[i]
    end
    for i in eachindex(obss.vix_mask)
        bufs.vix_mask[:,i] .= obss.vix_mask[i]
    end
    gbufs = CudaUtil.copyto_itr!(bufs_2.gpu, bufs)
    return (;keys = (;obss.ts), x=gbufs, y=gbufs)
end

# function gen_output_all(model, data, batch_size)
#     x = Matrix{Float32}(undef, feature_count, pt.batch_size)
#     xgpu = CuArray{Float32}(undef, feature_count, pt.batch_size)
#     df = mapreduce(vcat, eachobs(data.all_data, batchsize=pt.batch_size)) do obss
#         if size(obss, 1) == pt.batch_size
#             batch = data.prep_input(obss, x)
#             copyto!(xgpu, batch.x)
#             output = trainee.run_infer(model, xgpu) |> cpu
#             return DataFrame((;batch.keys..., output=eachcol(output)))
#         else
#             # Handle the last batch which is likely a different size
#             batch = data.prep_input(obss)
#             output = trainee.run_infer(model, batch.x |> gpu) |> cpu
#             return DataFrame((;batch.keys..., output=eachcol(output)))
#         end
#     end



#     global kargs = (;obss, m)
#     error
#     # dataframe of batch_size rows of same columns as input data
#     # need to make matrices that can be pushed to gpu and used in model

#     @inbounds for i in eachindex(cols)
#         x[i,:] .= cols[i]
#     end

#     # TODO: find better way
#     data = map(eachcol(obss)) do col
#         reduce(hcat, col)
#     end
#     x = (;obss.prices_seq, obss.prices_mask, obss.vix_seq, obss.vix_mask)
#     return (;keys, x, y=x)
# end
#endregion Data

#region Run
function calc_loss(model, batch)
    (;prices, vix) = run_train(model, batch.x)
    return Flux.mse(prices, batch.x.prices_seq) + Flux.mse(vix, batch.x.vix_seq)
end
function calc_loss_batchnorm(model, batch)
    (;prices, vix) = run_train(model, batch; batchnorm=true)
    # under_norm = model.layers.encoder.layers.encoder_input.layers.input_under.layers.input_norm_under.layers.input_under_norm
    # vix_norm = model.layers.encoder.layers.encoder_input.layers.input_vix.layers.input_norm_vix.layers.input_vix_norm
    # prices_y = under_norm(batch.prices_seq)
    # vix_y = vix_norm(batch.vix_seq)
    y_prices, y_vix = y_batchnorm(model, batch)
    # return Flux.mse(prices_h, batch.prices_seq) + Flux.mse(vix_h, batch.vix_seq)
    return Flux.mse(prices, y_prices) + Flux.mse(vix, y_vix)
end

function y_batchnorm(model, batch)
    under_norm = model.layers.encoder.layers.encoder_input.layers.input_under.layers.input_norm_under.layers.input_under_norm
    vix_norm = model.layers.encoder.layers.encoder_input.layers.input_vix.layers.input_norm_vix.layers.input_vix_norm
    y_prices = under_norm(batch.prices_seq)
    y_vix = vix_norm(batch.vix_seq)
    return y_prices, y_vix
end

function run_train(model, batchx; batchnorm=false)
    encoded = run_encoder(model.layers.encoder, batchx; batchnorm)
    return run_decoder(model.layers.decoder, encoded, batchx)
end

function run_infer(encoder, batchx; batchnorm=false)
    return run_encoder(encoder, batchx; batchnorm)
end

function run_encoder(encoder, batchx; batchnorm=false)
    # It's expected that the input is already masked, so no need to multiply it by the mask
    if batchnorm
        v = encoder(((batchx.prices_seq, batchx.prices_meta), (batchx.vix_seq, batchx.vix_meta)))
        return vcat(v, batchx.prices_meta, batchx.vix_meta)
    else
        # v = encoder((vcat(batch.prices_seq, batch.prices_meta), vcat(batch.vix_seq, batch.vix_meta)))
        v = encoder((batchx.prices_seq, batchx.vix_seq))
        return v
    end
end

function run_decoder(decoder, encoded, batchx)
    (dec_prices_raw, dec_vix_raw) = decoder(encoded)
    prices = dec_prices_raw .* batchx.prices_mask
    vix = dec_vix_raw .* batchx.vix_mask
    return (;prices, vix)
end
#endregion Run

#region Model
function make_model(params; batchnorm=false)
    cfg = config(params)
    if batchnorm
        return Chain(; encoder=model_encoder_batchnorm(cfg), decoder=model_decoder_batchnorm(cfg))
    else
        return Chain(; encoder=model_encoder(cfg), decoder=model_decoder(cfg))
    end
end

function config(params)
    (;hidden_width_mult, encoded_width) = params.model
    input_width_under = hsd.prices_seq_len(params.data)
    input_width_vix = hsd.vix_seq_len(params.data)
    input_width_meta = 2
    input_width = input_width_under + input_width_vix
    hidden_width_under = hidden_width_mult * input_width_under
    hidden_width_vix = hidden_width_mult * input_width_vix
    hidden_width = hidden_width_mult * input_width
    encoded_with_meta = encoded_width + 4
    return (;
        params.data...,
        params.model...,
        input_width_under, input_width_vix,
        input_width_meta,
        hidden_width_under, hidden_width_vix, hidden_width,
        encoded_with_meta,
    )
end

function model_encoder(cfg)
    input_under = Dense(cfg.input_width_under => cfg.hidden_width_under; bias=cfg.use_bias)
    input_vix = Dense(cfg.input_width_vix => cfg.hidden_width_vix; bias=cfg.use_bias)
    layer_input = Parallel(vcat; input_under, input_vix)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks1_count]
    dropout = Dropout(cfg.dropout)
    blocks2 = [SkipConnection(ModelUtil.make_block(through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks2_count]

    layer_output = Dense(through_width => cfg.encoded_width; bias=false)
    return Chain(;encoder_input=layer_input,
            encoder_blocks=Chain(blocks1..., dropout, blocks2...),
            encoder_output=layer_output)
end

function model_decoder(cfg)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix
    layer_input = Dense(cfg.encoded_width => through_width, cfg.activation; bias=false)

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks1_count]
    blocks2 = [SkipConnection(ModelUtil.make_block(through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks2_count]

    output_under = Dense(cfg.hidden_width_under => cfg.input_width_under; bias=cfg.use_bias)
    output_vix = Dense(cfg.hidden_width_vix => cfg.input_width_vix; bias=cfg.use_bias)
    layer_output = SplitLayer((output_under, output_vix), cfg.hidden_width_under)
    return Chain(;decoder_input=layer_input,
            decoder_blocks=Chain(blocks1..., blocks2...),
            decoder_output=layer_output)
end

#region BatchNormed
function model_encoder_batchnorm(cfg)
    input_norm_under = Parallel(vcat; input_under_norm=BatchNorm(cfg.input_width_under), input_under_meta_norm=BatchNorm(cfg.input_width_meta))
    input_norm_vix = Parallel(vcat; input_vix_norm=BatchNorm(cfg.input_width_vix), input_vix_meta_norm=BatchNorm(cfg.input_width_meta))
    input_under = Chain(;input_norm_under, input_under1=Dense(cfg.input_width_under + cfg.input_width_meta => cfg.hidden_width_under; bias=cfg.use_bias))
    input_vix = Chain(;input_norm_vix, input_vix1=Dense(cfg.input_width_vix + cfg.input_width_meta => cfg.hidden_width_vix; bias=cfg.use_bias))
    layer_input = Parallel(vcat; input_under, input_vix)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks1_count]
    # dropout = Dropout(cfg.dropout)
    blocks2 = [SkipConnection(ModelUtil.make_block(through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks2_count]

    layer_output = Dense(through_width => cfg.encoded_width; bias=false)
    return Chain(;encoder_input=layer_input,
            norm1=BatchNorm(through_width),
            # encoder_blocks=Chain(blocks1..., dropout, blocks2...),
            encoder_blocks=Chain(blocks1..., BatchNorm(through_width), blocks2...),
            norm2=BatchNorm(through_width),
            encoder_output=layer_output)
end

function model_decoder_batchnorm(cfg)
    through_width = cfg.hidden_width_under + cfg.hidden_width_vix
    layer_input = Dense(cfg.encoded_with_meta => through_width, cfg.activation; bias=false)

    # blocks = [SkipConnection(ModelUtil.make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:cfg.block_count]

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks1_count]
    # dropout = Dropout(0.1)
    blocks2 = [SkipConnection(ModelUtil.make_block(through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks2_count]

    output_under = Chain(Dense(cfg.hidden_width_under => cfg.input_width_under; bias=cfg.use_bias), BatchNorm(cfg.input_width_under))
    output_vix = Chain(Dense(cfg.hidden_width_vix => cfg.input_width_vix; bias=cfg.use_bias), BatchNorm(cfg.input_width_vix))
    layer_output = SplitLayer((output_under, output_vix), cfg.hidden_width_under)
    # layer_output = in -> (output_under(in[1:cfg.hidden_width_under,:]), output_vix(in[cfg.hidden_width_under+1:cfg.hidden_width]))
    return Chain(;decoder_input=layer_input,
            norm1=BatchNorm(through_width),
            decoder_blocks=Chain(blocks1..., BatchNorm(through_width), blocks2...),
            norm2=BatchNorm(through_width),
            decoder_output=layer_output)
end
#endregion BatchNormed

function get_inference_model(model)
    return model.layers.encoder
end
#endregion Model

end