module OhlcShapeModel
using Intervals, Dates, StatsBase, DataFrames, Base64
using Flux, NNlib, MLUtils
using DateUtil, Paths
using ModelUtil, TrainUtil, MLTrain
import OhlcShapeData as osd
import CudaUtil

const NAME = replace(string(@__MODULE__), "Model" => "")

params_model() = (;
    encoded_width = 32,
    block_count = 2,
    layers_per_block = 2,
    hidden_width_mult = 2,
    dropout = 0f0,
    activation = NNlib.swish,
    use_input_bias = false,
)

#region MLTrain Interface
function make_trainee(params_m=params_model())
    df, params_data = Paths.load_data_params(Paths.db_input(NAME), DataFrame)
    params = (;data=params_data, model=params_m)

    global state = Trainee(;
        name=NAME,
        version=ModelUtil.encode_version(1, params),
        make_model = () -> make_model(params),
        get_inference_model,
        run_train = (m, batch) -> run_train(m, batch),
        run_infer = (m, batch) -> run_infer(m, batch),
        prep_data = params_train -> make_data(df, merge(params, (;train=params_train))),
        get_learning_rate = TrainUtil.learning_rate_linear_decay(), # TrainUtil.lr_cycle_decay(),
        get_loss = calc_loss,
        params,
        mod = @__MODULE__
    )
    return state
end

to_draw_x(batch, ind) = vc(batch.x, ind)
to_draw_y(batch, ind) = vc(batch.y, ind)
to_draw_yh(yhat, ind) = vc(yhat, ind) # vcat(yhat.prices[1:end,ind], yhat.vix[1:end,ind])

vc(x,batchind) = vcat(x.price_seq[:,batchind], x.vix_seq[:,batchind], x.scaling[:,batchind])
#endregion MLTrain Interface

#region Data
function make_data(df, params)
    # Hold out some of the most recent data so we can backtest on untrained data
    cutoff = Date(2023,6,1)
    df_train = filter(:date => (date -> date < cutoff), df)
    df_holdout = filter(:date => (date -> date >= cutoff), df)
    @assert df_holdout.date[1] > df_train.date[end] "df_holdout.date[1] $(df_holdout.date[1]) > $(df_train.date[end]) df_train.date[end]"

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
    return batch.date
end
get_input_keys() = [:date]

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
    price_len = length(obss.price_seq[1]),
    vix_len = length(obss.vix_seq[1]),
    scaling_len = length(obss.scaling[1]),
    batch_size = size(obss, 1),
)
make_buffers(obss) = make_buffers(get_lengths(obss)...)
import Flux
function make_buffers(price_len, vix_len, scaling_len, batch_size)
    cpu = (;
        price_seq = Matrix{Float32}(undef, price_len, batch_size),
        # Note: Using a BitArray makes it very slow to copy data to the GPU.
        # price_mask = BitArray(undef, prices_len, batch_size),
        price_mask = Matrix{Bool}(undef, price_len, batch_size),
        vix_seq = Matrix{Float32}(undef, vix_len, batch_size),
        # vix_mask = BitArray(undef, vix_len, batch_size),
        vix_mask = Matrix{Bool}(undef, vix_len, batch_size),
        scaling = Matrix{Float32}(undef, scaling_len, batch_size),
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
    for i in eachindex(obss.price_seq)
        bufs.price_seq[:,i] .= obss.price_seq[i]
    end
    for i in eachindex(obss.price_mask)
        bufs.price_mask[:,i] .= obss.price_mask[i]
    end
    for i in eachindex(obss.vix_seq)
        bufs.vix_seq[:,i] .= obss.vix_seq[i]
    end
    for i in eachindex(obss.vix_mask)
        bufs.vix_mask[:,i] .= obss.vix_mask[i]
    end
    for i in eachindex(obss.scaling)
        bufs.scaling[:,i] .= obss.scaling[i]
    end
    gbufs = CudaUtil.copyto_itr!(bufs_2.gpu, bufs)
    return (;keys = (;obss.date), x=gbufs, y=gbufs)
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
#     x = (;obss.price_seq, obss.price_mask, obss.vix_seq, obss.vix_mask)
#     return (;keys, x, y=x)
# end
#endregion Data

#region Run
function calc_loss(model, batch)
    (;price_seq, vix_seq, scaling) = run_train(model, batch.x)
    # global kloss = (;price_seq, vix_seq, scaling)
    # any(isnan, price_seq) && error("price has NaN")
    # any(isnan, vix_seq) && error("vix has NaN")
    # any(isnan, scaling) && error("scaling has NaN")
    # ls = Flux.mse(price_seq, batch.x.price_seq) + Flux.mse(vix_seq, batch.x.vix_seq) + Flux.mse(scaling, batch.x.scaling)
    ls = err(price_seq, batch.x.price_seq) + err(vix_seq, batch.x.vix_seq) + err(scaling, batch.x.scaling)
    # if isnan(ls)
    #     error("loss is NaN")
    # end
    return ls
end
function err(v1, v2)
    sum(err.(v1, v2))
end
# err(x1, x2) = (x1 + one(x1)) / (x2 + one(x1)) - one(x1)
err(x1::Number, x2::Number) = abs((x1 + 1f0) / (x2 + 1f0) - 1f0)

function run_train(model, batchx)
    encoded = run_encoder(model.layers.encoder, batchx)
    return run_decoder(model.layers.decoder, encoded, batchx)
end

function run_infer(encoder, batchx)
    return run_encoder(encoder, batchx)
end

function run_encoder(encoder, batchx)
    # It's expected that the input is already masked, so no need to multiply it by the mask
    v = encoder((batchx.price_seq, batchx.vix_seq, batchx.scaling))
    return v
end

function run_decoder(decoder, encoded, batchx)
    (dec_price_raw, dec_vix_raw, scaling) = decoder(encoded)
    price_seq = dec_price_raw .* batchx.price_mask
    vix_seq = dec_vix_raw .* batchx.vix_mask
    # scaling = to_binary.(scaling_raw)
    return (;price_seq, vix_seq, scaling)
end
to_binary(x) = ifelse(x <= 0f0, 0f0, 1f0)
#endregion Run

#region Model
function make_model(params)
    cfg = config(params)
    global kcfg = cfg
    return Chain(; encoder=model_encoder(cfg), decoder=model_decoder(cfg))
end

function config(params)
    (;hidden_width_mult) = params.model
    input_width_price = params.data.widths.price_seq
    input_width_vix = params.data.widths.vix_seq
    input_width_scaling = params.data.widths.scaling
    input_width = input_width_price + input_width_vix + params.data.widths.scaling
    width1_price = hidden_width_mult * input_width_price
    width1_vix = hidden_width_mult * input_width_vix
    through_width = width1_price + width1_vix + params.data.widths.scaling
    hidden_width = hidden_width_mult * input_width
    return (;
        params.data...,
        params.model...,
        input_width_price, input_width_vix, input_width_scaling,
        width1_price, width1_vix,
        through_width, hidden_width,
    )
end

function model_encoder(cfg)
    input_price = Dense(cfg.input_width_price => cfg.width1_price; bias=cfg.use_input_bias)
    input_vix = Dense(cfg.input_width_vix => cfg.width1_vix; bias=cfg.use_input_bias)
    input_scaling = Dense(cfg.input_width_scaling => cfg.input_width_scaling; bias=cfg.use_input_bias)
    layer_input = Parallel(vcat; input_price, input_vix, input_scaling)

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(cfg.through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, false), +) for _ in 1:blocks1_count]
    dropout = Dropout(cfg.dropout)
    blocks2 = [SkipConnection(ModelUtil.make_block(cfg.through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, false), +) for _ in 1:blocks2_count]

    layer_output = Dense(cfg.through_width => cfg.encoded_width; bias=false)
    return Chain(;encoder_input=layer_input,
            encoder_blocks=Chain(blocks1..., dropout, blocks2...),
            encoder_output=layer_output)
end

function model_decoder(cfg)
    layer_input = Dense(cfg.encoded_width => cfg.through_width, cfg.activation; bias=false)

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(cfg.through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, false), +) for _ in 1:blocks1_count]
    blocks2 = [SkipConnection(ModelUtil.make_block(cfg.through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, false), +) for _ in 1:blocks2_count]

    output_price = Dense(cfg.width1_price => cfg.input_width_price; bias=cfg.use_input_bias)
    output_vix = Dense(cfg.width1_vix => cfg.input_width_vix; bias=cfg.use_input_bias)
    output_scaling = Dense(cfg.input_width_scaling => cfg.input_width_scaling; bias=cfg.use_input_bias)
    s1 = cfg.width1_price
    s2 = cfg.width1_price + cfg.width1_vix
    s3 = cfg.width1_price + cfg.width1_vix + cfg.input_width_scaling
    layer_output = RangesLayer(
        (output_price, output_vix, output_scaling),
        (1:s1, (s1+1):s2,  (s2+1):s3)
    )
    return Chain(;decoder_input=layer_input,
            decoder_blocks=Chain(blocks1..., blocks2...),
            decoder_output=layer_output)
end

function get_inference_model(model)
    return model.layers.encoder
end
#endregion Model

end