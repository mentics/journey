module OhlcShapeModel
using Intervals, Dates, StatsBase, DataFrames, Base64
using Flux, NNlib, MLUtils
using DateUtil, Paths
using ModelUtil, TrainUtil, MLTrain
import OhlcShapeData as osd
import CudaUtil

const NAME = replace(string(@__MODULE__), "Model" => "")

params_model() = (;
    encoded_width = 64,
    block_count = 4,
    layers_per_block = 4,
    hidden_width_mult = 2,
    through_width_mult = 1.5,
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

to_draw_x(batch, ind) = batch.x.seq[:,ind]
to_draw_y(batch, ind) = batch.y.seq[:,ind]
to_draw_yh(yhat, ind) = yhat[:,ind] # vcat(yhat.prices[1:end,ind], yhat.vix[1:end,ind])

# vc(x,batchind) = vcat(x.price_seq[:,batchind], x.vix_seq[:,batchind], x.scaling[:,batchind])
#endregion MLTrain Interface

#region Data
function make_data(df, params)
    # Hold out some of the most recent data so we can backtest on untrained data
    holdout_date = params.data.holdout_date
    df_train = filter(:date => (date -> date < holdout_date), df)
    df_holdout = filter(:date => (date -> date >= holdout_date), df)
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
    len = length(obss.seq[1]),
    batch_size = size(obss, 1),
)
make_buffers(obss) = make_buffers(get_lengths(obss)...)
import Flux
function make_buffers(len, batch_size)
    cpu = (;
        seq = Matrix{Float32}(undef, len, batch_size),
        # Note: Using a BitArray makes it very slow to copy data to the GPU.
        # price_mask = BitArray(undef, prices_len, batch_size),
        mask = Matrix{Bool}(undef, len, batch_size),
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
    for i in eachindex(obss.seq)
        bufs.seq[:,i] .= obss.seq[i]
    end
    for i in eachindex(obss.mask)
        bufs.mask[:,i] .= obss.mask[i]
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
    seq = run_train(model, batch.x)
    ls = Flux.mae(seq, batch.x.seq)
    # global kloss = (;price_seq, vix_seq, scaling)
    # any(isnan, price_seq) && error("price has NaN")
    # any(isnan, vix_seq) && error("vix has NaN")
    # any(isnan, scaling) && error("scaling has NaN")
    # ls = Flux.mse(price_seq, batch.x.price_seq) + Flux.mse(vix_seq, batch.x.vix_seq) + Flux.mse(scaling, batch.x.scaling)
    # ls = err(price_seq, batch.x.price_seq) + err(vix_seq, batch.x.vix_seq) + err(scaling, batch.x.scaling)
    # if isnan(ls)
    #     error("loss is NaN")
    # end
    return ls
end
# function err(v1, v2)
#     sum(err.(v1, v2))
# end
# # err(x1, x2) = (x1 + one(x1)) / (x2 + one(x1)) - one(x1)
# err(x1::Number, x2::Number) = abs((x1 + 1f0) / (x2 + 1f0) - 1f0)

function run_train(model, batchx)
    encoded = run_encoder(model.layers.encoder, batchx)
    return run_decoder(model.layers.decoder, encoded, batchx)
end

function run_infer(encoder, batchx)
    return run_encoder(encoder, batchx)
end

function run_encoder(encoder, batchx)
    # It's expected that the input is already masked, so no need to multiply it by the mask
    v = encoder(batchx.seq)
    return v
end

function run_decoder(decoder, encoded, batchx)
    decoder(encoded) .* batchx.mask
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
    (;hidden_width_mult, through_width_mult) = params.model
    input_width = params.data.width
    through_width = round(Int, through_width_mult * input_width)
    hidden_width = round(Int, hidden_width_mult * input_width)
    return (;
        params.data...,
        params.model...,
        input_width, through_width, hidden_width,
    )
end

function model_encoder(cfg)
    input = Dense(cfg.input_width => cfg.through_width; bias=cfg.use_input_bias)

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(cfg.through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, false), +) for _ in 1:blocks1_count]
    dropout = Dropout(cfg.dropout)
    blocks2 = [SkipConnection(ModelUtil.make_block(cfg.through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, false), +) for _ in 1:blocks2_count]

    output = Dense(cfg.through_width => cfg.encoded_width; bias=false)
    return Chain(;encoder_input=input,
            encoder_blocks=Chain(blocks1..., dropout, blocks2...),
            encoder_output=output)
end

function model_decoder(cfg)
    input = Dense(cfg.encoded_width => cfg.through_width; bias=false)

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(cfg.through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, false), +) for _ in 1:blocks1_count]
    blocks2 = [SkipConnection(ModelUtil.make_block(cfg.through_width, cfg.hidden_width, cfg.layers_per_block, cfg.activation, false), +) for _ in 1:blocks2_count]

    output = Dense(cfg.through_width => cfg.input_width; bias=cfg.use_input_bias)
    return Chain(;decoder_input=input,
            decoder_blocks=Chain(blocks1..., blocks2...),
            decoder_output=output)
end

function get_inference_model(model)
    return model.layers.encoder
end
#endregion Model

end