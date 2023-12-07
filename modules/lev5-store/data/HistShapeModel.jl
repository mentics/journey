module HistShapeModel
using Dates, StatsBase, DataFrames, Base64
using Flux, NNlib, MLUtils
using DateUtil, Paths
using ModelUtil, TrainUtil, MLTrain
import HistShape:NAME

params_model() = (;
    encoded_width = 32,
    block_count = 1,
    layers_per_block = 2,
    hidden_width_mult = 2,
    dropout = 0.2f0,
    activation = NNlib.swish,
    use_bias = false,
)

encode_version(num, params) = "$(num)-$(Base64.base64encode(hash(params)))"

#region MLTrain Interface
function MLTrain(params=params_model())
    df, params_data = load_data_params(db_input(NAME), DataFrame)

    global state = Trainee10(;
        name=NAME,
        version=encode_version(1, params),
        make_model = () -> make_model(merge(params, params_data)),
        get_inference_model,
        run_train,
        run_infer,
        prep_data = params_data -> make_data(df, params_data),
        get_learning_rate = TrainUtil.learning_rate_linear_decay(), # TrainUtil.lr_cycle_decay(),
        get_loss = calc_loss,
        params = (;data=params_data, model=params),
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

to_draw_x(batch, ind) = batch.vix_seq[1:end,ind]
to_draw_y(batch, ind) = batch.vix_seq[1:end,ind]
to_draw_yh(yhat, ind) = yhat.vix[1:end,ind]

# to_draw_y(batch, ind) = (batch.ts, batch.y[:,ind])
# to_draw_yh(yhat, ind) = (bins(), softmax(yhat[:,ind]))

# to_draw_x(batch, ind) = batch.vix.v[1:end,ind]
# to_draw_yh(yhat, ind) = yhat.vix[1:end,ind]
#endregion MLTrain Interface

#region Next
function get_inference_model(model)
    return model.layers.encoder
end

function save_encoded()
    # (;model, run) = load_inference()
    # data = load_data_input()
    # @assert unique(data.ts) == data.ts
    # batch_size = config().batch_size
    # width = config().encoded_width + 4
    # df = DataFrame([DateTime[], [Float32[] for _ in 1:width]...], [:ts, [Symbol("c$(i)") for i in 1:width]...])
    # for inds in IndexUtil.batch_inds_all(eachindex(data.ts), batch_size)
    #     enc = run(batch_from_inds(data, inds)) |> cpu
    #     # m = hcat(data.ts[inds], vcat(v, meta_under, meta_vix)')
    #     m = hcat(data.ts[inds], enc')
    #     push!.(Ref(df), [m[i,:] for i in 1:size(m, 1)])
    # end
    # @assert unique(df.ts) == df.ts
    # dat.save(ENCODED_PATH, df)
    # return df
end

#endregion Next

#region Data
function make_data(df, params)
    shuffled = shuffleobs(df) # pre-shuffle so holdout is random
    training, holdout = splitobs(shuffled; at=(1.0 - params.holdout))
    InputData5(;
        data_for_epoch = () -> shuffleobs(training),
        prep_input,
        holdout,
        single = (ind -> single(df, ind))
    )
end

function single(df, ind)
    return prep_input(df[ind:ind,:])
    row = df[ind:ind,:]
    data = map(eachcol(row)[2:end]) do col
        reduce(hcat, col)
    end
    return prep_input(data)
end

function prep_input(obss)
    # dataframe of batch_size rows of same columns as input data
    # need to make matrices that can be pushed to gpu and used in model
    global kobss = obss
    # TODO: find better way
    data = map(eachcol(obss)) do col
        reduce(hcat, col)
    end
    return NamedTuple{Tuple(Symbol.(names(obss)))}(data)
end

# function test_kfold(obss)
#     for (train_data, val_data) in kfolds(obss; k=5)
#         for obs in eachobs(train_data, batchsize=10)
#             return obs
#         end
#     end
# end
#endregion Data

#region Run
function calc_loss(model, batch)
    (;prices, vix) = run_train(model, batch)
    return Flux.mse(prices, batch.prices_seq) + Flux.mse(vix, batch.vix_seq)
end

function run_train(model, batch)
    encoded = run_encoder(model.layers.encoder, batch)
    return run_decoder(model.layers.decoder, encoded, batch)
end

function run_infer(encoder, batch)
    return run_encoder(encoder, batch)
end

function run_encoder(encoder, batch)
    # It's expected that the input is already masked, so no need to multiply it by the mask
    v = encoder((vcat(batch.prices_seq, batch.prices_meta), vcat(batch.vix_seq, batch.vix_meta)))
    return vcat(v, batch.prices_meta, batch.vix_meta)
end

function run_decoder(decoder, encoded, input)
    (dec_prices_raw, dec_vix_raw) = decoder(encoded)
    prices = dec_prices_raw .* input.prices_mask
    vix = dec_vix_raw .* input.vix_mask
    return (;prices, vix)
end
#endregion Run

#region Model
function make_model(params)
    cfg = config(params)
    return Chain(; encoder=model_encoder(cfg), decoder=model_decoder(cfg))
end

function config(params)
    (;weeks_count, hidden_width_mult, encoded_width) = params
    input_width_under = DateUtil.TIMES_PER_WEEK * weeks_count
    vix_count = DateUtil.DAYS_PER_WEEK * weeks_count
    input_width_vix = vix_count * 4
    input_width_meta = 2
    input_width = input_width_under + input_width_vix
    hidden_width_under = hidden_width_mult * input_width_under
    hidden_width_vix = hidden_width_mult * input_width_vix
    hidden_width = hidden_width_mult * input_width
    encoded_with_meta = encoded_width + 4
    return (;
        params...,
        vix_count,
        input_width_under, input_width_vix,
        input_width_meta,
        hidden_width_under, hidden_width_vix, hidden_width,
        encoded_with_meta,
    )
end

function model_encoder(cfg)
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

function model_decoder(cfg)
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

end