module ReturnProbModel
using DataFrames
using Flux, MLUtils
using Paths, ModelUtil, TrainUtil, MLTrain

#=
No pushing to gpu in data or model modules. Only MLTrain pushed to gpu.
=#

const NAME = replace(string(@__MODULE__), "Model" => "")

params_model() = (;
    block_count = 4,
    layers_per_block = 4,
    hidden_width_mult = 4,
    dropout = 0.2f0,
    activation = NNlib.swish,
    use_bias = false,
)

#region MLTrain Interface
function make_trainee(params_m=params_model())
    df, params_data = Paths.load_data_params(Paths.db_input(NAME), DataFrame)
    input_width = size(df, 2) - 3 # 2 key cols and a y col
    params = (;data=params_data, model=params_m)

    global state = Trainee(;
        name=NAME,
        version=ModelUtil.encode_version(1, params),
        make_model = () -> make_model(input_width, params),
        get_inference_model,
        run_train,
        run_infer,
        prep_data = params_train -> make_data(df, merge(params, (;train=params_train))),
        get_learning_rate = TrainUtil.learning_rate_linear_decay(), # TrainUtil.lr_cycle_decay(),
        get_loss = calc_loss,
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

to_draw_x(batch, ind) = batch.vix_seq[1:end,ind]
to_draw_y(batch, ind) = batch.vix_seq[1:end,ind]
to_draw_yh(yhat, ind) = yhat.vix[1:end,ind]

# to_draw_y(batch, ind) = (batch.ts, batch.y[:,ind])
# to_draw_yh(yhat, ind) = (bins(), softmax(yhat[:,ind]))

# to_draw_x(batch, ind) = batch.vix.v[1:end,ind]
# to_draw_yh(yhat, ind) = yhat.vix[1:end,ind]
#endregion MLTrain Interface

#region Data
function make_data(df, params)
    shuffled = shuffleobs(df) # pre-shuffle so holdout is random
    training, holdout = splitobs(shuffled; at=(1.0 - params.train.holdout))
    InputData(;
        all_data = df,
        data_for_epoch = () -> shuffleobs(training),
        prep_input = obss -> prep_input(obss, params),
        get_input_keys,
        holdout,
        single = ind -> single(df, ind, params)
    )
end

# Can call on input batch after prep_input
function get_input_keys(batch)
    return batch.ts, batch.expir
end

function single(df, ind, params)
    return prep_input(df[ind:ind,:], params)
    # row = df[ind:ind,:]
    # data = map(eachcol(row)[2:end]) do col
    #     reduce(hcat, col)
    # end
    # return prep_input(data, params)
end

function prep_input(obss, params)
    # dataframe of batch_size rows of same columns as input data
    # need to make matrices that can be pushed to gpu and used in model
    x = permutedims(Matrix(select(obss, Not([:ts, :expir, :y_bin]))))
    y = Flux.onehotbatch(obss.y_bin, 1:params.data.bins_count)
    return (;x, y)
end
#endregion Data

#region Run
function calc_loss(model, batch)
    yhat = run_train(model, batch.x)
    return Flux.Losses.logitcrossentropy(yhat, batch.y)
end

function run_train(model, batchx)
    return model(batchx)
end

run_infer(model, batchx) = run_train(model, batchx)
#endregion Run

#region Model
function make_model(input_width, params)
    cfg = params.model
    through_width = cfg.hidden_width_mult * input_width
    input_layer = Dense(input_width => through_width; bias=cfg.use_bias)

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(through_width, through_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks1_count]
    dropout = Dropout(cfg.dropout)
    blocks2 = [SkipConnection(ModelUtil.make_block(through_width, through_width, cfg.layers_per_block, cfg.activation, cfg.use_bias), +) for _ in 1:blocks2_count]

    output_layer = Dense(through_width => params.data.bins_count; bias=false)
    return Chain(;input_layer, blocks=Chain(blocks1..., dropout, blocks2...), output_layer)
end

function get_inference_model(model)
    return model
end
# TODO: function make_inference_model()
#endregion Model

end