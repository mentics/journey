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
        prep_input = (obss, args...) -> prep_input(obss, params, args...),
        get_input_keys,
        holdout,
        single = ind -> single(df, ind, params)
    )
end

# Can call on input batch after prep_input
function get_input_keys(batch)
    return batch.keys
end
get_input_keys() = [:ts, :expir]

function single(df, ind, params)
    return prep_input(df[ind:ind,:], params)
    # row = df[ind:ind,:]
    # data = map(eachcol(row)[2:end]) do col
    #     reduce(hcat, col)
    # end
    # return prep_input(data, params)
end

function prep_input_old(obss, params)
    # dataframe of batch_size rows of same columns as input data
    # need to make matrices that can be pushed to gpu and used in model
    keys = (;obss.ts, obss.expir)
    # x = permutedims(Matrix(select(obss, Not([:ts, :expir, :y_bin]))))
    # x = PermutedDimsArray(Matrix(select(obss, Not([:ts, :expir, :y_bin]))), (2,1))
    x = PermutedDimsArray(Matrix(select(obss, Not([:ts, :expir, :y_bin]))), (2,1))
    y = Flux.onehotbatch(obss.y_bin, 1:params.data.bins_count)
    return (;keys, x, y)
end

# 41.1 microseconds
# not thread safe because of reuse of m
function prep_input(obss, params, x)
    # dataframe of batch_size rows of same columns as input data
    # need to make matrices that can be pushed to gpu and used in model
    keys = (;obss.ts, obss.expir)
    # x = permutedims(Matrix(select(obss, Not([:ts, :expir, :y_bin]))))
    # x = PermutedDimsArray(Matrix(select(obss, Not([:ts, :expir, :y_bin]))), (2,1))
    cols = collect(Vector{Float32}, eachcol(obss[!,4:end]))
    # @show size(x) size(cols) size(cols[1])
    # x = Matrix{Float32}(undef, length(cols), length(cols[1]))
    @inbounds for i in eachindex(cols)
        x[i,:] .= cols[i]
    end
    # x = PermutedDimsArray(Matrix(select(obss, Not([:ts, :expir, :y_bin]))), (2,1))
    y = Flux.onehotbatch(obss.y_bin, 1:params.data.bins_count)
    return (;keys, x, y)
end

function prep_input(obss, params)
    # dataframe of batch_size rows of same columns as input data
    # need to make matrices that can be pushed to gpu and used in model
    keys = (;obss.ts, obss.expir)
    cols = collect(Vector{Float32}, eachcol(obss[!,4:end]))
    x = Matrix{Float32}(undef, length(cols), length(cols[1]))
    @inbounds for i in eachindex(cols)
        x[i,:] .= cols[i]
    end
    y = Flux.onehotbatch(obss.y_bin, 1:params.data.bins_count)
    return (;keys, x, y)
end

# 63.8 microseconds
# function prep_input2(obss, params)
#     # dataframe of batch_size rows of same columns as input data
#     # need to make matrices that can be pushed to gpu and used in model
#     keys = (;obss.ts, obss.expir)
#     x = reduce(vcat, reshape.(eachcol(obss[!,4:end]), 1, :))
#     y = Flux.onehotbatch(obss.y_bin, 1:params.data.bins_count)
#     return (;keys, x, y)
# end

function test(obss, m)
    # @show reverse(size(obss))
    # m = Matrix{Float32}(undef, 62, 512)
    # i = 0
    # for col in Iterators.drop(eachcol(obss), 3)
    @inbounds for i in 4:size(obss,2)
        # i += 1
        # m[i-3, :] .= col
        # m[i-3, :] .= obss[!,i]
        @inbounds for j in 1:512
            m[i-3,j] = obss[j, i]
        end
    end
    # @time sm = obss[!,4:end]
    # @time ec = eachcol(sm)
    # @time reshape.(ec, 1, :)
    return nothing
end
#endregion Data

#region Run
function calc_loss(model, batch)
    yhat = run_train(model, batch.x)
    return Flux.Losses.logitcrossentropy(yhat, batch.y)
    # return Flux.Losses.crossentropy(yhat, batch.y)
end

function run_train(model, batchx)
    model(batchx)
    # yhat = model(batchx)
    # yhat = relu(yhat)
    # ss = sum(yhat; dims=1)
    # yhat = yhat ./ ss
    # return yhat
end

function run_infer(model, batchx)
    yhat = model(batchx)
    yhat = relu(yhat)
    ss = sum(yhat; dims=1)
    yhat = yhat ./ ss
    return yhat
end
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

#region Check
# function check_live(training)
    # training.data.single()
# end
function check_load(ind=1)
    df, _ = Paths.load_data_params(Paths.db_output(rpm.NAME), DataFrame)
    check_output(df, ind)
end
import DrawUtil
function check_output(df, ind=1)
    data = Vector(select(df, Not(:key))[ind,:])
    replace!(x -> x < 0 ? 0f0 : x, data)
    # return data
    data ./= sum(data)
    # @show count(x -> x > 0, data)
    key = df.key[ind]
    @show key
    # DrawUtil.draw(:barplot, softmax(data))
    DrawUtil.draw(:barplot, data)
end

function check1(training, ind=1)
    trainee = training.trainee
    batch = training.data.single(ind)
    out1 = vec(trainee.run_infer(training.model, batch.x |> gpu) |> cpu)
    DrawUtil.draw(:barplot, out1)
end
#endregion Check

end