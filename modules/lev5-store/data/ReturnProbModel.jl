module ReturnProbModel
using Intervals, Dates, DataFrames
using Flux, MLUtils
using DateUtil, Paths, ModelUtil, TrainUtil, MLTrain
import CudaUtil:copyto_itr!

#=
No pushing to gpu in data or model modules. Only MLTrain pushed to gpu.
=#

get_input_width(df) = size(df, 2) - 3 # 2 key cols and a y col

const NAME = replace(string(@__MODULE__), "Model" => "")

params_model() = (;
    block_count = 4,
    layers_per_block = 4,
    hidden_width_mult = 2,
    dropout = 0.05f0,
    activation = NNlib.swish,
    use_bias = false,
)

#region MLTrain Interface
function make_trainee(params_m=params_model())
    df, params_data = Paths.load_data_params(Paths.db_input(NAME), DataFrame)
    input_width = get_input_width(df)
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
# date_range_train() = (;date_range = DateUtil.DEFAULT_DATA_START_DATE..Date(2023,6,30))
# date_range_backtest() = (;date_range = Date(2023,7,1)..DateUtil.market_today())
function make_data(df, params)
    # Hold out some of the most recent data so we can backtest on untrained data
    df_train = filter(:ts => DateUtil.ts_in(params.data.hist.data.train_date_range), df)
    df_holdout = filter(:ts => !DateUtil.ts_in(params.data.hist.data.train_date_range), df)
    @assert df_holdout.ts[1] > df_train.ts[end] "df_holdout.ts[1] $(df_holdout.ts[1]) > $(df_train.ts[end]) df_train.ts[end]"

    # shuffled = shuffleobs(df) # pre-shuffle so holdout is random
    # training, holdout = splitobs(shuffled; at=(1.0 - params.train.holdout))
    InputData(;
        all_data = df,
        data_for_epoch = () -> shuffleobs(df_train),
        prep_input = (obss, args...) -> prep_input(obss, params, args...),
        get_input_keys,
        holdout=df_holdout,
        single = ind -> single(df, ind, params)
    )
end

# Can call on input batch after prep_input
function get_input_keys(batch)
    return batch.keys
end
get_input_keys() = [:ts, :expir]

single(df, ind, params) = prep_input(df[ind:ind,:], params)

# function prep_input_old(obss, params)
#     # dataframe of batch_size rows of same columns as input data
#     # need to make matrices that can be pushed to gpu and used in model
#     keys = (;obss.ts, obss.expir)
#     # x = permutedims(Matrix(select(obss, Not([:ts, :expir, :y_bin]))))
#     # x = PermutedDimsArray(Matrix(select(obss, Not([:ts, :expir, :y_bin]))), (2,1))
#     x = PermutedDimsArray(Matrix(select(obss, Not([:ts, :expir, :y_bin]))), (2,1))
#     y = Flux.onehotbatch(obss.y_bin, 1:params.data.bins_count)
#     return (;keys, x, y)
# end

make_buffers(obss) = make_buffers(get_input_width(obss), size(obss, 1))
import Flux
function make_buffers(input_width, batch_size)
    c = (;
        x = Matrix{Float32}(undef, input_width, batch_size),
        # Note: custom objects are used for onehot and copyto! doesn't seem to work on them.
        # y = Matrix{Bool}(undef, bins_count, batch_size),
    )
    g = (;
        x = Flux.gpu(c.x)
    )
    return (;cpu=c, gpu=g)
end

prep_input(obss, params) = prep_input(obss, params, make_buffers(obss))
# 41.1 microseconds
# not thread safe because of reuse of m
function prep_input(obss, params, bufs)
    # dataframe of batch_size rows of same columns as input data
    # need to make matrices that can be pushed to gpu and used in model
    keys = (;obss.ts, obss.expir)
    cols = collect(Vector{Float32}, eachcol(obss[!,4:end]))
    for i in eachindex(cols)
        bufs.cpu.x[i,:] .= cols[i]
    end
    copyto!(bufs.gpu.x, bufs.cpu.x)
    y = Flux.onehotbatch(obss.y_bin, 1:params.data.bins_count) |> gpu
    return (;keys, x=bufs.gpu.x, y)
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

# function test(obss, m)
#     # @show reverse(size(obss))
#     # m = Matrix{Float32}(undef, 62, 512)
#     # i = 0
#     # for col in Iterators.drop(eachcol(obss), 3)
#     @inbounds for i in 4:size(obss,2)
#         # i += 1
#         # m[i-3, :] .= col
#         # m[i-3, :] .= obss[!,i]
#         @inbounds for j in 1:512
#             m[i-3,j] = obss[j, i]
#         end
#     end
#     # @time sm = obss[!,4:end]
#     # @time ec = eachcol(sm)
#     # @time reshape.(ec, 1, :)
#     return nothing
# end
#endregion Data

#region Run
function calc_loss(model, batch)
    yhat = run_train(model, batch.x)
    return calc_loss_for(yhat, batch.y)
end
function calc_loss_for(yhat, y)
    # return Flux.Losses.logitcrossentropy(yhat, batch.y)
    return Flux.Losses.crossentropy(yhat, y)
end

import Distributions
import VectorCalcUtil as vcu
function min_loss(params)
    count = params.data.bins_count
    y_bins = [count ÷ 2, 1, count, rand(1:count)]
    d = Dict()
    for y_bin in y_bins
        y = Flux.onehotbatch(y_bin, 1:count)
        # y = Flux.onehotbatch(obss.y_bin, 1:count)
        perfect = calc_loss_for(y, y)

        ndist = Distributions.Normal(1.0, 0.1)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri10 = calc_loss_for(yhat, y)

        ndist = Distributions.Normal(1.0, 0.05)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri5 = calc_loss_for(yhat, y)

        ndist = Distributions.Normal(1.0, 0.01)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri1 = calc_loss_for(yhat, y)

        yhat = vcu.normalize!(rand(count))
        random = calc_loss_for(yhat, y)

        res = (;perfect, distri1, distri5, distri10, random)
        println(res)
        d[y_bin] = res
    end
    return d
end

function run_train(model, batchx)
    # model(batchx)
    yhat = model(batchx)
    yhat = relu(yhat)
    ss = sum(yhat; dims=1)
    yhat = yhat ./ ss
    return yhat
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
    df, _ = Paths.load_data_params(Paths.db_output(NAME), DataFrame)
    check_output(df, ind)
end
import DrawUtil
using ProbMeta
function check_output(df, ind=1)
    # data = Vector(select(df, Not(:key))[ind,:])
    # replace!(x -> x < 0 ? 0f0 : x, data)
    # # return data
    # data ./= sum(data)
    # # @show count(x -> x > 0, data)
    # key = df.key[ind]
    # @show key
    # # DrawUtil.draw(:barplot, softmax(data))

    DrawUtil.draw(:barplot, Bins.xs(), df.output[ind])
end

function check1(training, inds=1)
    trainee = training.trainee
    DrawUtil.draw(:vlines, 1.0; color=:white)
    for ind in inds
        batch = training.data.single(ind)
        out1 = vec(trainee.run_infer(training.model, batch.x |> gpu) |> cpu)
        DrawUtil.draw!(:barplot, Bins.xs(), out1; label="i-$(ind)")
    end
end

#endregion Check

end