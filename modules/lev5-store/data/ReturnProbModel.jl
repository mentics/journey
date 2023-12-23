module ReturnProbModel
using Intervals, Dates, DataFrames, StatsBase
using Flux, MLUtils
using DateUtil, Paths, ModelUtil, TrainUtil, MLTrain
import CudaUtil:copyto_itr!

#=
No pushing to gpu in data or model modules. Only MLTrain pushed to gpu. well... would be nice, but api for data was messy that way, but maybe can get back to that.

TODO: something is wrong with dividing by ce_all this way.
=#

get_input_width(df) = size(df, 2) - 3 # 2 key cols and a y col

const NAME = replace(string(@__MODULE__), "Model" => "")

params_model() = (;
    block_count = 2,
    layers_per_block = 2,
    use_output_for_hidden = true,
    hidden_width_mult = 1,
    dropout = 0.0f0,
    activation = NNlib.swish,
    use_bias_in = false,
    use_bias_block = false,
    use_bias_out = false,
    output_activation = NNlib.relu,
)

import MLyze
function setup_ce_all(df, nbins)
    ky = calc_y_pmfk(df.y_bin, nbins)
    # return [Flux.crossentropy(ky, Flux.onehot(y, 1:nbins)) for y in 1:nbins]
    return [Flux.crossentropy(ky, get_y(y) |> cpu) for y in 1:nbins]
end
calc_y_pmfk(y_bins, nbins) = MLyze.calc_pmf_kde(y_bins; nbins=nbins)

# function save_y_pmfk()
#     df, params_data = Paths.load_data_params(Paths.db_input(NAME), DataFrame)
#     y_pmfk = calc_y_pmfk(df.y_bin, params_data.bins_count)
#     Paths.save_data(Paths.db_output("y_pmfk"); y_pmfk)
# end

# function load_y_pmfk()
#     # TODO: age?
#     return Paths.load_data(Paths.db_output("y_pmfk"), "y_pmfk")
# end

function save_ce_all()
    df, params_data = Paths.load_data_params(Paths.db_input(NAME), DataFrame)
    ce_all = setup_ce_all(df, params_data.bins_count)
    Paths.save_data(Paths.db_output("ce_all"); ce_all)
end

# function load_ce_all()
#     # TODO: age?
#     return Paths.load_data(Paths.db_output("ce_all"), "ce_all")
# end

#region MLTrain Interface
function make_trainee(params_m=params_model())
    df, params_data = Paths.load_data_params(Paths.db_input(NAME), DataFrame)
    # df = select(df, Not([:xtq1_call, :xtq2_call, :xtq3_call, :xtq1_put, :xtq2_put, :xtq3_put]))
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
        get_loss = (model, batch) -> calc_loss(model, batch),
        params,
        mod = @__MODULE__
    )
    return state
end

# to_draw_x(batch, ind) = (1:803, batch.x[1:end,ind])
to_draw_y(batch, ind) = batch.y[1:end,ind] |> cpu
to_draw_yh(yhat, ind) = yhat[1:end,ind] |> cpu

# to_draw_x(batch, ind) = batch.prices_seq[1:end,ind]
# to_draw_y(batch, ind) = batch.prices_seq[1:end,ind]
# to_draw_yh(yhat, ind) = yhat.prices[1:end,ind]

# to_draw_x(batch, ind) = batch.vix_seq[1:end,ind]
# to_draw_y(batch, ind) = batch.vix_seq[1:end,ind]
# to_draw_yh(yhat, ind) = yhat.vix[1:end,ind]

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
    ce_all = setup_ce_all(df, params.data.bins_count)

    # shuffled = shuffleobs(df) # pre-shuffle so holdout is random
    # training, holdout = splitobs(shuffled; at=(1.0 - params.train.holdout))
    InputData(;
        all_data = df,
        data_for_epoch = () -> shuffleobs(df_train),
        prep_input = (obss, args...) -> prep_input(obss, params, args...; ce_all),
        get_input_keys,
        holdout=df_holdout,
        single = ind -> single(df, ind, params; ce_all)
    )
end

# Can call on input batch after prep_input
function get_input_keys(batch)
    return batch.keys
end
get_input_keys() = [:ts, :expir]

single(df, ind, params; kws...) = prep_input(df[ind:ind,:], params; kws...)

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

prep_input(obss, params; kws...) = prep_input(obss, params, make_buffers(obss); kws...)
# 41.1 microseconds
# not thread safe because of reuse of m
function prep_input(obss, params, bufs; ce_all)
    # dataframe of batch_size rows of same columns as input data
    # need to make matrices that can be pushed to gpu and used in model
    keys = (;obss.ts, obss.expir)
    cols = collect(Vector{Float32}, eachcol(obss[!,4:end]))
    for i in eachindex(cols)
        bufs.cpu.x[i,:] .= cols[i]
    end
    copyto!(bufs.gpu.x, bufs.cpu.x)
    # y = Flux.onehotbatch(obss.y_bin, 1:params.data.bins_count) |> gpu
    y = YS2[][:,obss.y_bin]
    ce_compare = (ce_all[obss.y_bin] .^2) |> gpu
    return (;keys, x=bufs.gpu.x, y, ce_compare)
end

import CUDA
const YS2 = Ref{CuArray{Float32, 2, CUDA.Mem.DeviceBuffer}}()
import Distributions
function setup_y()
    ys = Matrix(undef, Bins.VNUM, Bins.VNUM)
    for i in 1:Bins.VNUM
        dlap = Distributions.Laplace(Float32(i), 16f0) # from roughly fitting to all y_bin and narrowing as a goal
        ys[:,i] .= vcu.normalize!([Distributions.pdf(dlap, x) for x in 1:Bins.VNUM])
    end
    YS2[] = ys |> gpu
end
get_y(y) = YS2[][:,y]

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
function calc_loss(model, batch; kws...)
    yhat = run_train(model, batch.x)
    return calc_loss_for(yhat, batch.y, batch.ce_compare; kws...)
end
function calc_loss_for(yhat, y, ce_compare)
    # return Flux.Losses.crossentropy(yhat, y)
    ce = Flux.Losses.crossentropy(yhat, y; agg=(x -> mean(x ./ ce_compare)))
    return ce
    # smooth_penalty = 10 * calc_smooth_penalty(yhat) # / (10 + ce)
    # return ce + smooth_penalty
end

import Distributions
import VectorCalcUtil as vcu
function min_loss(params)
    count = params.data.bins_count
    y_bins = [count ÷ 2, 1, count, rand(1:count)]
    d = Dict()
    ce_compare = fill(1f0, count)
    for y_bin in y_bins
        y = Flux.onehotbatch(y_bin, 1:count)
        # y = Flux.onehotbatch(obss.y_bin, 1:count)
        perfect = calc_loss_for(y, y, ce_compare)

        ndist = Distributions.Normal(1.0, 0.1)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri10 = calc_loss_for(yhat, y, ce_compare)

        ndist = Distributions.Normal(1.0, 0.05)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri5 = calc_loss_for(yhat, y, ce_compare)

        ndist = Distributions.Normal(1.0, 0.01)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri1 = calc_loss_for(yhat, y, ce_compare)

        yhat = vcu.normalize!(rand(count))
        random = calc_loss_for(yhat, y, ce_compare)

        res = (;perfect, distri1, distri5, distri10, random)
        println(res)
        d[y_bin] = res
    end
    return d
end

function max_smooth_penalty()
    v = [x % 2 == 0 ? 1. : 0. for x in 1:100]
    vcu.normalize!(v)
    return calc_smooth_penalty(v)
end
function ok_smooth_penalty()
    v = [x < 50 ? x : 100.0 - x for x in 1.0:100.0]
    vcu.normalize!(v)
    return calc_smooth_penalty(v)
end
const KERNEL3 = reshape([-1.0, 1.0,-1.0], 3,1,1) |> gpu
const KERNEL_CPU = reshape([-1.0, 1.0,-1.0], 3,1,1)
import CUDA:CuArray
function calc_smooth_kernel(v::CuArray)
    sz = size(v)
    reshape(Flux.conv(reshape(v, sz[1],1,sz[2]), KERNEL3; pad=1), sz[1], sz[2])
end
function calc_smooth_kernel(v::CuArray{Float32, 1})
    sz = (length(v),1)
    reshape(Flux.conv(reshape(v, sz[1],1,sz[2]), KERNEL3; pad=1), sz[1], sz[2])
end
function calc_smooth_kernel(v::AbstractVector)
    sz = (length(v),1)
    reshape(Flux.conv(reshape(v, sz[1],1,sz[2]), KERNEL_CPU; pad=1), sz[1], sz[2])
end
# calc_smooth_penalty(yhat) = sum(abs.(yhat .- circshift(yhat, 1)))
function calc_smooth_penalty(v)
    count(x -> x > 0, calc_smooth_kernel(v)) / length(v)
end

const KERNEL = fill(0.2f0, 5) |> gpu
function run_train(model, batchx)
    yhat = model(batchx)
    # global kyhat1 = yhat
    # return softmax(yhat)

    # yhat = relu(yhat)
    # sz = size(yhat)
    # yhat_smoothed = reshape(NNlib.conv(reshape(yhat, sz[1], 1, sz[2]), reshape(KERNEL, 5, 1, 1); pad=2), sz...)
    # return softmax(yhat_smoothed)


    # yhat = relu(yhat)
    # global kyhat = yhat
    # sz = size(yhat)
    # yhat = reshape(NNlib.conv(reshape(yhat, sz[1], 1, sz[2]), reshape(KERNEL, 5, 1, 1); pad=2), sz...)
    ss = sum(yhat; dims=1)
    yhat = yhat ./ ss
    return yhat
end

function run_infer(model, batchx)
    return run_train(model, batchx)
    # yhat = model(batchx)
    # yhat = relu(yhat)
    # ss = sum(yhat; dims=1)
    # yhat = yhat ./ ss
    # return yhat
end
#endregion Run

#region Model
function make_model(input_width, params)
    cfg = params.model
    output_width = params.data.bins_count
    through_width = cfg.use_output_for_hidden ? cfg.hidden_width_mult * output_width : cfg.hidden_width_mult * input_width

    input_layer = Dense(input_width => through_width; bias=cfg.use_bias_in)

    blocks1_count = cfg.block_count ÷ 2
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(through_width, through_width, cfg.layers_per_block, cfg.activation, cfg.use_bias_block), +) for _ in 1:blocks1_count]
    dropout = Dropout(cfg.dropout)
    blocks2 = [SkipConnection(ModelUtil.make_block(through_width, through_width, cfg.layers_per_block, cfg.activation, cfg.use_bias_block), +) for _ in 1:blocks2_count]

    output_layer = Dense(through_width => output_width, cfg.output_activation; bias=cfg.use_bias_out)

    b1 = [Symbol(string("b1_",i)) => blocks1[i] for i in eachindex(blocks1)]
    b2 = [Symbol(string("b2_",i)) => blocks2[i] for i in eachindex(blocks2)]

    if iszero(cfg.dropout)
        return Chain(;input_layer, b1..., b2..., output_layer)
    else
        # return Chain(;input_layer, blocks=Chain(blocks1..., dropout, blocks2...), output_layer)
        return Chain(;input_layer, b1..., dropout, b2..., output_layer)
    end
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
        yhat = vec(trainee.run_infer(training.model, batch.x |> gpu) |> cpu)
        cc = vec(batch.ce_compare |> cpu)
        return yhat, cc, (batch.y |> cpu)
        DrawUtil.draw!(:barplot, Bins.xs(), yhat; label="i-$(ind)")
    end
end

#endregion Check

end