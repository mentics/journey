module ReturnProbModel
using Intervals, Dates, DataFrames, StatsBase
using Flux, MLUtils
import CUDA:CuArray
using DateUtil, Paths, ModelUtil, TrainUtil, MLTrain
import CudaUtil:copyto_itr!
import DataFrameUtil as dfu
import  ProbMeta as Bins

#=
No pushing to gpu in data or model modules. Only MLTrain pushed to gpu. well... would be nice, but api for data was messy that way, but maybe can get back to that.

TODO: something is wrong with dividing by ce_all this way?
=#

const NAME = replace(string(@__MODULE__), "Model" => "")

params_model() = (;
    block_count = 4,
    layers_per_block = 2,
    use_output_for_hidden = false,
    hidden_width_mult = 2,
    dropout = 0.1f0,
    use_bias_in = false,
    use_bias_block = false,
    use_bias_out = false,
    activation = NNlib.swish,
    output_activation = NNlib.sigmoid_fast,
    # output_func = run_train_softmax,
    output_func = run_train_sum1,
    softmax_temp = 1.2f0, # 8.4f0,
    ce_compare_squared = true,

    bins_count = 203,
    bins_span = 0.05,
)

#region MLTrain Interface
function load_input(; days_to_xpir=nothing)
    df, params_data, path = Paths.load_data_params(Paths.db_input(NAME), DataFrame)
    if !isnothing(days_to_xpir)
        df = dfu.filter_days_to_xpir(df, days_to_xpir)
    end
    return df, params_data
end

function make_trainee(params_m=params_model(); days_to_xpir=nothing)
    df, params_data = load_input(; days_to_xpir)
    # df = select(df, Not([:xtq1_call, :xtq2_call, :xtq3_call, :xtq1_put, :xtq2_put, :xtq3_put]))
    println("Training with $(size(df,1)) observations")
    input_width = get_input_width(df) # get_input_width(df, params_data.skip_cols)
    params = (;data=params_data, model=params_m)
    Bins.init_bins(params.model.bins_count, params.model.bins_span)

    global state = Trainee(;
        name=NAME,
        version=ModelUtil.encode_version(1, params),
        make_model = () -> make_model(input_width, params),
        get_inference_model,
        run_train = (model, batch) -> params_m.output_func(model, batch, params),
        run_infer = (model, batch) -> params_m.output_func(model, batch, params),
        prep_data = params_train -> make_data(df, merge(params, (;train=params_train))),
        get_learning_rate = TrainUtil.learning_rate_linear_decay(1e-4), # TrainUtil.lr_cycle_decay(),
        get_loss = (model, batch) -> calc_loss(params_m.output_func, model, batch, params),
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
# TODO: make this easier, more reliable?
get_input_width(df) = size(df, 2) - 3 # should get this 3 from params.data.skip_cols
# get_input_width(df, skip) = size(df, 2) - skip

function make_data(df, params)
    global kmd = (;df, params)
    date_range = params.data.hist.data.date_range
    @assert df.ts[1] in date_range && df.ts[end] in date_range
    # Hold out some of the most recent data so we can backtest on untrained data
    dfrange = filter(:ts => (date -> year(date) >= 2018), df)
    df_train, df_holdout = dfu.split(dfrange, params.data.hist.data.holdout_date; keycol=:ts)
    @assert df_holdout.ts[1] > df_train.ts[end] "df_holdout.ts[1] $(df_holdout.ts[1]) > $(df_train.ts[end]) df_train.ts[end]"
    # ce_all = setup_ce_all(df_train)
    println("Training with data between $(df_train.ts[1]) and $(df_train.ts[end])")
    println("Holdout has data between $(df_holdout.ts[1]) and $(df_holdout.ts[end])")

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

make_buffers(obss::DataFrame) = make_buffers(get_input_width(obss), size(obss, 1))
import Flux
function make_buffers(input_width::Integer, batch_size::Integer)
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
function prep_input(obss, params, bufs)
    # dataframe of batch_size rows of same columns as input data
    # need to make matrices that can be pushed to gpu and used in model
    keys = (;obss.ts, obss.expir)
    cols = collect(Vector{Float32}, eachcol(obss[!,(params.data.skip_cols+1):end]))
    for i in eachindex(cols)
        bufs.cpu.x[i,:] .= cols[i]
    end
    copyto!(bufs.gpu.x, bufs.cpu.x)
    # y = YS2[][:,obss.y_bin]
    y = Flux.onehotbatch(obss.y_bin, 1:params.model.bins_count) |> gpu
    # ce_compare = obss.ce_compare |> gpu # ce_all[obss.y_bin] |> gpu
    # if !OVERRIDE_SQUARED[] && params.model.ce_compare_squared
    #     ce_compare .^= 2
    # end
    # return (;keys, x=bufs.gpu.x, y, ce_compare)
    return (;keys, x=bufs.gpu.x, y)
end
const OVERRIDE_SQUARED = Ref(false)

import CUDA
const YS2 = Ref{CuArray{Float32, 2, CUDA.Mem.DeviceBuffer}}()
import Distributions
function setup_y()
    ys = Matrix(undef, Bins.num_edges(), Bins.num_edges())
    for i in 1:Bins.num_edges()
        dlap = Distributions.Laplace(Float32(i), 16f0) # from roughly fitting to all y_bin and narrowing as a goal
        ys[:,i] .= vcu.normalize!([Distributions.pdf(dlap, x) for x in 1:Bins.num_edges()])
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
function calc_loss(f, model, batch, params; kws...)
    yhat = f(model, batch.x, params)
    # return calc_loss_for(yhat, batch.y, batch.ce_compare; kws...)
    return calc_loss_for(yhat, batch.y; kws...)
end
function agg_loss(x, ce_compare)
    len = size(ce_compare, 1)
    @assert size(x) == (1,len)
    vx = vec(x)
    @assert size(vx) == (len,)
    comp = vx ./ ce_compare
    @assert size(comp) == (len,)
    m = mean(comp)
    @assert typeof(m) == Float32 "typeof(m) $(typeof(m)) == Float32"
    return m
end
function calc_loss_for(yhat, y) # , ce_compare)
    # ce = Flux.Losses.crossentropy(yhat, y; agg=(x -> agg_loss(x, ce_compare)))
    # return ce

    # return Flux.Losses.crossentropy(yhat, y)
    return Flux.Losses.binarycrossentropy(yhat, y)

    # return Flux.Losses.mse(yhat, y)

    # smooth_penalty = 10 * calc_smooth_penalty(yhat) # / (10 + ce)
    # return ce + smooth_penalty
end

import Distributions
import VectorCalcUtil as vcu
import ReturnProbData as rpd
function min_loss()
    y_pmfk = load_y_pmfk()
    pmfk_lookup = rpd.load_pmfk_lookup()
    count = length(y_pmfk) # params.data.bins_count
    _, pmfk_max = findmax(y_pmfk)
    y_bins = [count ÷ 2, pmfk_max, 1, count, rand(1:count)]

    d = Dict()
    # ce_all = fill(1f0, count)
    ce_all = load_ce_all()
    for y_bin in y_bins
        ce_compare = ce_all[y_bin:y_bin]
        y = Flux.onehotbatch(y_bin, 1:count)
        # y = Flux.onehotbatch(obss.y_bin, 1:count)
        perfect = calc_loss_for(y, y, ce_compare)

        ndist = Distributions.Normal(1.0, 0.001)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri01 = calc_loss_for(yhat, y, ce_compare)

        ndist = Distributions.Normal(1.0, 0.01)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri1 = calc_loss_for(yhat, y, ce_compare)

        ndist = Distributions.Normal(1.0, 0.05)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri5 = calc_loss_for(yhat, y, ce_compare)

        ndist = Distributions.Normal(1.0, 0.1)
        yhat = replace(x -> x < 1f-10 ? 0f0 : x, vcu.normalize!([Distributions.pdf(ndist, x) for x in Bins.xs()]))
        distri10 = calc_loss_for(yhat, y, ce_compare)

        yhat = vcu.normalize!(rand(count))
        random = calc_loss_for(yhat, y, ce_compare)

        # @show y_bin
        # global kargs = (;y_pmfk, y, ce_compare)
        yp = calc_loss_for(y_pmfk, y, ce_compare)

        res = (;perfect, distri01, distri1, distri5, distri10, random, yp)
        # println(res)
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

function run_train_softmax(model, batchx, params)
    yhat = model(batchx)
    return softmax(yhat ./ params.model.softmax_temp)
end

function run_train_sum1(model, batchx, params)
    yhat = model(batchx)
    global kyhat = yhat
    @assert all(>=(0f0), yhat)
    ss = sum(yhat; dims=1)
    @assert all(>=(0f0), ss)
    yhat = yhat ./ ss
    return yhat
end

    # const KERNEL = fill(0.2f0, 5) |> gpu
    # sz = size(yhat)
    # yhat_smoothed = reshape(NNlib.conv(reshape(yhat, sz[1], 1, sz[2]), reshape(KERNEL, 5, 1, 1); pad=2), sz...)
    # return softmax(yhat_smoothed)
#endregion Run

#region Model
function make_model(input_width, params)
    cfg = params.model
    output_width = params.data.bins_count
    through_width = cfg.use_output_for_hidden ? cfg.hidden_width_mult * output_width : cfg.hidden_width_mult * input_width
    @show input_width through_width

    input_layer = Dense(input_width => through_width, cfg.activation; bias=cfg.use_bias_in)

    # blocks1_count = cfg.block_count ÷ 2
    # blocks2_count = cfg.block_count - blocks1_count
    # blocks1 = [SkipConnection(ModelUtil.make_block(through_width, through_width, cfg.layers_per_block, cfg.activation, cfg.use_bias_block), +) for _ in 1:blocks1_count]
    # dropout = Dropout(cfg.dropout)
    # blocks2 = [SkipConnection(ModelUtil.make_block(through_width, through_width, cfg.layers_per_block, cfg.activation, cfg.use_bias_block), +) for _ in 1:blocks2_count]
    # b1 = [Symbol(string("b1_",i)) => blocks1[i] for i in eachindex(blocks1)]
    # b2 = [Symbol(string("b2_",i)) => blocks2[i] for i in eachindex(blocks2)]
    # if iszero(cfg.dropout)
    #     return Chain(;input_layer, b1..., b2..., output_layer)
    # else
    #     # return Chain(;input_layer, blocks=Chain(blocks1..., dropout, blocks2...), output_layer)
    #     return Chain(;input_layer, b1..., dropout, b2..., output_layer)
    # end

    blocks = []
    push!(blocks, :block_1 => SkipConnection(ModelUtil.make_block(through_width, through_width, cfg.layers_per_block, cfg.activation, cfg.use_bias_block), +))
    for i in 1:cfg.block_count
        # push!(blocks, Symbol("bn$(i)") => bn(through_width))
        if !iszero(cfg.dropout)
            push!(blocks, Symbol("dropout_$(i)") => Dropout(cfg.dropout))
        end
        push!(blocks, Symbol("block_$(i)") => SkipConnection(ModelUtil.make_block(through_width, through_width, cfg.layers_per_block, cfg.activation, cfg.use_bias_block), +))
    end

    output_layer = Dense(through_width => output_width, cfg.output_activation; bias=cfg.use_bias_out)

    return Chain(;
        input_layer,
        # bnin=bn(through_width),
        blocks...,
        # bnout=bn(through_width),
        output_layer
    )
end
bn(w) = BatchNorm(w)

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
        # return yhat, cc, (batch.y |> cpu)
        DrawUtil.draw!(:barplot, Bins.xs(), yhat; label="i-$(ind)")
    end
end

#endregion Check

end