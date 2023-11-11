module ProbML
using Flux, MLUtils, CUDA
using CudaUtil
using DrawUtil
using Dates

CUDA.allowscalar(false)
dev(x) = gpu(x)

hypers() = (;
    learning_rate = 1e-3,
    activation_func = swish,
    init_func = Flux.glorot_uniform, #Flux.rand32,
    batch_size = 2^8, # 2^16,
    obs_count = 200000,
    num_bins = 200,
    ret_min = -0.15,
    ret_max = 0.15,
    k = 0.0001
)

function calc_loss(model, x, y)
    # (;yhat) = run_model(model, x)
    # l = Flux.Losses.crossentropy(yhat, y; dims=1, agg=sum)
    # return l

    # (;penalty_neg, penalty_norm) = penalties(yhat_raw)
    # l = Flux.Losses.mae(yhat, y)

    l = Flux.Losses.logitcrossentropy(model(x), y; dims=1)
    return l

    # return l + penalty_neg + penalty_norm

    # # return Flux.Losses.logitcrossentropy(yhat, y)
    # (;penalty_neg, penalty_norm) = penalties(yhat_raw)
    # l = Flux.Losses.logitcrossentropy(yhat, y)
    # @show penalty_neg penalty_norm l
    # return l + penalty_neg + penalty_norm
end

cap(x) = x < 0 ? zero(x) : x

function run_model(model, x)
    yhat_raw = model(x)
    # yhat2 = cap.(yhat_raw)
    # yhat = yhat2 ./ sum(yhat2)

    # yhat = yhat_raw
    yhat = Flux.softmax(yhat_raw; dims=1)
    # yhat = Flux.softmax(abs.(yhat_raw); dims=1)
    return (;yhat, yhat_raw)
end

function penalties(yhat_raw)
    sz = size(yhat_raw, 2)
    # println(size(yhat_raw)) : (200, 1024)
    penalty_neg = sum(abs.(yhat_raw) .- yhat_raw) / sz
    penalty_norm = sum(abs.(1.0 .- sum(yhat_raw; dims=1))) / sz
    return (;penalty_neg, penalty_norm)
end

function make_model(input_width, output_width)
    width = input_width
    mult = 16
    h1 = mult * width
    h2 = 2 * mult * width
    h3 = 8 * mult * width
    h4 = mult * width

    (;init_func) = hypers()
    activation_func = swish

    d1 = Dense(input_width => h1, activation_func; init=init_func, bias=true)

    d2 = Dense(h1 => h2, activation_func; init=init_func, bias=false)
    d3 = Dense(h2 => h3, activation_func; init=init_func, bias=false)
    d4 = Dense(h3 => h4, activation_func; init=init_func, bias=false)
    skip = SkipConnection(Chain(d2, d3, d4), +)

    d5 = Dense(h4 => output_width, activation_func; init=init_func, bias=false)

    mcpu = Chain(d1, skip, d5)
    println("Created model with param count: ", sum(length, Flux.params(mcpu)))
    return mcpu
end

function make_model_dense(input_width, output_width)
    width = input_width
    mult = 16
    h1 = mult * width
    h2 = 2 * mult * width
    h3 = 8 * mult * width
    h4 = mult * width

    (;activation_func, init_func) = hypers()

    d1 = Dense(input_width => h1, activation_func; init=init_func, bias=true) # TODO: try bias?
    d2 = Dense(h1 => h2, activation_func; init=init_func, bias=false)
    # dr1 = Dropout(0.05)
    d3 = Dense(h2 => h3, activation_func; init=init_func, bias=false)
    d4 = Dense(h3 => h4, activation_func; init=init_func, bias=false)
    d5 = Dense(h4 => output_width, activation_func; init=init_func, bias=false)
    mcpu = Chain(d1, d2, d3, d4, d5)
    println("Created model with param count: ", sum(length, Flux.params(mcpu)))
    return mcpu
end

function run(;iters=10, start_batch=1)
    (;model, opt_state, get_batch) = kall
    train(model, opt_state, get_batch; iters, start_batch)
    # test(model, data_test; loss_untrained)
end

function train(model, opt_state, get_batch; iters=10, start_batch=1)
    last_save = now(UTC)
    (;batch_size) = hypers()
    loss_prev = calc_base_loss(model, get_batch)
    for epoch in 1:iters
        loss_sum = 0.0
        i = start_batch
        batch = get_batch(i)
        while !isnothing(batch)
            (;x, y) = batch
            loss_batch, grads = Flux.withgradient(m -> calc_loss(m, x, y), model)
            loss = loss_batch / batch_size
            loss_sum += loss
            Flux.update!(opt_state, model, grads[1])
            # println("Train loss batch #$(i): $(loss) from $(loss_raw)")
            yield()
            # if (i % 10) == 0
            #     yhat = model(x)
            #     if sum(abs, yhat) < 0.1 * (seqlen - 2)
            #         println("ERROR: yhat reached too low: ", sumparams(model))
            #         return
            #     end
            # end
            i += 1
            batch = get_batch(i)
        end
        loss_epoch = loss_sum / (i-1)
        println("After $(i-1) batches, epoch $(epoch) average loss is $(loss_epoch) which is $(loss_epoch / loss_prev) * loss_prev")
        loss_prev = loss_epoch
        if (now(UTC) - last_save) >= Minute(30)
            # last_save = now(UTC)
            # checkpoint_save()
        end
        # push!(losses, loss)
    end
end

#region TSX
function setup()
    (;get_batch, input_width, output_width, obs, ys) = make_data()
    model = make_model(input_width, output_width) |> dev
    opt = AdamW(hypers().learning_rate)
    opt_state = Flux.setup(opt, model) |> dev
    loss_untrained = calc_base_loss(model, get_batch)
    global kall = (;model, opt, opt_state, get_batch, loss_untrained, obs, ys)
end

function calc_base_loss(model, get_batch)
    count = 10
    loss = 0.0
    for i in 1:count
        loss += calc_loss(model, get_batch(-1)...)
    end
    loss /= count
    loss /= hypers().batch_size
    return loss
end

import DataFiles as dat
import DataFrames as DF
function make_data(; ts=DateTime("2020-01-03T21:00:00"))
    (;obs_count, batch_size) = hypers()
    tsx = dat.get_tsx_for_prob()
    obs = collect(Matrix(DF.select(tsx, :tex => (x -> x ./ 1000) => :tex, [:extrin, :tex] => ((x, t) -> x ./ sqrt.(t) / 0.05) => :vol)[1:obs_count,:])')
    ys = tsx.ret[1:obs_count] .- 1.0
    output_width = hypers().num_bins
    # obs_cols = collect(eachcol(obs))
    input_width = size(obs, 1)
    batch_index_max = div(obs_count, batch_size) # - batch_size
    xbuf = Matrix{Float32}(undef, input_width, batch_size)
    ybuf = Matrix{Float32}(undef, output_width, batch_size)
    function get_batch(batch_index)
        if batch_index == -1
            batch_index = rand(1:batch_index_max)
        end
        batch_index <= batch_index_max || return nothing
        start_index = (batch_index - 1) * batch_size
        for i in 1:batch_size
            ind = start_index + i
            xbuf[:,i] = obs[:,ind]
            ybuf[:,i] = calc_y(ys[ind])
        end
        x = xbuf |> dev
        # y = Flux.onehotbatch(getindex.(vec(argmax(ybuf; dims=1)), 1), 1:200) |> dev
        y = calc_ybatch(ybuf) |> dev
        # y = ybuf |> dev
        return (;x, y)
    end
    return (;get_batch, input_width, output_width, obs, ys)
end

import VectorCalcUtil:normalize!,normalize
calc_y(y) = normalize!(kernel.(y, bins()))
calc_ybatch(y) = Flux.onehotbatch(getindex.(vec(argmax(y; dims=1)), 1), axes(y, 1))

function kernel(y, bin)
    k = hypers().k
    dy = bin - y
    if y * bin > 0
        return exp(-(dy^2) / k)
    else
        # Opposite sides, lower k
        return exp(-(dy^2) / (k / 10))
    end
end
#endregion TSX

#region Test
test_calc_loss() = calc_loss(kall.model, kall.get_batch(1)...)

import DrawUtil
function test_show_kernel(y)
    ys = calc_y(y)
    draw(:barplot, bins(), ys)
end

function yyhat(model, x, y)
    # yhat_raw=model(x |> dev) |> cpu
    return (;y = calc_ybatch(calc_y(y)), (run_model(model, x |> dev) |> cpu)...)
end

function test1(ind=1)
    (;y, yhat, yhat_raw) = yyhat(kall.model, kall.obs[:,ind], kall.ys[ind])
    println(penalties(yhat_raw))
    return (;y, yhat, yhat_raw)
end

function show(ind)
    xs = bins()
    (;y, yhat, yhat_raw) = yyhat(kall.model, kall.obs[:,ind], kall.ys[ind])
    println(penalties(yhat_raw))
    draw(:barplot, xs, vec(yhat); label="yhat")
    # draw!(:barplot, xs, vec(yhat_raw); label="yhat_raw")
    draw!(:barplot, xs, vec(y); label="y")
    return (;y, yhat, yhat_raw)
end

function show_comp(param_ind; other=0.5) # 1 = tex, 2 = vol
    xs = bins()
    draw(:vlines, 0.0)
    for val in 0.1:0.1:0.9
        x = param_ind == 1 ? [val,other] : [other,val]
        yhat = run_model(kall.model, x |> dev).yhat |> cpu
        # yhat = map(x -> -log(x), yhat)
        draw!(:barplot, xs, vec(yhat); label="yhat-$(val)")
    end
end
#endregion

#region Util
function precalc_bins()
    (;num_bins, ret_min, ret_max) = hypers()
    bin_width = (ret_max - ret_min) / num_bins
    return [ret_min + bin_width * i for i in 1:num_bins]
end
bins() = BINS2[]
const BINS2 = Ref(precalc_bins())
#endregion

end