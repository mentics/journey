module ProbML
using Flux, MLUtils, CUDA
using CudaUtil
using DrawUtil
using Dates

import Distributions as D

import SliceMap as SM

dev(x) = cpu(x)

struct ModelWithExtra{L,D}
    layers::L
    extra::D
end
Flux.@functor ModelWithExtra

(m::ModelWithExtra)(x) = m.layers(x)

function make_model(width)
    mult = 16
    h1 = mult * width
    h2 = 2 * mult * width
    h3 = 8 * mult * width
    h4 = mult * width
    # TODO: what about a prob per predictor? Then wouldn't have to multiply those together?
    act = swish
    out = 1 # prob for coords
    d1 = Dense(width => h1, act; bias=true) # TODO: try bias?
    d2 = Dense(h1 => h2, act; bias=false)
    # dr1 = Dropout(0.05)
    d3 = Dense(h2 => h3, act; bias=false)
    d4 = Dense(h3 => h4, act; bias=false)
    d5 = Dense(h4 => out, act; bias=false)
    # mcpu = Chain(d1, d2, dr1, d3, d4, d5)
    # kernel_params = Dense(width => width; bias=false)
    kernel_params = fill(0.01, width)
    mcpu = ModelWithExtra(Chain(d1, d2, d3, d4, d5), kernel_params)
    println("Created model with param count: ", sum(length, Flux.params(mcpu)))
    return mcpu
end

function run(;iters=10)
    (;model, opt_state, get_batch, loss_untrained) = kall
    train(model, opt_state, get_batch; iters, loss_untrained)
    # test(model, data_test; loss_untrained)
end

function train(model, opt_state, get_batch; iters=10, loss_untrained=1.0)
    last_save = now(UTC)
    println("Starting training")
    for epoch in 1:iters
        loss_sum = 0.0
        i = 1
        batch = get_batch(i)
        while !isnothing(batch)
            (;x, y) = batch
            # @assert x isa CuArray
            # @assert y isa CuArray
            loss, grads = Flux.withgradient(m -> calc_loss(m, x, y), model)
            # loss, grads = Flux.withgradient((model, x, y) -> sum(x .* model.extra), model, x, y)
            loss /= size(x)[end] * loss_untrained # Divide by batch size and "worst-case" loss
            loss_sum += loss
            Flux.update!(opt_state, model, grads[1])
            println("Train loss batch #$(i): $(loss)")
            yield()
            println("k: ", model.extra)
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
        loss = loss_sum / (i-1)
        println("Train loss epoch #$(epoch): $(loss) from $(loss_sum) and $(i-1)")
        if (now(UTC) - last_save) >= Minute(30)
            # last_save = now(UTC)
            # checkpoint_save()
        end
        # push!(losses, loss)
    end
end

function setup()
    (;get_batch, width, obs) = make_data()
    model = make_model(width) |> dev
    opt = AdamW(1e-3)
    opt_state = Flux.setup(opt, model) |> dev
    base_loss_count = 3
    loss_untrained = 0.0
    for i in 1:base_loss_count
        loss_untrained += calc_loss(model, get_batch(i)...) |> cpu
    end
    loss_untrained /= base_loss_count
    global kall = (;model, opt, opt_state, get_batch, loss_untrained, obs)
end


test_calc_loss() = calc_loss(kall.model, kall.get_batch(1)...)

function calc_loss(model, x_batch, obs)
    # global kloss = (;model, x_batch, obs, k, y, yhat)
    # @show size(x_batch) size(obs)
    # k = model.extra
    # @show typeof(y) typeof(yhat)
    # @show size(y) size(yhat)

    # k = [0.01, 0.01, 0.01] ./ 10
    (;y, yhat) = yyhat(model, x_batch, model.extra, obs)
    return Flux.Losses.mse(y, yhat)
    # return Flux.Losses.mae(y, yhat)
end

function test1(x=kall.get_batch(1).x)
    (;y, yhat) = yyhat(kall.model, x, kall.model.extra, kall.obs) |> cpu
end

#region Test Normal
module NormDist

function norm_setup()
    kernel = make_kernel(;mults=[0.33, 0.77])
    (;get_batch, width, numpreds) = make_data_norm(kernel)
    model = make_model(width) |> dev
    opt = AdamW(1e-3)
    opt_state = Flux.setup(opt, model) |> dev
    loss_untrained = 0.0
    for i in 1:3
        loss_untrained += calc_loss(model, get_batch(i)...) |> cpu
    end
    global kall = (;kernel, model, opt, opt_state, get_batch, numpreds, loss_untrained)
end

function norm_make_kernel(;mults)
    return function(x)
        coords = x[1:2]
        sigmas = x[3:4] .* mults
        means = zeros(size(sigmas,1))
        norm = D.MvNormal(means, sigmas)
        return hcat(D.pdf(norm, coords))
    end
end

function norm_apply_kernel(kernel, m::AbstractMatrix)
    SM.mapcols(m) do x
        kernel(x)
    end
    # mapslices(m; dims=1) do x
    #     kernel(x)
    # end
end

function make_data_norm(kernel)
    numbatches = 100
    batchsize = 1024
    numpreds = 2
    width = 2 + numpreds
    function get_batch(batchi)
        batchi <= numbatches || return nothing
        # println("get_batch $(batchi)")
        m = Matrix{Float32}(undef, width, batchsize)
        for i in 1:batchsize
            sigma1 = 0.001 + rand() * 5.0
            sigma2 = 0.001 + rand() * 5.0
            c1 = rand() * 2 - 1
            c2 = rand() * 2 - 1
            m[:,i] = [c1, c2, sigma1, sigma2]
        end
        x = m |> dev
        # @assert x isa CuArray{Float32, 2, CUDA.Mem.DeviceBuffer} typeof(x)
        y = norm_apply_kernel(kernel, m) |> dev
        return (;x, y)
    end
    return (;get_batch, width, numpreds)
end

function compare(sigmas=[0.5,0.5])
    test_show(sigmas)
    show_norm(sigmas)
end

function test_show(sigmas)
    m = grid(sigmas)
    # xs = m[1,:,1]
    # ys = m[2,1,:]
    len = size(m,2)*size(m,3)
    xs = reshape(m[1,:,:], len)
    ys = reshape(m[2,:,:], len)
    zs = reshape(cpu(kall.model(dev(m))), len)
    draw(:surface, xs, ys, zs; axis=(type=Makie.Axis3,), colormap=:viridis)
end

function show_norm(sigmas)
    xs = Float32[]
    ys = Float32[]
    zs = Float32[]
    pts = []
    for x in -1.0:0.01:1.0, y in -1.0:0.01:1.0
        # m[:,1] = [x,y,sigma]
        # prob = actual(hcat([x; y; sigma]))
        prob = kall.kernel([x, y, sigmas...])[1,1]
        push!(pts, (x, y, prob))
        push!(xs, x)
        push!(ys, y)
        push!(zs, prob)
    end
    # draw(:surface, xs, ys, zs; axis=(type=Makie.Axis3,))
    draw!(:surface, xs, ys, zs; colormap=:tokyo)
    # Makie.surface!(xs, ys, zs; colormap=:twilight)
end

function grid(sigmas)
    xs = collect(-1.0:0.1:1.0)
    ys = collect(-1.0:0.1:1.0)
    m = Array{Float32}(undef, 2 + length(sigmas), length(xs), length(ys))
    for x in eachindex(xs), y in eachindex(ys)
        m[:,x,y] = [xs[x],ys[y],sigmas...]
    end
    return m
end

end
#endregion Test Normal

end