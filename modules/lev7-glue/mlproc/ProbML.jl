module ProbML
using Flux, MLUtils, CUDA
using CudaUtil
using DrawUtil
using Dates

import Distributions as D

function actual(c1, c2, sigma)
    norm = D.MvNormal([0.0, 0.0], sigma)
    return hcat(D.pdf(norm, [c1,c2]))
end

function actual(m::AbstractMatrix)
    mapslices(m; dims=1) do x
        c1, c2, sigma = x
        actual(c1, c2, sigma)
    end
end

actual(v::Vector) = actual(v[1], v[2], v[3])

function calc_loss(model, x, y)
    # global kxy = (;x, y)
    yhat = model(x)
    # @show size(x) size(y) size(yhat)
    return Flux.Losses.mse(y, yhat)
end

function make_model(width)
    mult = 16
    h1 = mult * width
    h2 = 2 * mult * width
    h3 = 8 * mult * width
    h4 = mult * width
    # TODO: what about a prob per predictor? Then wouldn't have to multiply those together?
    out = 1 # prob for coords
    d1 = Dense(width => h1; bias=false) # TODO: try bias?
    d2 = Dense(h1 => h2, tanh; bias=false)
    # dr1 = Dropout(0.05)
    d3 = Dense(h2 => h3, tanh; bias=false)
    d4 = Dense(h3 => h4, relu; bias=false)
    d5 = Dense(h4 => out; bias=false)
    # mcpu = Chain(d1, d2, dr1, d3, d4, d5)
    mcpu = Chain(d1, d2, d3, d4, d5)
    println("Created model with param count: ", sum(length, Flux.params(mcpu)))
    return mcpu
end

function make_data_norm()
    numbatches = 100
    batchsize = 512
    width = 3
    function get_batch(batchi)::Union{Nothing,NamedTuple{(:x, :y), Tuple{CuArray{Float32, 2, CUDA.Mem.DeviceBuffer}, CuArray{Float32, 2, CUDA.Mem.DeviceBuffer}}}}
        batchi <= numbatches || return nothing
        # println("get_batch $(batchi)")
        m = Matrix{Float64}(undef, width, batchsize)
        for i in 1:batchsize
            sigma = 0.25 + rand() * 0.5
            c1 = rand() * 2 - 1
            c2 = rand() * 2 - 1
            m[1,i] = c1
            m[2,i] = c2
            m[3,i] = sigma
        end
        x = m |> gpu
        @assert x isa CuArray{Float32, 2, CUDA.Mem.DeviceBuffer} typeof(x)
        y = actual(m) |> gpu
        return (;x, y)
    end
    return (;get_batch, width=3, numpreds=1)
end

function setup()
    (;get_batch, width, numpreds) = make_data_norm()
    model = make_model(width) |> gpu
    opt = AdamW(1e-3)
    opt_state = Flux.setup(opt, model) |> gpu
    loss_untrained = 0.0
    for i in 1:3
        loss_untrained += calc_loss(model, get_batch(i)...) |> cpu
    end
    global kall = (;model, opt, opt_state, get_batch, numpreds, loss_untrained)
end

function run(;iters=10)
    (;model, opt_state, get_batch, loss_untrained) = kall
    train(model, opt_state, get_batch; iters, loss_untrained)
    # test(model, data_test; loss_untrained)
end

function train(model, opt_state, get_batch; iters=10, loss_untrained=1.0)
    last_save = now(UTC)
    for epoch in 1:iters
        loss_sum = 0.0
        i = 0
        batch = get_batch(i)
        while !isnothing(batch)
            (;x, y) = batch
            i += 1
            loss, grads = Flux.withgradient(calc_loss, model, x, y)
            loss /= size(x)[end] * loss_untrained # Divide by batch size and "worst-case" loss
            loss_sum += loss
            Flux.update!(opt_state, model, grads[1])
            # println("Train loss batch #$(i): $(loss)")
            yield()
            # if (i % 10) == 0
            #     yhat = model(x)
            #     if sum(abs, yhat) < 0.1 * (seqlen - 2)
            #         println("ERROR: yhat reached too low: ", sumparams(model))
            #         return
            #     end
            # end
            batch = get_batch(i)
        end
        loss = loss_sum / i
        println("Train loss epoch #$(epoch): $(loss) from $(loss_sum) and $(i)")
        if (now(UTC) - last_save) >= Minute(30)
            # last_save = now(UTC)
            # checkpoint_save()
        end
        # push!(losses, loss)
    end
end

function grid(sigma)
    xs = collect(-1.0:0.1:1.0)
    ys = collect(-1.0:0.1:1.0)
    m = Array{Float64}(undef, 3, length(xs), length(ys))
    for x in eachindex(xs), y in eachindex(ys)
        m[:,x,y] = [xs[x],ys[y],sigma]
    end
    return m
end

function compare(sigma=0.5)
    test_show(sigma)
    show_norm(sigma)
end

function test_show(sigma)
    m = grid(sigma)
    # xs = m[1,:,1]
    # ys = m[2,1,:]
    len = size(m,2)*size(m,3)
    xs = reshape(m[1,:,:], len)
    ys = reshape(m[2,:,:], len)
    zs = reshape(cpu(kall.model(gpu(m))), len)
    draw(:surface, xs, ys, zs; axis=(type=Makie.Axis3,), colormap=:viridis)
end

function show_norm(sigma)
    xs = Float64[]
    ys = Float64[]
    zs = Float64[]
    pts = []
    for x in -1.0:0.01:1.0, y in -1.0:0.01:1.0
        # m[:,1] = [x,y,sigma]
        # prob = actual(hcat([x; y; sigma]))
        prob = actual([x; y; sigma])[1,1]
        push!(pts, (x, y, prob))
        push!(xs, x)
        push!(ys, y)
        push!(zs, prob)
    end
    # draw(:surface, xs, ys, zs; axis=(type=Makie.Axis3,))
    draw!(:surface, xs, ys, zs; colormap=:tokyo)
    # Makie.surface!(xs, ys, zs; colormap=:twilight)
end

end