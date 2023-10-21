module ProbML
using Flux, MLUtils, CUDA
using CudaUtil
using DrawUtil

import Distributions as D

function actual(x)
    c1, c2, sigma = x
    norm = D.MvNormal([0.0, 0.0], sigma)
    return D.pdf(norm, [c1,c2])
end

function calc_loss(model, xy)
    (;x, y) = xy
    yhat = model(x)
    return Flux.Losses.mse(y, yhat)
end

function make_model(width)
    mult = 4
    h1 = mult * width
    h2 = 2 * mult * width
    h3 = 8 * mult * width
    h4 = mult * width
    # TODO: what about a prob per predictor? Then wouldn't have to multiply those together?
    out = 1 # prob for coords
    d1 = Dense(width => h1; bias=false) # TODO: try bias?
    d2 = Dense(h1 => h2, tanh; bias=false)
    dr1 = Dropout(0.05)
    d3 = Dense(h2 => h3, tanh; bias=false)
    d4 = Dense(h3 => h4, relu; bias=false)
    d5 = Dense(h4 => out; bias=false)
    mcpu = Chain(d1, d2, dr1, d3, d4, d5)
    println("Created model with param count: ", sum(length, Flux.params(mcpu)))
    return mcpu
end

function make_data_norm()
    batchsize = 32
    function get_batch(batchi)::NamedTuple{(:x, :y), Tuple{Matrix, Matrix}}
        x = Matrix(undef, width, batchsize)
        for i in 1:batchsize
            sigma = rand() * 10
            c1 = rand() * 2 - 1
            c2 = rand() * 2 - 1
            x[1,i] = c1
            x[2,i] = c2
            x[3,i] = sigma
        end
        y = actual(x)
        return (;x, y)
    end
    return (;get_batch, width=3, numpreds=1)
end

function run(make_new=false; iters=10)
    if make_new
        (;get_batch, width) = make_data_norm()
        model = make_model(width) |> gpu
        opt = AdamW(1e-3)
        opt_state = Flux.setup(opt, model) |> gpu
        global kall = (;model, opt, opt_state, get_batch, numpreds, loss_untrained)
    else
        (;model, opt_state, get_batch, loss_untrained) = kall
    end

    train(model, opt_state, get_batch; iters, loss_untrained)
    # test(model, data_test; loss_untrained)
end

function train(model, opt_state, get_batch; iters=10, loss_untrained=1.0)
    last_save = now(UTC)
    for epoch in 1:iters
        loss_sum = 0.0
        i = 0
        x = get_batch(i) |> gpu
        while !isnothing(x)
            i += 1
            loss, grads = Flux.withgradient(calc_loss, model, x)
            loss /= size(x)[end] * loss_untrained # Divide by batch size and "worst-case" loss
            loss_sum += loss
            Flux.update!(opt_state, model, grads[1])
            println("Train loss batch #$(i): $(loss)")
            yield()
            # if (i % 10) == 0
            #     yhat = model(x)
            #     if sum(abs, yhat) < 0.1 * (seqlen - 2)
            #         println("ERROR: yhat reached too low: ", sumparams(model))
            #         return
            #     end
            # end
            x = get_batch(i) |> gpu
        end
        loss = loss_sum / i
        println("Train loss epoch #$(epoch): $(loss)")
        if (now(UTC) - last_save) >= Minute(30)
            # last_save = now(UTC)
            # checkpoint_save()
        end
        # push!(losses, loss)
    end
end

function test_show(sigma)
    pts = []
    m = Matrix(undef, 3, 1)
    for x in -1.0:0.01:1.0, y in -1.0:0.01:1.0
        m[:,1] = [x,y,sigma]
        prob = kall.model(m)
        push!(pts, [x, y, prob])
    end
    draw(:surface, pts)
end

import Makie
function show_norm(sigma)
    xs = Float64[]
    ys = Float64[]
    zs = Float64[]
    pts = []
    for x in -1.0:0.01:1.0, y in -1.0:0.01:1.0
        # m[:,1] = [x,y,sigma]
        prob = actual([x,y,sigma])
        push!(pts, (x, y, prob))
        push!(xs, x)
        push!(ys, y)
        push!(zs, prob)
    end
    # draw(:surface, xs, ys, zs; axis=(type=Makie.Axis3,))
    Makie.surface(xs, ys, zs)
end

end