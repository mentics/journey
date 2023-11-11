module ProbMLCoord
using Flux, MLUtils, CUDA
using CudaUtil
using DrawUtil
using Dates

import Distributions as D

import SliceMap as SM

CUDA.allowscalar(false)
# dev(x) = cpu(x)
dev(x) = gpu(x)

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
    kernel_params = fill(0.001, width)
    mcpu = ModelWithExtra(Chain(d1, d2, d3, d4, d5), kernel_params)
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
    for epoch in 1:iters
        loss_base = calc_base_loss(model, get_batch)
        println("Base loss for epoch $(epoch): $(loss_base)")
        loss_sum = 0.0
        i = start_batch
        batch = get_batch(i, model.extra)
        while !isnothing(batch)
            (;x, y) = batch
            # @assert x isa CuArray
            # @assert y isa CuArray
            loss, grads = Flux.withgradient(m -> calc_loss(m, x, y), model)
            # loss, grads = Flux.withgradient((model, x, y) -> sum(x .* model.extra), model, x, y)
            loss /= size(x)[end] * loss_base # Divide by batch size and initial loss
            loss_sum += loss
            Flux.update!(opt_state, model, grads[1])
            println("Train loss batch #$(i): $(loss)")
            yield()
            # println("k: ", model.extra)
            # if (i % 10) == 0
            #     yhat = model(x)
            #     if sum(abs, yhat) < 0.1 * (seqlen - 2)
            #         println("ERROR: yhat reached too low: ", sumparams(model))
            #         return
            #     end
            # end
            i += 1
            batch = get_batch(i, model.extra)
        end
        loss = loss_sum / (i-1)
        println("Average train loss epoch #$(epoch): $(loss) from $(loss_sum) and $(i-1)")
        if (now(UTC) - last_save) >= Minute(30)
            # last_save = now(UTC)
            # checkpoint_save()
        end
        # push!(losses, loss)
    end
end

#region Test TSX
function setup()
    (;get_batch, width, obs, k) = make_data()
    model = make_model(width) |> dev
    opt = AdamW(1e-3)
    opt_state = Flux.setup(opt, model) |> dev
    loss_untrained = calc_base_loss(model, get_batch)
    global kall = (;model, opt, opt_state, get_batch, loss_untrained, obs, k)
end

function calc_base_loss(model, get_batch)
    count = 10
    loss = 0.0
    for i in 1:count
        loss += calc_loss(model, get_batch(-1, model.extra)...) |> cpu
    end
    loss /= count
    return loss
end

# ret is coord; tex, vol are predictors
import ProbMultiKde as pmk
function make_data(; ts=DateTime("2020-01-03T21:00:00"))
    kde = pmk.get_kde(ts)
    batchsize = 64
    # obs = reduce(hcat, [[ob.ret, ob.tex, ob.vol] for ob in kde.obs[1:2*batchsize]])
    obs = reduce(hcat, [[ob.ret, ob.tex, ob.vol] for ob in kde.obs])
    obs_cols = collect(eachcol(obs))
    width = size(obs_cols[1], 1)
    k = fill(0.001, 3)
    # numcoords = 1
    # numpreds = 2
    # width = numcoords + numpreds
    # m = Matrix{Float32}(undef, width, batchsize)
    grid_len = 64
    xbuf = Matrix{Float32}(undef, width, grid_len * batchsize)
    ybuf = Matrix{Float32}(undef, 1, size(xbuf, 2))
    function get_batch(batchi, k_model)
        if batchi == -1
            batchi = rand(1:div(length(obs_cols), grid_len))
        end
        # println("getting batch $(batchi)")
        batchi <= (size(obs, 2) - batchsize) || return nothing
        for i in 1:batchsize
            # ((batchi - 1) * batchsize + 1):(batchi * batchsize)
            ob = obs_cols[(batchi - 1) * batchsize + i]
            make_grid(xbuf, (i - 1) * grid_len, grid_len, ob)
        end
        x = xbuf |> dev
        kernel_batch3(ybuf, xbuf, k, obs_cols)
        y = ybuf |> dev
        return (;x, y)
    end
    return (;get_batch, width, obs=obs_cols, k)
end

function make_grid(m, ind_offset, grid_len, coord)
    width = size(coord, 1)
    grid_width = 1.0 / (grid_len + 1)
    grid = [i * grid_width for i in 1:grid_len]
    #  collect(grid_width:grid_width:(1.0 - grid_width))
    ad = mod(coord[1], grid_width)
    # @show grid_width coord ad
    # @show ind_offset eachindex(grid)
    for i in eachindex(grid)
        g = grid[i]
        dist = abs(g - coord[1]) / 4
        m[:, ind_offset + i] = [g + ad, (coord[2:end] .+ (dist .* randoff(width-1, dist)))...]
    end
    return m
end

randoff(width, off) = rand(width) .* off .- (off/2)

# function make_kernel(sigmas)
#     # norm = D.MvNormal(zeros(length(sigmas)), sigmas)
#     return function(actual, x)
#         # D.pdf(norm, x .- actual)
#         pmk.kerk(actual, x, sigmas)
#     end
# end

# @inline function direct_kernel(actual, x, k)
#     @assert size(actual) == (3,)
#     @assert size(x) == (3,)
#     @assert size(k) == (3,)
#     return sum(exp.(-elmult(x .- actual) ./ k))
# end

# function xtobs(m, k, obs)
#     prod(SM.mapcols(m) do x
#         sum(SM.mapcols(obs) do ob
#             diffed_kernel(x .- ob, k)
#         end; dims=2) ./ size(obs,2)
#     end; dims=1)
# end

# function kernel_batch(m, k, obs)
#     m2 = stack(map(eachcol(m)) do x
#         SM.mapcols(obs) do ob
#             -((x .- ob).^2)
#         end
#     end)
#     k1 = k .+ 0.01
#     k2 = k1.^2 ./ sum(k1)
#     m3 = exp.(m2 ./ k2)
#     m4 = dropdims(sum(m3; dims=2); dims=2) ./ size(obs, 2)
#     if any(!isfinite, k2 |> cpu)
#         error("k bad stuff")
#     end
#     if any(!isfinite, m4 |> cpu)
#         error("m bad stuff")
#     end
#     if any(iszero, k2 |> cpu) #  || any(iszero, m4 |> cpu)
#         error("zero stuff")
#     end
#     m5 = prod(m4; dims=1) # this gives scalar index error when k goes bad (inf or nan?)
#     # sum(m4; dims=1)
#     return m5
# end

@inline function diffed_kernel(dx, k)
    # @assert size(dx) == (3,)
    # @assert size(k) == (3,)
    return exp.(-(dx.^2) ./ k)
end

@inline function kernel1(dx, k)
    return exp(-(dx^2) / k)
end

@inline prod3(x) = x[1] * x[2] * x[3]
@inline kern(dx, k) = prod3(diffed_kernel(dx, k))

@inline dist2(c, x) = (x[1] - c[1])^2 + (x[2] - c[2])^2 + (x[3] - c[3])^2
@inline dist2(c, x1, x2, x3) = (x1 - c[1])^2 + (x2 - c[2])^2 + (x3 - c[3])^2
# function kernel_batch2(m, k, obs_v)
#     global kkargs = (;m, k, obs_v)
#     return mapslices(m; dims=1) do x
#         (_, ind) = findmin(ob -> dist2(ob, x), obs_v)
#         kern(x .- obs_v[ind], k)
#     end
# end

function kernel_batch3(res, m, k, obs_v)
    global kkargs = (;res, m, k, obs_v)
    for i in axes(m, 2)
        x1 = m[1,i]
        x2 = m[2,i]
        x3 = m[3,i]
        (_, ind) = findmin(ob -> dist2(ob, x1, x2, x3), obs_v)
        nearest = obs_v[ind]
        y1 = kernel1(x1 - nearest[1], k[1])
        y2 = kernel1(x1 - nearest[1], k[2])
        y3 = kernel1(x1 - nearest[1], k[3])
        res[i] = y1 * y2 * y3
    end
    return
end

function use_kernel(m, k, obs_v)
    res = Matrix{Float32}(undef, 1, size(m, 2))
    kernel_batch3(res, m, k, obs_v)
    return res
end

test_calc_loss() = calc_loss(kall.model, kall.get_batch(1)...)

function calc_loss(model, x, y)
    # global kloss = (;model, x_batch, obs, k, y, yhat)
    # @show size(x_batch) size(obs)
    # k = model.extra
    # @show typeof(y) typeof(yhat)
    # @show size(y) size(yhat)

    # k = [0.01, 0.01, 0.01] ./ 10
    # (;y, yhat) = yyhat(model, x, model.extra, obs)
    yhat = model(x)
    return Flux.Losses.mse(y, yhat)
    # return Flux.Losses.mae(y, yhat)
end

@inline function yyhat(model, x_batch, k, obs)
    return (;y = use_kernel(x_batch |> cpu, k, obs), yhat=model(x_batch |> dev))
end

# function test1(x=kall.get_batch(1).x)
#     (;y, yhat) = yyhat(kall.model, x, kall.model.extra, kall.obs) |> cpu
# end

function show(preds)
    preds = vec32(preds)
    rets = vec32(0.0:0.01:1.0)
    m = Matrix{Float32}(undef, 3, length(rets))
    for i in eachindex(rets)
        m[:,i] = [rets[i], preds...]
    end
    (;y, yhat) = yyhat(kall.model, m, kall.k, kall.obs)
    yhat = yhat |> cpu
    # heights = cpu(kall.model(dev(m)))
    draw(:barplot, rets, vec(y); label="y")
    draw!(:barplot, rets, vec(yhat); label="yhat")
    return (;y,yhat)
end
#endregion Test TSX

#region Util
vec32(v) = collect(Float32, v)
#endregion

end