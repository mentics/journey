module MLTrainDist
using Dates, IterTools
using Flux, NNlib, MLUtils, CUDA
using CudaUtil
import Distributions

function make_model(hypers)
    Chain(
        Dense(hypers.inputwidth => hypers.hiddenwidth, hypers.activation; bias=true),
        Dense(hypers.hiddenwidth => hypers.hiddenwidth, hypers.activation; bias=true),
        Dense(hypers.hiddenwidth => hypers.outputwidth, hypers.outputactivation; bias=true)
    )
end

function calcloss(model, batch)
    yhat = model(batch.x)
    y = batch.y
    return Flux.Losses.mse(yhat, y)
end

function hypers()
    inputwidth = 1
    hiddenwidth = 10000
    outputwidth = 1000
    return (;
        inputwidth,
        hiddenwidth,
        outputwidth,
        activation = NNlib.gelu,
        outputactivation = NNlib.sigmoid,
    )
end

function info()
    batchlen = 256
    numsamples = 8192
    numbatches = round(Int, numsamples / batchlen, RoundUp)
    return (;
        batchlen, numsamples, numbatches
    )
end

function make_batcher()
    (;outputwidth) = hypers()
    binposs = [2 * i / outputwidth for i in 1:outputwidth]
    global kbinposs = binposs
    distri = Distributions.Normal(1.0, 0.33)
    (;batchlen, numbatches) = info()
    batch1 = make_batch(binposs, batchlen, fill(1.0, batchlen)) |> gpu
    batchgpu[] = batch1
    return function(batchi)
        batchi <= floor(numbatches * 0.8) || return nothing
        return make_batch(binposs, batchlen, rand(distri, batchlen)) |> gpu
        # return batch1
    end
end

const batchgpu = Ref{NamedTuple{(:x, :y), Tuple{CUDA.CuArray{Float32, 2, CUDA.Mem.DeviceBuffer}, CUDA.CuArray{Float32, 2, CUDA.Mem.DeviceBuffer}}}}()

function make_batch(binposs, batchlen, xs)
    x = reshape(xs, 1, batchlen)
    y = stack(map(xs) do xi
        yforx(binposs, xi)
    end)
    return (;x, y)
end

# yforx(binposs, x) = [1 / ℯ^(64*abs(x - bp)) for bp in binposs]
yforx(binposs, x) = [abs(x - bp) < (binposs[1]) ? 1.0 : 0.0 for bp in binposs]

function run(; iters=10)
    modelcpu = make_model(hypers())
    global kmodelcpu = modelcpu
    model = modelcpu |> gpu
    global kmodelgpu = model
    opt = AdamW(1e-4)
    opt_state = gpu(Flux.setup(opt, model))
    global kopt = opt
    global kopt_state = opt_state

    train(make_batcher(), model, opt_state; iters)
end

function train(batcher, model, opt_state; iters=10)
    # lossbase = calclossbase(batcher(1))
    # println("lossbase: ", lossbase)
    for epoch in 1:iters
        loss_sum = 0.0
        i = 1
        x = batcher(i) # |> gpu
        while !isnothing(x)
            # ls, grads = Flux.withgradient(calcloss, model, x)
            # # ls /= size(x)[end] * lossbase
            # Flux.update!(opt_state, model, grads[1])
            # println("Train loss batch #$(i): $(ls)")
            ls = trainbatch(model, opt_state, x)
            println("Train loss batch #$(i): $(ls)")
            loss_sum += ls
            yield()
            i += 1
            # if (i % 10) == 0
            #     yhat = model(x)
            #     if sum(abs, yhat) < 0.1 * (seqlen - 2)
            #         println("ERROR: yhat reached too low: ", sumparams(model))
            #         return
            #     end
            # end
            x = batcher(i) |> gpu
        end
        loss = loss_sum / i
        println("Train loss epoch #$(epoch): $(loss)")
        # if (now(UTC) - last_save) >= Minute(30)
        #     last_save = now(UTC)
        #     checkpoint_save()
        # end
        # push!(losses, loss)
    end
end

function trainbatch(model, opt_state, x)
    ls, grads = Flux.withgradient(calcloss, model, x)
    # ls /= size(x)[end] * lossbase
    Flux.update!(opt_state, model, grads[1])
    return ls
end

function train_continue(model=kmodelgpu, opt_state=kopt_state; iters=10)
    global kmodelgpu = gpu(model)
    global kopt_state = gpu(opt_state)
    train(make_batcher(), kmodelgpu, kopt_state; iters)
end

end