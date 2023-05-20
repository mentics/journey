module LyzeT4
using Flux, MLUtils, CUDA
using CudaUtil
import Trade4Data as T4

const BatchSize = Ref(128)
const LossClassWeights = Ref([0.25f0, 1.0f0] |> gpu)

function compare_random(batch_size=BatchSize[])
    rnd1 = rand(2, batch_size)
    rnd2 = stack(Flux.onehot.(rand(1:2, batch_size), Ref([1,2])))
    loss_random = Flux.Losses.logitcrossentropy(rnd1, rnd2)
    loss_zeros = Flux.Losses.logitcrossentropy(zeros(2, batch_size), rnd2)
    loss_ones = Flux.Losses.logitcrossentropy(ones(2, batch_size), rnd2)
    @show loss_random loss_zeros loss_ones
    return
end

function model_loss(model, x, y)
    yhat = model(x)
    # return Flux.Losses.logitcrossentropy(yhat, Flux.label_smoothing(y, .2)) # Did much worse training on 10 epochs

    # The following after 10 epochs had:
    #   (falsepos = 2188, falseneg = 5153, truepos = 32489, trueneg = 98521)
    # return Flux.Losses.logitcrossentropy(yhat, y)

    # For LossClassWeights = [0.25f0, 1.0f0] The following after 10 epochs had:
    #   (falsepos = 36, falseneg = 14840, truepos = 22802, trueneg = 100673)
    # For LossClassWeights = [1.0f0, 0.25f0] The following after 10 epochs had:
    #   (falsepos = 65325, falseneg = 55, truepos = 37587, trueneg = 35384)
    yhat2 = LossClassWeights[] .* yhat
    return Flux.Losses.logitcrossentropy(yhat2, y)
end

function make_model(len)
    h1 = 8 * len
    h2 = 4 * len
    h3 = 2 * len
    out = 2 # output is one-hot yes/no
    d1 = Dense(len => h1; bias=false)
    d2 = Dense(h1 => h2; bias=false)
    # d3 = Dense(h2 => h3; bias=false)
    d4 = Dense(h2 => out; bias=false)
    mcpu = Chain(d1, d2, d4)
    println("Created model with param count: ", sum(length, Flux.params(mcpu)))
    return mcpu |> gpu
end

function run(make_new=false; iters=10)
    if make_new
        data = (stack(T4.kxs), stack(T4.klabels))
        data_train, data_test = splitobs(data; at=.9, shuffle=true)
        model = make_model(size(data[1], 1))
        opt = AdamW(1e-3)
        opt_state = Flux.setup(opt, model)
        global kdata_train = data_train
        global kdata_test = data_test
        global kmodel = model
        global kopt = opt
        global kopt_state = opt_state
    else
        data_train, data_test = kdata_train, kdata_test
        model = kmodel
        opt = kopt
        opt_state = kopt_state
    end

    train(model, opt_state, data_train; iters)
    test(model, data_test)
end

function train(model, opt_state, data; iters)
    ldr = DataLoader(data; batchsize=BatchSize[]) |> gpu
    batch_count = length(ldr)
    # d1 = first(ldr)
    # @show "first batch" typeof(d1) # size(d1)
    losses = Float32[]
    for epoch in 1:iters
        loss_sum = 0
        for (x, y) in ldr
            ls, grads = Flux.withgradient(model_loss, model, x, y)
            global kgrads = grads
            loss_sum += ls
            Flux.update!(opt_state, model, grads[1])
            yield()
            # Flux.train!(model_loss, model, ldr, opt_state)
        end
        loss = loss_sum / batch_count
        println("Train loss epoch #$(epoch): ", loss)
        push!(losses, loss)
    end
end

function test(model, data)
    ldr = DataLoader(data; batchsize=BatchSize[]) |> gpu
    batch_count = length(ldr)
    loss_sum = sum(batch -> model_loss(model, batch[1], batch[2]), ldr)
    println("Test loss: ", loss_sum / batch_count)
end

function test1(i=1)
    kmodel(kdata_test[1][:,i] |> gpu) |> cpu
end

function check_model()
    # loop through all data, find all the ones it got wrong, and look over them.
    data = (stack(T4.kxs), stack(T4.klabels), stack(T4.kys))
    model = kmodel |> cpu
    falsepos = []
    falseneg = []
    truepos = []
    trueneg = []
    count = 0
    for (x, y, rates) in eachobs(data)
        yhat = model(x)
        yh = argmax(yhat)
        val = (;x, y, yhat, rates)
        if yh != Flux.onecold(y)
            push!(yh == 1 ? falsepos : falseneg, val)
        else
            push!(yh == 1 ? truepos : trueneg, val)
        end
        count += 1
    end
    global kcheck_results = (;falsepos, falseneg, truepos, trueneg)
    println("Checked $count observations, found: ", map(length, kcheck_results))
    return
end

end