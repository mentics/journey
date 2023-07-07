module LyzeT4
using Flux, MLUtils, CUDA
using CudaUtil
import Trade4Data as T4

const BatchSize = Ref(128)
const LossClassWeights = Ref([0.25f0, 1.0f0] |> gpu)

# TODO: need to do this with the particular ratio of categories in data
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
    mult = 4
    h1 = mult * len
    h2 = 2 * mult * len
    h3 = 2 * mult * len
    h4 = mult * len
    out = 2 # output is one-hot yes/no
    d1 = Dense(len => h1; bias=false)
    d2 = Dense(h1 => h2, tanh; bias=false)
    dr1 = Dropout(0.05)
    d3 = Dense(h2 => h3, tanh; bias=false)
    d4 = Dense(h3 => h4, relu; bias=false)
    d5 = Dense(h4 => out; bias=false)
    mcpu = Chain(d1, d2, dr1, d3, d4, d5)
    println("Created model with param count: ", sum(length, Flux.params(mcpu)))
    return mcpu |> gpu
end

function run(make_new=false; iters=10)
    if make_new
        LossClassWeights[] = [0.25f0, 1.0f0] |> gpu
        xbal, ybal = MLUtils.oversample(T4.kxs, T4.klabels; shuffle=true, fraction=0.25)
        data = (stack(xbal), stack(ybal))
        data_train, data_test = splitobs(data; at=.9, shuffle=false)
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

function set_lr(lr)
    kopt.os[1].eta = lr
    global kopt_state = Flux.setup(kopt, kmodel);
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

function test1(i=1; data=kdata_test)
    x = data[1][:,i]
    y = data[2][:,i]
    yh = kmodel(x |> gpu) |> cpu
    (;x, y, yh, match=Flux.onecold(y) == argmax(yh))
end

function check_all(; data=kdata_test)
    len = size(data[1], ndims(data[1]))
    falsepos = []
    falseneg = []
    truepos = []
    trueneg = []
    count = 0

    for i in 1:len
        val = test1(i; data)
        x, y, yh, match = val
        pos = argmax(yh) == 1
        if match
            push!(pos ? truepos : trueneg, val)
        else
            push!(pos ? falsepos : falseneg, val)
        end
        count += 1
    end
    global kcheck_results = (;falsepos, falseneg, truepos, trueneg)
    println("Checked $count observations, found: ", map(length, kcheck_results))
    return
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
        if yh == Flux.onecold(y)
            push!(yh == 1 ? truepos : trueneg, val)
        else
            push!(yh == 1 ? falsepos : falseneg, val)
        end
        count += 1
    end
    global kcheck_results = (;falsepos, falseneg, truepos, trueneg)
    println("Checked $count observations, found: ", map(length, kcheck_results))
    return
end

# MLUtils.group_indices(classes::) where T<:AbstractVector
#     dict = Dict{eltype(T), Vector{Int}}()
#     for (idx, elem) in enumerate(classes)
#         if !haskey(dict, elem)
#             push!(dict, elem => [idx])
#         else
#             push!(dict[elem], idx)
#         end
#     end
#     return dict
# end

end