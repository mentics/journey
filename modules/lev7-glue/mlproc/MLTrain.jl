module MLTrain
import Base:@kwdef
using Dates, Random, DataStructures
using CUDA
import Flux:Flux,cpu,gpu,throttle
using MLUtils
# import Optimisers:AdamW
using DateUtil, IndexUtil, ModelUtil

CUDA.allowscalar(false)
dev(x) = gpu(x) # gpu(x)

params_train(;kws...) = (;
    rng_seed = 1,
    # holdout = 0.1,
    kfolds = 5,
    batch_size = 512,
    weight_decay = 0.001f0,
    kws...
)

# Note: the following seemed to work well for HistShapeModel
# params_train(;kws...) = (;
#     rng_seed = 1,
#     holdout = 0.1,
#     kfolds = 5,
#     batch_size = 512,
#     weight_decay = 0.00004f0,
#     kws...
# )

#=
example use:
trainee = {module}.MLTrain()
training = MLTrain.setup(trainee)
MLTrain.train(training)
=#

#=
Typical interface/approach:
structure()
hypers()
make_data(type instance)
make_model(type instance)
make_loss_func(type instance)
make_opt(type instance) # default is Lion
=#
# function setup end
# function make_model end
# function make_data end
# function make_loss_func end
# make_opt(learning_rate_func, loss_untrained) = AdamW(learning_rate_func(0, 0f0, loss_untrained))

#region Types
export InputData
@kwdef struct InputData
    all_data
    data_for_epoch
    prep_input
    get_input_keys
    holdout
    single
end

export Trainee
@kwdef struct Trainee
    name
    version
    make_model
    get_inference_model
    run_train
    run_infer
    prep_data
    get_learning_rate
    get_loss
    params
    mod
end

@kwdef struct Training
    trainee::Trainee
    model
    opt
    opt_state
    data
    metrics
    params
end
#endregion Types

#region Train
function setup(trainee::Trainee, pt=nothing; kws...)
    isnothing(pt) && ( pt = params_train(;kws...) )
    # params = (;data=trainee.params.data, model=trainee.params.model, train=pt)
    params = (;trainee.params..., train=pt)
    global kparams = params
    Random.seed!(Random.default_rng(), pt.rng_seed)

    model = trainee.make_model() |> dev
    println("Model has param count: ", sum(length, Flux.params(model)))

    # opt = Flux.AdamW(trainee.get_learning_rate(1), (0.9, 0.999), pt.weight_decay)
    opt = Flux.Optimisers.Lion(trainee.get_learning_rate(1))
    opt_state = Flux.setup(opt, model) |> dev

    # if !iszero(pt.weight_decay)
    #     opt_inner = Flux.AdamW(trainee.get_learning_rate(1))
    #     opt = Flux.OptimiserChain(Flux.WeightDecay(pt.weight_decay), opt_inner)
    #     opt_state = Flux.setup(opt, model) |> dev
    # else
    #     opt = Flux.AdamW(trainee.get_learning_rate(1))
    #     opt_state = Flux.setup(opt, model) |> dev
    # end

    data = trainee.prep_data(params)
    metrics = Dict{Symbol,Any}()

    training = Training(;
        trainee,
        model,
        opt,
        opt_state,
        data,
        metrics,
        params,
    )

    print("Initial loss: ")
    metrics[:loss_untrained] = check_holdout(training) # calc_base_loss(model, trainee.get_loss, data, params.batch_size)

    return training
end

function train(training::Training; epochs=1000)
    Random.seed!(Random.default_rng(), training.params.train.rng_seed)
    println("Training beginning...")
    trainee = training.trainee
    model = training.model
    metrics = training.metrics
    params = training.params.train
    data = training.data
    weight_decay = params.weight_decay

    loss_holdout_hist = CircularBuffer(5)
    loss_epoch_hist = CircularBuffer(5)
    loss_prev = 100.0
    epoch_losses = Float32[]
    training.metrics[:epoch_losses] = epoch_losses
    print_status = throttle(4; leading=false) do status
        println(status)
        # println("epoch: $(epoch), fold: $(ifold), batch: $(batches_fold_count) / $(length(cv_inds.train)) ($(ibatch)) - Training loss $(loss)")
    end

    bufs = nothing
    batch_size_prev = 0
    for epoch in 1:epochs
        loss_epoch_sum = 0.0
        batches_epoch_count = 0
        ifold = 0
        for (train_data, cv_data) in kfolds(data.data_for_epoch(); k=params.kfolds)
            ifold += 1
            batches_fold_count = 0
            Flux.trainmode!(model)
            for obss in eachobs(train_data, batchsize=params.batch_size)
                batch_size = size(obss, 1)
                if batch_size != batch_size_prev
                    bufs = trainee.mod.make_buffers(obss)
                    batch_size_prev = batch_size
                end
                batch = data.prep_input(obss, bufs)
                loss, grads = Flux.withgradient(trainee.get_loss, model, batch)
                loss_epoch_sum += loss
                @assert !any(v -> any(!isfinite, v), Flux.params(grads[1]))
                yield()
                Flux.update!(training.opt_state, model, grads[1])

                batches_fold_count += 1
                batches_epoch_count += 1

                print_status("  update - epoch: $(epoch), fold: $(ifold), fold_batches: $(batches_fold_count), epoch_batches: $(batches_epoch_count) - Training loss $(loss)")
            end

            Flux.testmode!(model)
            loss_validation = 0f0
            count = 0
            for obss in eachobs(cv_data, batchsize=params.batch_size)
                count += 1
                batch = data.prep_input(obss) |> dev
                loss_validation += trainee.get_loss(model, batch)
            end
            loss_validation /= count
            Flux.testmode!(model, :auto)

            println("Epoch $(epoch), fold $(ifold) - Validation loss #$(batches_epoch_count): $(loss_validation)")
        end
        loss_epoch = loss_epoch_sum / batches_epoch_count
        push!(epoch_losses, loss_epoch)

        println("Epoch $(epoch) - Average loss $(loss_epoch) which is $(loss_epoch / loss_prev) * loss_prev")
        pushfirst!(loss_epoch_hist, loss_epoch)
        loss_prev = loss_epoch

        loss_holdout = check_holdout(training)

        rate = trainee.get_learning_rate(epoch)
        Flux.Optimisers.adjust!(training.opt_state, rate)
        println("Updated learning rate to $(rate)")
        loss_holdout_ratio = loss_holdout / loss_epoch
        if loss_holdout_ratio > 1.1
            # holdout loss is trailing too far, so increase weight decay to generalize better
            weight_decay *= loss_holdout_ratio
            Flux.Optimisers.adjust!(training.opt_state; gamma=weight_decay)
            println("Increased weight decay to $(weight_decay) for ratio $(loss_holdout_ratio)")
        elseif length(loss_epoch_hist) == capacity(loss_epoch_hist)
            # TODO: use epoch loss also somehow? use this to detect when to end/give up?
            recent = (loss_epoch_hist[1] + loss_epoch_hist[2]) / 2
            past = (loss_epoch_hist[end] + loss_epoch_hist[end-1]) / 2
            improvement = 1.0 - recent / past
            if improvement < 0.001
                weight_decay *= 0.9
                println("Decreased weight decay to $(weight_decay) for epoch loss improvement $(improvement)")
            end
        end
        pushfirst!(loss_holdout_hist, loss_holdout)

        throttle(5 * 60; leading=false) do; ModelUtil.save_training(training) end
    end
end

# Copying BitArray to gpu is very slow
# using CUDA
# function test()
#     c = rand(100,100) .> 0.5
#     g = CUDA.cu(c)
#     @time copyto!(g, c)
# end
# function test2()
#     c = convert(Array{Bool}, rand(100,100) .> 0.5)
#     g = CUDA.cu(c)
#     @time copyto!(g, c)
# end

# import BenchmarkTools
# function test(gbufs, bufs)
#     # They were nearly identical time, but first one had allocations
#     BenchmarkTools.@btime gpu($bufs)
#     BenchmarkTools.@btime copyto_itr!($gbufs, $bufs)
# end

# function calc_base_loss(model, calc_loss, data, params)
#     count = 10
#     loss = 0.0
#     for obss in first(eachobs(data.data_for_epoch(); batchsize=params.train.batch_size), count)
#         loss += calc_loss(model, data.prep_input(obss, params) |> dev)
#     end
#     loss /= count
#     return loss
# end
#endregion Train

#region Check
# check_holdout(training) = check_holdout(training.model, training.trainee, training.data, training.params.train.batch_size)
# function check_holdout(model, trainee, data, batch_size)
function check_holdout(training)
    model = training.model
    trainee = training.trainee
    data = training.data
    params = training.params

    Flux.testmode!(model)
    # trainee.mod.OVERRIDE_SQUARED[] = true
    loss = 0.0
    count = 0
    for obss in eachobs(data.holdout; batchsize=params.train.batch_size)
        count += 1
        loss += trainee.get_loss(model, data.prep_input(obss) |> dev)
    end
    loss /= count
    Flux.testmode!(model, :auto)
    # trainee.mod.OVERRIDE_SQUARED[] = false

    println("Loss for holdout: $(loss)")
    # for ibatch in training.params[:cv].holdout
    #     loss = training.trainee.get_loss(training.trainee.training_model, training.trainee.batches.get(1, ibatch))
    #     println("Loss for holdout batch $(ibatch): $(loss)")
    # end
    return loss
end

obss1(training) = first(eachobs(training.data.data_for_epoch(), batchsize=training.params.train.batch_size))

function batch1(training)
    # return training.data.prep_input(first(eachobs(training.data.data_for_epoch(), batchsize=training.params.train.batch_size)))
    return training.data.prep_input(obss1(training))
end
single(training, ind) = (;batch=training.data.single(ind) |> cpu, yhat=training.trainee.run_train(training.model, training.data.single(ind).x) |> cpu)
# single_yhat(training, ind) = vec(training.trainee.run_train(training.model, training.data.single(ind).x) |> cpu)

import DrawUtil:draw,draw!
function checki(training, inds)
    draw(:vlines, 0f0)
    trainee = training.trainee
    for i in inds
        gbatch = training.data.single(i) |> gpu
        yhat = trainee.run_train(training.model, gbatch.x) |> cpu
        batch = gbatch |> cpu

        # # (x, x) = trainee.mod.to_draw_x(batch, 2)
        # (yx, yy) = trainee.mod.to_draw_y(batch, 1)
        # (yhx, yhy) = trainee.mod.to_draw_yh(yhat, 1)
        println(typeof(yhat))

        yy = trainee.mod.to_draw_y(batch, 1)
        yhy = trainee.mod.to_draw_yh(yhat, 1)
        draw!(:scatter, yy; label="y-$(i)")
        draw!(:scatter, yhy; label="yh-$(i)")

        # # draw(:scatter, x; label="x")
        # # draw!(:scatter, yhx, yhy; label="yh")
        # # draw!(:scatter, yx, yy ./ 10; label="y")
        # draw!(:scatter, yhx, yhy)
        # # draw!(:scatter, yx, yy ./ 10)
        # draw!(:scatter, [yhx[findmax(yhy[:,1])[2]]], [0.1])
    end
    return # (;batch, yhat)

    # for i in axes(under, 1), j in axes(under, 2)
    #     if under[i,j] != 0f0
    #       print("$(under[i,j]),")
    #     end
    #   end
end

function check(trainee, batchi)
    gbatch = trainee.batches.get(0, batchi)
    yhat = trainee.run_model(gbatch) |> cpu
    batch = gbatch |> cpu
    # (x, x) = trainee.mod.to_draw_x(batch, 2)
    (yx, yy) = trainee.mod.to_draw_y(batch, 2)
    (yhx, yhy) = trainee.mod.to_draw_yh(yhat, 2)
    # draw(:scatter, x; label="x")
    draw(:scatter, yhx, yhy; label="yh")
    draw!(:scatter, yx, yy; label="y")
    return (;batch, yhat)

    # for i in axes(under, 1), j in axes(under, 2)
    #     if under[i,j] != 0f0
    #       print("$(under[i,j]),")
    #     end
    #   end
end
#endregion Check

#region Infer
# TODO: maybe in different module
using DataFrames
using Paths
import ThreadPools
# save_output(training::Training; kws...) = save_output(training.trainee, training.model; kws...)
# function save_output(trainee::Trainee, model=nothing; kws...)
function save_output(training::Training; kws...)
    trainee = training.trainee
    model = training.model
    # if isnothing(model)
    #     model = trainee.make_model() |> dev
    #     ModelUtil.load_infer(trainee.name, model, trainee.params)
    #     println("Model has param count: ", sum(length, Flux.params(model)))
    # end
    pt = params_train(;kws...)
    data = trainee.prep_data(pt)
    # TODO:
    check_holdout(training)
    model = trainee.get_inference_model(model)

    bufs = trainee.mod.make_buffers(obss1(training))
    # xgpu = CuArray{Float32}(undef, feature_count, pt.batch_size)
    df = mapreduce(vcat, eachobs(data.all_data, batchsize=pt.batch_size)) do obss
        if size(obss, 1) == pt.batch_size
            batch = data.prep_input(obss, bufs)
            yhat = trainee.run_infer(model, batch.x) |> cpu
            output = [yhat[:,i] for i in axes(yhat,2)]
            return DataFrame((;batch.keys..., output))
        else
            # Handle the last batch which is likely a different size
            batch = data.prep_input(obss)
            yhat = trainee.run_infer(model, batch.x |> gpu) |> cpu
            output = [yhat[:,i] for i in axes(yhat,2)]
            return DataFrame((;batch.keys..., output))
        end
    end

    keycols = data.get_input_keys()
    @assert issorted(df, keycols)
    @assert allunique(df, keycols)
    @assert size(data.all_data, 1) == size(df, 1)
    path = Paths.save_data_params(Paths.db_output(trainee.name), trainee.params, df)
    print("Saved output data to $(path)")
    return df
end

# TODO: put this in data read or someplace like that
# in dict form, it's in DataRead now
function load_output(model_name)
    Paths.load_data_params(Paths.db_output(model_name), DataFrame)
end
#endregion Infer

function latent_space(mod)
    # return mod.run_encode(kall.model, kall.get_batch(0, 1) |> dev)
    data = mapreduce(hcat, 1:kall.batch_count) do batchi
        batch = kall.get_batch(0, batchi) |> dev
        return mod.run_encode(kall.model, batch) |> cpu
    end
    inds = mapreduce(vcat, 1:kall.batch_count) do batchi
        kall.get_inds(0, batchi)
    end
    return (;data, inds)
end

function test_loss(training, ind)
    training.trainee.get_loss(training.model, training.data.single(ind) |> gpu)
end
test_batch_loss(trainee, ibatch) = trainee.get_loss(trainee.training_model, trainee.batches.get(0, ibatch))

function save(training)
    check_holdout(training)
    ModelUtil.save_training(training)
end
function load(training; hash=nothing)
    ModelUtil.load_training(training; hash)
    check_holdout(training)
    return
end

end
