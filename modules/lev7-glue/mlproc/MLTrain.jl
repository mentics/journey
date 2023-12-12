module MLTrain
import Base:@kwdef
using Dates, Random
using CUDA
import Flux:Flux,cpu,gpu,throttle
using MLUtils
import Optimisers:AdamW
using DateUtil, IndexUtil, ModelUtil
CUDA.allowscalar(false)
dev(x) = gpu(x) # gpu(x)

# TODO: need to save holdout with training model so we keep that when continuing training

params_train() = (;
    rng_seed = 1,
    holdout = 0.1,
    kfolds = 5,
    batch_size = 512,
)

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
function setup(trainee::Trainee, params=params_train())
    Random.seed!(Random.default_rng(), params.rng_seed)

    model = trainee.make_model() |> dev
    println("Model has param count: ", sum(length, Flux.params(model)))

    opt = AdamW(trainee.get_learning_rate(1))
    opt_state = Flux.setup(opt, model) |> dev

    data = trainee.prep_data(params)
    metrics = Dict{Symbol,Any}()
    metrics[:loss_untrained] = calc_base_loss(model, trainee.get_loss, data, params.batch_size)
    println("Initial loss: $(metrics[:loss_untrained])")

    training = Training(;
        trainee,
        model,
        opt,
        opt_state,
        data,
        metrics,
        params = (;data=trainee.params.data, model=trainee.params.model, train=params),
    )
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

    loss_prev = 1.0
    epoch_losses = Float32[]
    training.metrics[:epoch_losses] = epoch_losses
    print_status = throttle(4; leading=false) do status
        println(status)
        # println("epoch: $(epoch), fold: $(ifold), batch: $(batches_fold_count) / $(length(cv_inds.train)) ($(ibatch)) - Training loss $(loss)")
    end

    for epoch in 1:epochs
        loss_epoch_sum = 0.0
        batches_epoch_count = 0
        ifold = 0
        for (train_data, cv_data) in kfolds(data.data_for_epoch(); k=params.kfolds)
            ifold += 1
            batches_fold_count = 0
            for obss in eachobs(train_data, batchsize=params.batch_size)
                batch = data.prep_input(obss) |> dev
                loss, grads = Flux.withgradient(trainee.get_loss, model, batch)
                loss_epoch_sum += loss
                yield()
                Flux.update!(training.opt_state, model, grads[1])

                batches_fold_count += 1
                batches_epoch_count += 1

                print_status("  update - epoch: $(epoch), fold: $(ifold), fold_batches: $(batches_fold_count), epoch_batches: $(batches_epoch_count) - Training loss $(loss)")
            end

            loss_validation = 0f0
            count = 0
            for obss in eachobs(cv_data, batchsize=params.batch_size)
                count += 1
                batch = data.prep_input(obss) |> dev
                loss_validation += trainee.get_loss(model, batch)
            end
            loss_validation /= count

            println("Epoch $(epoch), fold $(ifold) - Validation loss #$(batches_epoch_count): $(loss_validation)")
        end
        loss_epoch = loss_epoch_sum / batches_epoch_count
        push!(epoch_losses, loss_epoch)

        println("Epoch $(epoch) - Average loss $(loss_epoch) which is $(loss_epoch / loss_prev) * loss_prev")
        loss_prev = loss_epoch

        check_holdout(model, trainee, data, params.batch_size)

        rate = trainee.get_learning_rate(epoch)
        Flux.Optimisers.adjust!(training.opt_state, rate)
        println("Updated learning rate to $(rate)")

        throttle(5 * 60; leading=false) do; ModelUtil.save_training(training) end
    end
end

function calc_base_loss(model, calc_loss, data, batch_size)
    count = 10
    loss = 0.0
    for obss in first(eachobs(data.data_for_epoch(); batchsize=batch_size), count)
        loss += calc_loss(model, data.prep_input(obss) |> dev)
    end
    loss /= count
    return loss
end
#endregion Train

#region Check
function check_holdout(model, trainee, data, batch_size)
    loss = 0.0
    count = 0
    for obss in eachobs(data.holdout; batchsize=batch_size)
        count += 1
        loss += trainee.get_loss(model, data.prep_input(obss) |> dev)
    end
    loss /= count

    println("Loss for holdout: $(loss)")
    # for ibatch in training.params[:cv].holdout
    #     loss = training.trainee.get_loss(training.trainee.training_model, training.trainee.batches.get(1, ibatch))
    #     println("Loss for holdout batch $(ibatch): $(loss)")
    # end
end

function batch1(training)
    return training.data.prep_input(first(eachobs(training.data.data_for_epoch(), batchsize=training.params.train.batch_size)))
end

import DrawUtil:draw,draw!
function checki(training, inds)
    draw(:vlines, 0f0)
    trainee = training.trainee
    for i in inds
        gbatch = training.data.single(i) |> gpu
        yhat = trainee.run_train(training.model, gbatch) |> cpu
        batch = gbatch |> cpu

        # # (x, x) = trainee.mod.to_draw_x(batch, 2)
        # (yx, yy) = trainee.mod.to_draw_y(batch, 1)
        # (yhx, yhy) = trainee.mod.to_draw_yh(yhat, 1)

        yy = trainee.mod.to_draw_y(batch, 1)
        yhy = trainee.mod.to_draw_yh(yhat, 1)
        draw!(:scatter, yy)
        draw!(:scatter, yhy)

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
function save_output(trainee::Trainee; load=false)
    model = trainee.get_inference_model(trainee.make_model() |> dev)
    if load
        ModelUtil.load_infer(trainee.name, model, trainee.params)
        println("Model has param count: ", sum(length, Flux.params(model)))
    end

    pt = params_train()
    data = trainee.prep_data(pt)
    df = mapreduce(vcat, eachobs(data.all_data, batchsize=pt.batch_size)) do obss
        batch = data.prep_input(obss) |> dev
        output = trainee.run_infer(model, batch.x) |> cpu
        df = DataFrame(permutedims(output), :auto) # rows to cols

        keys = data.get_input_keys(batch) |> cpu
        insertcols!(df, 1, :key => keys)
        return df

        # keys = data.get_input_keys(batch) |> cpu
        # if keys isa AbstractDataFrame
        #     for (name, col) in Iterators.reverse(pairs(eachcol(keys)))
        #         insertcols!(df, 1, name => col)
        #     end
        # else
        #     @assert keys isa AbstractVector
        #     insertcols!(df, 1, :key => keys)
        # end
        # return df
    end

    @assert issorted(df.key)
    @assert allunique(df.key)
    @assert size(data.all_data, 1) == size(df, 1)
    path = Paths.save_data_params(Paths.db_output(trainee.name), trainee.params, df)
    print("Saved output data to $(path)")
    return df
end

# TODO: put this in data read or someplace like that
function load_rpm_output(model_name)
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

test_batch_loss(trainee, ibatch) = trainee.get_loss(trainee.training_model, trainee.batches.get(0, ibatch))

end
