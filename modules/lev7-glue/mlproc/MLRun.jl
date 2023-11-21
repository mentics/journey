module MLRun
import Base:@kwdef
using Dates
using CUDA
import Flux:Flux,cpu,gpu,throttle
import Optimisers:AdamW
using DateUtil, IndexUtil, ModelUtil

CUDA.allowscalar(false)
dev(x) = gpu(x) # gpu(x)

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

export Batches2
@kwdef struct Batches2
    get
    count
    holdout
end

export Trainee7
@kwdef struct Trainee7
    name
    version
    training_model
    inference_model
    run_model
    infer
    batches::Batches2
    get_learning_rate
    get_loss
    mod
    cfg
end

@kwdef struct Training7
    trainee::Trainee7
    opt
    opt_state
    metrics
    params
end

function setup(trainee::Trainee7)
    model = trainee.training_model |> dev
    println("Model has param count: ", sum(length, Flux.params(model)))

    base_loss = () -> calc_base_loss(model, trainee.get_loss, trainee.batches.get, trainee.batches.count)
    loss_untrained = base_loss()
    println("Loss untrained: $(loss_untrained)")
    metrics = Dict{Symbol,Any}(:loss_untrained => loss_untrained)

    opt = AdamW(trainee.get_learning_rate(1))
    opt_state = Flux.setup(opt, model) |> dev

    params = Dict{Symbol,Any}()

    training = Training7(;
        trainee,
        opt,
        opt_state,
        metrics,
        params,
    )
    # (;mod, version, model, opt, opt_state, get_batch, get_inds, obs_count, loss_untrained, calc_loss, base_loss, batch_count, batch_size, learning_rate)
    return training
end

function run(training::Training7; epochs=1000, fold_count=5)
    cv = IndexUtil.cv_folds(training.trainee.batches.count, fold_count)
    training.params[:cv] = cv
    train(training; epochs)
end

function train(training; epochs=1000)
    println("Training beginning...")
    trainee = training.trainee
    cv = training.params[:cv]
    model = trainee.training_model
    batches = trainee.batches

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
        for ifold in axes(cv.folds, 1)
            cv_inds = IndexUtil.inds_for_fold(cv, ifold)
            batches_fold_count = 0
            for ibatch in cv_inds.train
                batch = batches.get(epoch, ibatch) |> dev
                loss, grads = Flux.withgradient(trainee.get_loss, model, batch)
                loss_epoch_sum += loss
                yield()
                Flux.update!(training.opt_state, model, grads[1])

                batches_fold_count += 1
                batches_epoch_count += 1

                print_status("  update - epoch: $(epoch), fold: $(ifold), batch: $(batches_fold_count) / $(length(cv_inds.train)) ($(ibatch)) - Training loss $(loss)")
            end

            loss_validation = 0f0
            for ibatch in cv_inds.validation
                batch = batches.get(epoch, ibatch) |> dev
                loss_validation += trainee.get_loss(model, batch)
            end
            loss_validation /= size(cv_inds.validation, 1)

            println("Epoch $(epoch), fold $(ifold) - Validation loss #$(batches_epoch_count): $(loss_validation)")
        end
        loss_epoch = loss_epoch_sum / batches_epoch_count
        push!(epoch_losses, loss_epoch)

        println("Epoch $(epoch) - Average loss $(loss_epoch) which is $(loss_epoch / loss_prev) * loss_prev")

        check_holdout(training)

        rate = trainee.get_learning_rate(epoch)
        Flux.Optimisers.adjust!(training.opt_state, rate)
        println("Updated learning rate to $(rate)")

        throttle(30 * 60; leading=false) do; ModelUtil.save(training) end
    end
end

function calc_base_loss(model, calc_loss, get_batch, batch_count)
    count = 10
    loss = 0.0
    for _ in 1:count
        loss += calc_loss(model, get_batch(0, rand(1:batch_count)) |> dev)
    end
    loss /= count
    return loss
end

import DrawUtil:draw,draw!
function check(trainee, batchi)
    gbatch = trainee.batches.get(0, batchi)
    yhat = trainee.run_model(gbatch) |> cpu
    batch = gbatch |> cpu
    x = trainee.mod.to_draw_x(batch, 2)
    yh = trainee.mod.to_draw_yh(yhat, 2)
    draw(:scatter, x; label="x")
    draw!(:scatter, yh; label="yh")
    return (;batch, yhat)

    # for i in axes(under, 1), j in axes(under, 2)
    #     if under[i,j] != 0f0
    #       print("$(under[i,j]),")
    #     end
    #   end
end

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

test_batch_loss(trainee, ibatch) = trainee.get_loss(trainee.model, trainee.batches.get(0, ibatch))

function check_holdout(training)
    loss = training.trainee.get_loss(training.trainee.training_model, training.trainee.batches.holdout)
    println("Loss for holdout: $(loss)")
    # for ibatch in training.params[:cv].holdout
    #     loss = training.trainee.get_loss(training.trainee.training_model, training.trainee.batches.get(1, ibatch))
    #     println("Loss for holdout batch $(ibatch): $(loss)")
    # end
end

end
