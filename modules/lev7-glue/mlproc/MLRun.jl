module MLRun
using Dates
using CUDA
import Flux:Flux,cpu,gpu,AdamW
using IndexUtil

CUDA.allowscalar(false)
dev(x) = gpu(x) # gpu(x)

#=
Typical interface/approach:
structure()
hypers()
make_data(type instance)
make_model(type instance)
make_loss_func(type instance)
make_opt(type instance) # default is AdamW
=#
function setup end
function make_model end
function make_data end
function make_loss_func end
make_opt(learning_rate_func, loss_untrained) = AdamW(learning_rate_func(1, 1, loss_untrained))

function reset(mod)
    model = mod.make_model() |> dev
    println("Created model with param count: ", sum(length, Flux.params(model)))
    (;get_batch, batch_size, batch_count, get_inds, obs_count) = mod.make_data()
    calc_loss = mod.make_loss_func()
    base_loss = () -> calc_base_loss(model, calc_loss, get_batch, batch_count)
    loss_untrained = base_loss()
    learning_rate = mod.learning_rate_func
    opt = isdefined(mod, :make_opt) ? mod.make_opt(loss_untrained) : make_opt(learning_rate, loss_untrained)
    opt_state = Flux.setup(opt, model) |> dev
    global kall = (;model, opt, opt_state, get_batch, get_inds, obs_count, loss_untrained, calc_loss, base_loss, batch_count, batch_size, learning_rate)
    return
end

function run(mod; epochs=10, fold_count=5)
    isdefined(@__MODULE__, :kall) || reset(mod)
    cv = IndexUtil.cv_folds(kall.batch_count, fold_count)
    train(kall.model, kall.opt_state, kall.get_batch, kall.calc_loss, kall.base_loss, cv, kall.learning_rate; epochs)
    # holdout
    # test()
    # visualize result?
end

function train(model, opt_state, get_batch, calc_loss, base_loss, cv, learning_rate; epochs=10)
    MAX_OUTPUT_PERIOD = Second(4)
    println("Training beginning...")
    last_save = now(UTC)
    loss_prev = base_loss()
    losses = []
    global klosses = losses
    for epoch in 1:epochs
        loss_sum = 0.0
        i = 0
        for foldi in axes(cv.folds, 1)
            output_time = now(UTC)
            cv_inds = IndexUtil.inds_for_fold(cv, foldi)
            bi = 1
            for batchi in cv_inds.train
                batch = get_batch(epoch, batchi) |> dev
                # loss_batch, grads = Flux.withgradient(m -> calc_loss(m, batch), model)
                loss, grads = Flux.withgradient(calc_loss, model, batch)
                loss_sum += loss
                i += 1
                push!(losses, (epoch, foldi, batchi, loss))
                yield()
                Flux.update!(opt_state, model, grads[1])
                if now(UTC) - output_time > MAX_OUTPUT_PERIOD
                    println("epoch: $(epoch), fold: $(foldi), batch: $(bi) / $(length(cv_inds.train)) ($(batchi)) - Training loss $(loss)")
                    output_time = now(UTC)
                end
                bi += 1
            end

            loss_validation = 0f0
            for batchi in cv_inds.validation
                batch = get_batch(epoch, batchi) |> dev
                loss_validation += calc_loss(model, batch)
            end
            loss_validation /= size(cv_inds.validation, 1)

            println("Epoch $(epoch), fold $(foldi) - Validation loss #$(i): $(loss_validation)")
        end
        loss_epoch = loss_sum / i
        println("Epoch $(epoch) - Average loss $(loss_epoch) which is $(loss_epoch / loss_prev) * loss_prev")
        Flux.Optimisers.adjust!(opt_state, learning_rate(epoch, loss_prev, loss_epoch))
        loss_prev = loss_epoch
        if (now(UTC) - last_save) >= Minute(30)
            # last_save = now(UTC)
            # checkpoint_save()
        end
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

using JLD2
function save(path)
    model_state = Flux.state(cpu(kall.model))
    opt_state = cpu(kall.opt_state)
    jldsave(path; model_state, opt_state)
end
function load(path)
    model_state, opt_state = JLD2.load(path, "model_state", "opt_state")
    Flux.loadmodel!(kall.model, model_state)
    Flux.loadmodel!(kall.opt_state, opt_state)
end

function check(mod, batchi)
    batch = kall.get_batch(0, batchi) |> dev
    yhat = run_model(mod, kall.model, batch)
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

end