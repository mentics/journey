module MLRun
using Dates
using CUDA
import Flux:Flux,cpu,gpu,AdamW

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
make_opt(config) = AdamW(config().learning_rate_func(0, 0))

function reset(config)
    model = make_model(config) |> dev
    println("Created model with param count: ", sum(length, Flux.params(model)))
    (;get_batch, batch_size, batch_count) = make_data(config)
    calc_loss = make_loss_func(config)
    opt = make_opt(config)
    opt_state = Flux.setup(opt, model) |> dev
    base_loss = () -> calc_base_loss(model, calc_loss, get_batch, batch_count, batch_size)
    loss_untrained = base_loss()
    global kall = (;model, opt, opt_state, get_batch, loss_untrained, calc_loss, base_loss, batch_count, batch_size)
    return
end

function run(config; iters=10)
    isdefined(@__MODULE__, :kall) || reset(config)
    train(kall.model, kall.opt_state, kall.get_batch, kall.calc_loss, kall.base_loss, kall.batch_count, kall.batch_size; iters)
    # holdout
    # test()
    # visualize result?
end

function train(model, opt_state, get_batch, calc_loss, base_loss, batch_count, batch_size; iters=10, start_batch=1)
    last_save = now(UTC)
    loss_prev = base_loss()
    for epoch in 1:iters
        loss_sum = 0.0
        i = start_batch
        batch = get_batch(epoch, i) |> dev
        # while !isnothing(batch)
        while i <= batch_count
            loss_batch, grads = Flux.withgradient(m -> calc_loss(m, batch), model)
            loss = loss_batch / batch_size
            loss_sum += loss
            Flux.update!(opt_state, model, grads[1])
            println("Train loss batch #$(i): $(loss) from $(loss_batch)")
            yield()
            # if (i % 10) == 0
            #     yhat = model(x)
            #     if sum(abs, yhat) < 0.1 * (seqlen - 2)
            #         println("ERROR: yhat reached too low: ", sumparams(model))
            #         return
            #     end
            # end
            i += 1
            batch = get_batch(epoch, i) |> dev
        end
        loss_epoch = loss_sum / (i-1)
        println("After $(i-1) batches, epoch $(epoch) average loss is $(loss_epoch) which is $(loss_epoch / loss_prev) * loss_prev")
        loss_prev = loss_epoch
        if (now(UTC) - last_save) >= Minute(30)
            # last_save = now(UTC)
            # checkpoint_save()
        end
        # push!(losses, loss)
    end
end

function calc_base_loss(model, calc_loss, get_batch, batch_count, batch_size)
    count = 10
    loss = 0.0
    for _ in 1:count
        loss += calc_loss(model, get_batch(0, rand(1:batch_count)) |> dev)
    end
    loss /= count
    loss /= batch_size
    return loss
end

end