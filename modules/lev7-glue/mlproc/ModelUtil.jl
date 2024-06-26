module ModelUtil
using Dates
using Flux
import JLD2
import DateUtil
using Paths, FilesArrow

export RangesLayer

struct RangesLayer{L,W}
    layers::L
    ranges::W
end
Flux.@functor RangesLayer (layers,)

function (m::RangesLayer)(x)
    return map(m.layers, m.ranges) do layer, range
        # layer(view(x, range))
        vw = view(x, range, fill(:, ndims(x)-1)...)
        layer(vw)
    end
end

encode_version(num, params) = "$(num)-$(Paths.safe_hash(params)))"

function make_block(through_width, hidden_width, num_layers, activation, bias=false)
    layers = Dense[]
    push!(layers, Dense(through_width => hidden_width, activation; bias))
    for _ in 1:(num_layers-2)
        push!(layers, Dense(hidden_width => hidden_width, activation; bias))
    end
    push!(layers, Dense(hidden_width => through_width, activation; bias))
    return Chain(layers)
end

# function make_bins(num_bins, left, right)
#     span = right - left
#     return left .+ [span * b for b in 0.0:(1/(num_bins-1)):1.0]
# end

function to_temporal_ts(ts)
    date = Date(ts)
    return Float32.((
        dayofweek(date) / 7,
        dayofmonth(date) / daysinmonth(date),
        dayofquarter(date) / DateUtil.daysinquarter(date),
        dayofyear(date) / daysinyear(date),
        hour(ts) / 24,
    ))
end

function to_temporal_date(date)
    return Float32.((
        dayofweek(date) / 7,
        dayofmonth(date) / daysinmonth(date),
        dayofquarter(date) / DateUtil.daysinquarter(date),
        dayofyear(date) / daysinyear(date),
    ))
end

#region Persistence
save_training(training) = save_training(training.trainee.name, training.model, training.opt_state, training.params)
function save_training(name, model, opt_state, params)
    start = time()
    model_state = Flux.state(cpu(model))
    opt_state = cpu(opt_state)
    path = Paths.save_data_params(Paths.db_checkpoint(name), params; suffix=string(DateUtil.file_ts()), model_state, opt_state)
    stop = time()
    println("Saved model, opt_state, and params to $(path) in $(stop - start) seconds")
end

load_training(training; hash=nothing) = load_training(training.trainee.name, training.model, training.opt_state, training.params; hash)
function load_training(name, model, in_opt_state, params; hash=nothing)
    start = time()
    (path, (model_state, opt_state)) = Paths.load_data_params(Paths.db_checkpoint(name), params, "model_state", "opt_state"; latest=true, hash)
    Flux.loadmodel!(model, model_state)
    Flux.loadmodel!(in_opt_state, opt_state)
    stop = time()
    println("Loaded model and opt_state from $(path) in $(stop - start) seconds")
end

save_infer(trainee, model_training) = save_infer(trainee.name, trainee.get_inference_model(model_training), trainee.params)
function save_infer(name, model, params)
    start = time()
    model_state = Flux.state(cpu(model))
    path = Paths.save_data_params(Paths.db_infer(name), params; suffix=string(DateUtil.file_ts()), model_state)
    stop = time()
    print("Saved inference model to $(path) in $(stop - start) seconds.")
end

load_infer(trainee) = load_infer(trainee.name, trainee.make_model(), trainee.params)
load_infer(trainee, model) = load_infer(trainee.name, model, trainee.params)
function load_infer(name, model, params)
    start = time()
    (path, model_state) = Paths.load_data_params(Paths.db_infer(name), params, "model_state"; latest=true)
    # global klinfer = (;model, model_state)
    Flux.loadmodel!(model, model_state)
    stop = time()
    println("Loaded inference model from $(path) in $(stop - start) seconds")
    return model
end
#endregion Persistence

end