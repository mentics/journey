module ModelUtil
using Dates
using Flux
import JLD2
import FileUtil, DateUtil

export SplitLayer

struct SplitLayer{L,W}
    layers::L
    split::W
end
Flux.@functor SplitLayer

function (m::SplitLayer)(x)
    return (
        m.layers[1](x[1:m.split,:]),
        m.layers[2](x[(m.split+1):end,:])
    )
end

function make_block(cfg, through_width, hidden_width, num_layers)
    layers = Dense[]
    push!(layers, Dense(through_width => hidden_width, cfg.activation; bias=false))
    for _ in 1:(num_layers-2)
        push!(layers, Dense(hidden_width => hidden_width, cfg.activation; bias=false))
    end
    push!(layers, Dense(hidden_width => through_width, cfg.activation; bias=false))
    return Chain(layers)
end

function make_bins(num_bins, left, right)
    span = right - left
    return left .+ [span * b for b in 0.0:(1/(num_bins-1)):1.0]
end

# TODO: consider days until monthly expiration? until dividends?
# tradierDividends only gives us back since 2014-3-21 for spy
function to_temporal(ts)
    date = Date(ts)
    return (
        dayofweek(date) / 7,
        dayofmonth(date) / daysinmonth(date),
        dayofquarter(date) / DateUtil.daysinquarter(date),
        dayofyear(date) / daysinyear(date),
        hour(ts) / 24,
    )
end

#region Persistence
path_default_base() = joinpath(FileUtil.root_shared(), "mlrun")
path_default_base(mod_name) = joinpath(path_default_base(), string(mod_name))

function save(training, path_base=path_default_base(training.trainee.name))
    trainee = training.trainee
    mkpath(joinpath(path_base, "train"))
    path = joinpath(path_base, "train", "$(trainee.name)-$(trainee.version)-$(DateUtil.file_ts()).jld2")
    print("Saving model and opt_state to $(path)...")
    start = time()
    model_state = Flux.state(cpu(trainee.training_model))
    opt_state = cpu(training.opt_state)
    JLD2.jldsave(path; model_state, opt_state)
    stop = time()
    println(" done in $(stop - start) seconds.")
end

function load(training, path=path_default_base(training.trainee.name))
    trainee = training.trainee
    if isdir(path)
        path = FileUtil.most_recently_modified(joinpath(path, "train"); matching=trainee.version)
        @assert !isnothing(path) "Could not find training model file in path $(path)"
    end
    model_state, opt_state = JLD2.load(path, "model_state", "opt_state")
    Flux.loadmodel!(trainee.training_model, model_state)
    Flux.loadmodel!(training.opt_state, opt_state)
    println("Loaded model and opt_state from $(path)")
end

function save_inference(trainee, path_base=path_default_base(trainee.name))
    mkpath(path_base)
    path = joinpath(path_base, "infer", "$(trainee.name)-$(trainee.version)-infer-$(DateUtil.file_ts()).jld2")
    print("Saving inference model to $(path)...")
    start = time()
    model_state = Flux.state(trainee.inference_model |> cpu)
    JLD2.jldsave(path; model_state)
    stop = time()
    println(" done in $(stop - start) seconds.")
end

function load_inference(name, version, model, path=path_default_base(name))
    if isdir(path)
        path = FileUtil.most_recently_modified(joinpath(path, "infer"); matching="$(version)-infer-")
        @assert !isnothing(path) "Could not find inference model file in path $(path)"
    end
    model_state = JLD2.load(path, "model_state")
    Flux.loadmodel!(model, model_state)
    println("Loaded inference model from $(path)")
end
#endregion Persistence

end