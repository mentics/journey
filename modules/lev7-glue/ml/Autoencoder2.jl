module Autoencoder2
using Dates, IterTools
using Parquet2, DataFrames, Impute
using Flux, Optimisers, NNlib, MLUtils, CUDA
using CudaUtil

# TODO: maybe include bias terms now that we're not restricting 0 to 1

function model_encoder(inputwidth, encodedwidth, numlayers, activation)
    step = (encodedwidth - inputwidth) ÷ numlayers
    layerwidths = collect(inputwidth:step:encodedwidth)
    return Chain([Dense(layerwidths[i-1] => layerwidths[i],
                    i == (numlayers + 1) ? identity : activation;
                    bias=false
                ) for i in 2:(numlayers+1)])
end

function model_decoder(inputwidth, encodedwidth, numlayers, activation)
    step = (inputwidth - encodedwidth) ÷ numlayers
    layerwidths = collect(encodedwidth:step:inputwidth)
    return Chain([Dense(layerwidths[i-1] => layerwidths[i],
                    i == (numlayers + 1) ? identity : activation;
                    bias=false
                ) for i in 2:(numlayers+1)])
end

function make_model(hypers)
    (;seqlen, encodedwidth, numlayers, activation) = hypers
    return Chain(model_encoder(seqlen, encodedwidth, numlayers, activation), model_decoder(seqlen, encodedwidth, numlayers, activation))
end

# calcpenalty(modelpart) = abs.(modelpart)
calcsum(modelpart) = sum(abs, modelpart)
sumparams(model) = sum(calcsum, Flux.params(model))
sumyhat(yhat) = sum(abs, yhat)

function calcloss(model, x)
    yhat = model(x)
    penalty = 1.0 / (0.0001 + sumyhat(yhat))
    # # Mean of yhat elements except the last 2 should be 0.5... wait.. not quite, but we should be able to make an error term for it to avoid things going to 0
    # # maybe square denom
    # penalty = 1.0 / (0.01 + sumparams(model))
    return Flux.Losses.mse(x, yhat) + penalty
end

function hypers()
    encodedwidth = 64
    numlayers = 4
    return (;
        encodedwidth, numlayers,
        seqlen = numlayers^6 + encodedwidth,
        activation = NNlib.gelu
    )
end

function info(data)
    (;seqlen) = hypers()
    learningrate = 1e-4
    batchlen = 8192
    numsamples = length(data) - seqlen
    numbatches = round(Int, numsamples / batchlen, RoundUp)
    return (;
        learningrate, batchlen, numsamples, numbatches
    )
end

function run(data; iters=10)
    (;seqlen, encodedwidth, numlayers, activation) = hypers()
    (;learningrate) = info(data)
    modelcpu = make_model(hypers())
    global kmodelcpu = modelcpu
    model = modelcpu |> gpu
    global kmodelgpu = model
    opt = AdamW(learningrate)
    opt_state = gpu(Flux.setup(opt, model))
    global kopt = opt
    global kopt_state = opt_state

    train(make_batcher(data, seqlen), model, opt_state, info(data); iters)
end

function make_batcher(data, seqlen)
    (;batchlen, numbatches) = info(data)
    return function(batchi, variation)
        batchi <= floor(numbatches * 0.8) || return nothing
        return makebatch(data, seqlen, batchlen, batchi; variation)
    end
end

function train_continue(data, model=kmodelgpu, opt_state=kopt_state; iters=10)
    global kmodelgpu = gpu(model)
    global kopt_state = gpu(opt_state)
    train(make_batcher(data, hypers().seqlen), kmodelgpu, kopt_state, info(data); iters)
end

function calclossbase(batchexample)
    # 0.5 is the mean (approx because last 2 are different)
    ls = Flux.Losses.mse(batchexample, fill(0f0, size(batchexample)))
    return ls / size(batchexample)[end]
end

function train(batcher, model, opt_state, derinfo; iters=10)
    variation = 0
    (;learningrate, batchlen, numbatches) = derinfo
    # (;seqlen) = hypers()
    lossbase = calclossbase(batcher(1, variation))
    println("lossbase: ", lossbase)
    last_save = now(UTC)
    losses = Vector{Float64}(undef, numbatches)
    for epoch in 1:iters
        loss_sum = 0.0
        i = 1
        x = batcher(i, variation) |> gpu
        while !isnothing(x)
            ls, grads = Flux.withgradient(calcloss, model, x)
            ls /= size(x)[end] * lossbase
            losses[i] = ls
            loss_sum += ls
            Flux.update!(opt_state, model, grads[1])
            println("Train loss batch #$(i): $(ls)")
            yield()
            i += 1
            # if (i % 10) == 0
            #     yhat = model(x)
            #     if sum(abs, yhat) < 0.1 * (seqlen - 2)
            #         println("ERROR: yhat reached too low: ", sumparams(model))
            #         return
            #     end
            # end
            x = batcher(i, variation) |> gpu
        end
        loss = loss_sum / i
        println("Train loss epoch #$(epoch): $(loss)")
        if (now(UTC) - last_save) >= Minute(30)
            last_save = now(UTC)
            checkpoint_save()
        end
        toplosses = sortperm(losses; rev=true)
        Optimisers.adjust!(opt_state, learningrate / 2)
        for batchi in toplosses[1:3]
            ls = 0.0
            for variations in around(variation, batchlen)
                x = batcher(batchi, variations) |> gpu
                ls, grads = Flux.withgradient(calcloss, model, x)
                ls /= size(x)[end] * lossbase
                Flux.update!(opt_state, model, grads[1])
            end
            println("Top loss batch #$(batchi): (%$(100 * (1 - ls / losses[batchi])) improvement) $(losses[batchi]) -> $(ls)")
        end
        Optimisers.adjust!(opt_state, learningrate)
        variation += 1
        variation %= batchlen
    end
end

around(i, mx) = ( i = clamp(i, 1, mx-1) ; return (i-1):(i+1) )

#region Persist
const CONFIG = Ref(Dict{Symbol,String}())
CONFIG_WIN = Dict(
  :PATH_CHECKPOINTS => "D:/data/ml/journey/models",
  :PATH_DATA => "W:\\home\\jshellman\\ray\\alone",
)
CONFIG_LINUX = Dict(
  :PATH_CHECKPOINTS => "/home/jshellman/data/ml/journey/checkpoints",
  :PATH_DATA => "/home/jshellman/data/ml/input",
)
if Sys.iswindows()
  CONFIG[] = CONFIG_WIN
else
  CONFIG[] = CONFIG_LINUX
end

path_checkpoint() = CONFIG[][:PATH_CHECKPOINTS]
path_data() = CONFIG[][:PATH_DATA]

using JLD2
function checkpoint_save()
    kh = join(string.(values(hypers())), "-")
    # kh = join(string.(khyper), "-")
    path = joinpath(path_checkpoint(), "under-auto-$(kh)-$(round(Int, datetime2unix(now(UTC)))).jld2")
    println("Saving checkpoint to $(path)")
    jldsave(path, model_state=Flux.state(cpu(kmodelgpu)), opt_state=cpu(kopt_state))
end

function checkpoint_load(fname)
    path = joinpath(path_checkpoint(), fname)
    println("Loading checkpoint from $(path)")
    model = make_model(hypers())
    d = JLD2.load(path)
    Flux.loadmodel!(model, d["model_state"])
    global kmodelgpu = gpu(model)
    opt_state = d["opt_state"]
    global kopt_state = gpu(opt_state)
    return (;model, opt_state)
end
#endregion

#region Testing
using DrawUtil
function test1(data, ind)
    (;seqlen) = hypers()
    x = xforindex(ind, data, seqlen)
    xgpu = x |> gpu
    yhatgpu = kmodelgpu(xgpu)
    yhat = yhatgpu |> cpu
    lossbase = calclossbase(stack((x,)))
    println((;lossbase, loss=calcloss(kmodelgpu, xgpu), loss2=Flux.Losses.mse(x, yhat), loss0=Flux.Losses.mse(x, fill(0f0, length(x)))))
    draw(:lines, x)
    draw!(:lines, yhat)
end
#endregion

#region Data
import MLBatches
function makebatch(data, seqlen, batchlen, ind; variation)
    datalen = length(data) - seqlen
    indstart = 1 + (ind - 1) * batchlen
    println("Making batch for indstart $(indstart)")
    MLBatches.make_batch(xforindex, datalen, batchlen, indstart, data, seqlen; variation)
end

function xforindex(ind, data, seqlen)
    res = Array{Float32}(undef, seqlen)
    ind = ind + seqlen
    h0 = data[ind]
    seq = @view data[(ind-seqlen):(ind-1)]
    for i in 1:seqlen
        res[i] = 100f0 * ((seq[i] / h0) - 1.0)
    end
    return res
end

function make_data()
    fileinterp = joinpath(path_data(), "underinterp.parquet")
    if isfile(fileinterp)
        println("Loading interp parquet")
        pq = Parquet2.Dataset(fileinterp)
        df = DataFrame(pq; copycols=false)
    else
        println("Loading under parquet and interpolating")
        pq = Parquet2.Dataset(joinpath(path_data(), "under.parquet"))
        df = DataFrame(pq; copycols=false)
        # disallowmissing!(df, [:quote_ts, :under])
        df = interpolate(df)
        Parquet2.writefile(fileinterp, df)
    end
    return df
end

function interpolate(df)
    # Convert to float so interpolate won't complain about can't round
    df[!,:under] = Float32.(df[!,:under])
    dftimes = DataFrame(; quote_ts=DateTime(2010,01,01):Minute(30):DateTime(2022,10,1))
    joined = leftjoin(dftimes, df; on=:quote_ts)
    sort!(joined, [:quote_ts])
    dfint = Impute.interp(joined)
    dropmissing!(dfint)
    dfint[!,:under] = round.(Int, dfint[!,:under])
    for (row1, row2) in partition(eachrow(dfint), 2, 1)
        if (row2.quote_ts - row1.quote_ts) != Minute(30)
            @show row1
            @show row2
            error("data missing")
        end
        if abs(row2.under - row1.under) > 12000
            @show row1
            @show row2
            error("found thing")
        end
    end
    return dfint
end
#endregion

end
