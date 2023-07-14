module Autoencoder2
using Dates, IterTools
using Parquet2, DataFrames
using Flux, NNlib, MLUtils, CUDA
using CudaUtil

# TODO: maybe include bias terms now that we're not restricting 0 to 1
function start()
    df = make_data()
    data = df.under
    return data
end

function model_encoder(inputwidth, encodedwidth, numlayers, activation)
    step = (encodedwidth - inputwidth) ÷ numlayers
    layerwidths = collect(inputwidth:step:encodedwidth)
    return Chain([Dense(layerwidths[i-1] => layerwidths[i],
                    # i == (numlayers + 1) ? identity : activation;
                    i == 1 ? identity : activation;
                    bias = i == 1
                ) for i in 2:(numlayers+1)])
end

function model_decoder(inputwidth, encodedwidth, numlayers, activation)
    step = (inputwidth - encodedwidth) ÷ numlayers
    layerwidths = collect(encodedwidth:step:inputwidth)
    return Chain([Dense(layerwidths[i-1] => layerwidths[i],
                    i == (numlayers + 1) ? identity : activation;
                    bias = i == (numlayers + 1)
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

const MASK2 = Ref{Float32}()
const MASKGPU2 = Ref{CuArray{Float32, 1, CUDA.Mem.DeviceBuffer}}()

function setmask(num)
    for i in 1:num
        MASK2[][i] = 1f0
    end
    for i in (num + 1):length(MASK2[])
        MASK2[][i] = 0f0
    end
    MASKGPU2[] = MASK2[] |> gpu
    return MASK2[]
end

function calcloss(model, x)
    #enced = model.layers[1](x)
    #masked = enced .* MASKGPU[]
    #yhat = model.layers[2](masked)

    yhat = model(x)
    return Flux.Losses.mse(x, yhat)

    # penalty = 1.0 / (0.0001 + sumyhat(yhat))

    # # maybe square denom
    # penalty = 1.0 / (0.01 + sumparams(model))

    # return Flux.Losses.mse(x, yhat) + penalty
end

function hypers()
    encodedwidth = 64
    numlayers = 4
    return (;
        encodedwidth, numlayers,
        seqlen = numlayers^6 + encodedwidth,
        activation = NNlib.swish
    )
end

function info(data)
    (;seqlen) = hypers()
    learningrate = 1e-4
    batchlen = 512
    numsamples = length(data) - seqlen
    numbatches = round(Int, numsamples / batchlen, RoundUp)
    spliti = round(Int, 0.8 * numbatches, RoundDown)
    trainbatchis = 1:spliti
    testbatchis = (spliti+1):numbatches
    return (;
        learningrate, batchlen, numsamples, trainbatchis, testbatchis
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

    #for i in 1:encodedwidth
        #setmask(i)
        train(make_batcher(data, seqlen), model, opt_state, info(data); iters)
    #end
end

function make_batcher(data, seqlen)
    (;batchlen) = info(data)
    return function(batchi, variation)
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
    ls = Flux.Losses.mse(batchexample, zeros(Float32, size(batchexample))) # fill(0f0, size(batchexample)))
    return ls / size(batchexample)[end]
end

function train(batcher, model, opt_state, derinfo; iters=10)
    variation = 0
    (;learningrate, batchlen, trainbatchis) = derinfo
    trainbatchis = trainbatchis[1:10]
    # (;seqlen) = hypers()
    # println("lossbase: ", lossbase)
    last_save = now(UTC)
    numbatches = length(trainbatchis)
    lossesmin = fill(Inf, numbatches)
    global klosses = lossesmin
    lossbases = Vector{Float32}(undef, numbatches)
    for epoch in 1:iters
        if (variation % 10) == 0
            print("Updating lossbases...")
            Threads.@threads for i in eachindex(trainbatchis)
                lossbases[i] = calclossbase(batcher(trainbatchis[i], variation))
            end
            println(" done.")
        end
        loss_sum = 0.0
        for i in eachindex(trainbatchis)
            batchi = trainbatchis[i]
            x = batcher(batchi, variation) |> gpu
            ls, grads = Flux.withgradient(calcloss, model, x)
            ls /= size(x)[end] * lossbases[i]
            lossesmin[i] = min(lossesmin[i], ls)
            loss_sum += ls
            Flux.update!(opt_state, model, grads[1])
            println("Train loss batch #$(i): $(ls)")
            yield()
            # if (i % 10) == 0
            #     yhat = model(x)
            #     if sum(abs, yhat) < 0.1 * (seqlen - 2)
            #         println("ERROR: yhat reached too low: ", sumparams(model))
            #         return
            #     end
            # end
        end
        loss = loss_sum / numbatches
        println("Train loss epoch #$(epoch): $(loss)")
        if (now(UTC) - last_save) >= Minute(30)
            last_save = now(UTC)
            checkpoint_save()
        end
        toplossis = sortperm(lossesmin; rev=true)
        numtop = 3
        xtops = map(1:numtop) do i
            batcher(trainbatchis[toplossis[i]], variation)
        end

        ####
        topbatchcpu = reduce(hcat, xtops)
        toplossbase = calclossbase(topbatchcpu)
        topbatch = topbatchcpu |> gpu
        toplen = size(topbatch)[end]
        toplsorig = calcloss(model, topbatch) / (toplen * toplossbase)
        topls = 0.0
        improvement = 0.0
        for _ in 1:100
            Flux.Optimisers.adjust!(opt_state, rand(.05:0.001:0.84) * learningrate)
            topls, grads = Flux.withgradient(calcloss, model, topbatch)
            topls /= toplen * toplossbase
            Flux.update!(opt_state, model, grads[1])
            improvement = (1 - topls / toplsorig)
            improvement >= 0.0001 && break
        end
        println("Top loss combined batch #$(toplossis[1:numtop]): (%$(100 * improvement) improvement) $(toplsorig) -> $(topls)")
        ####


        # lss = zeros(numtop)
        # improvements = zeros(numtop)
        # topi = 1
        # for _ in 1:100
        #     println("Improving batchi $(toplossis[topi])")
        #     # batchi = trainbatchis[toplosses[i]]
        #     x = xtops[topi] # batcher(batchi, variation) |> gpu
        #     for _ in 1:100
        #         Flux.Optimisers.adjust!(opt_state, rand(.05:0.001:1.2) * learningrate)
        #         ls, grads = Flux.withgradient(calcloss, model, x)
        #         ls /= size(x)[end] * lossbase
        #         lss[topi] = ls
        #         Flux.update!(opt_state, model, grads[1])
        #         improvements[topi] = (1 - ls / losses[toplossis[topi]])
        #         improvements[topi] >= 0.0001 && break
        #     end
        #     for othertopi in (1:numtop)[1:end .!= topi]
        #         ls = calcloss(model, xtops[othertopi])
        #         lss[othertopi] = ls
        #         improvements[othertopi] = (1 - ls / losses[toplossis[othertopi]])
        #     end
        #     topi = findfirst(k -> k < 0.0001, improvements)
        #     isnothing(topi) && @goto breakout
        # end
        # @label breakout
        # println("Top loss batches #$(toplossis[1:numtop]):")
        # println(join(["  #$(toplossis[topi]): (%$(100 * improvements[topi]) improvement) $(losses[toplossis[topi]]) -> $(lss[topi])" for topi in 1:numtop], '\n'))
        # for topi in 1:numtop
        #     losses[toplossis[topi]] = min(losses[toplossis[topi]], lss[topi])
        # end
        # Flux.Optimisers.adjust!(opt_state, learningrate)

        variation = (variation + 1) % batchlen
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
    path = joinpath(path_checkpoint(), "under-auto3-$(kh)-$(round(Int, datetime2unix(now(UTC)))).jld2")
    println("Saving checkpoint to $(path)")
    jldsave(path, model_state=Flux.state(cpu(kmodelgpu)), opt_state=cpu(kopt_state))
end

most_recent_file(path) = CollUtil.findMaxDom(mtime, readdir(path))

function checkpoint_load(fname = nothing)
    path = isnothing(fname) ? most_recent_file(path_checkpoint()) : joinpath(path_checkpoint(), fname)
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
test1(data, ind) = testx(xforindex(ind, data, hypers().seqlen))

function testx(x)
    if ndims(x) > 1
        x = dropdims(x; dims=ndims(x))
    end
    xgpu = x |> gpu
    yhatgpu = kmodelgpu(xgpu)
    yhat = yhatgpu |> cpu
    lossbase = calclossbase(reshape(x, (size(x)..., 1)))
    loss = calcloss(kmodelgpu, xgpu)
    ls = loss / lossbase
    @show lossbase loss ls
    draw(:scatter, x)
    draw!(:scatter, yhat)
end
#endregion

#region Data
import MLBatches
function makebatch(data, seqlen, batchlen, ind; variation)
    datalen = length(data) - seqlen
    indstart = 1 + (ind - 1) * batchlen
    println("Making batch for indstart $(indstart) + $(variation)")
    MLBatches.make_batch(xforindex, datalen, batchlen, indstart, data, seqlen; variation)
end

function xforindex(ind, data, seqlen)
    res = Array{Float32}(undef, seqlen)
    ind = ind + seqlen
    h0 = data[ind]
    seq = @view data[(ind-seqlen):(ind-1)]
    for i in 1:seqlen
        res[i] = 100f0 * log(seq[i] / h0)
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

        # Convert to float so interpolate won't complain about can't round
        df[!,:under] = Float32.(df[!,:under])
        df = DataUtil.interpolate(df, DateTime(2010,01,01):Minute(30):DateTime(2022,10,1), :quote_ts)

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

        Parquet2.writefile(fileinterp, df)
    end
    return df
end
#endregion

#region Explore
function findmaxxi(data)
    batcher = make_batcher(data, hypers().seqlen)
    lss = allbatchlosses(data, batcher)
    _, batchi = findmax(lss)
    b = batcher(batchi, 0)
    mx = maxforbatch(b)
    global kmx = mx
    return mx
end
function allbatchlosses(data, batcher)
    return [Autoencoder2.calcloss(Autoencoder2.kmodelgpu, batcher(i, 0) |> gpu) for i in 1:43]
end
function maxforbatch(batch)
    maxloss = -Inf
    maxx = nothing
    maxi = 0
    i = 1
    for x in eachslice(batch; dims=ndims(batch))
        xm = collect(reshape(x, (size(x)..., 1)))
        xgpu = xm |> gpu
        ls = calcloss(kmodelgpu, xgpu)
        @show ls
        if ls > maxloss
            maxloss = ls
            maxx = xm
            maxi = i
        end
        i += 1
    end
    return (maxi, maxloss, maxx)
end
#endregion

end
