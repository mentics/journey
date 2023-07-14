module Autoencoder3
using Dates, IterTools
using Flux, NNlib, MLUtils, CUDA
using CudaUtil
using DataUtil

function hypers()
    encodedwidth = 128
    numlayers = 4
    lentarget = 4000 + 88 - encodedwidth
    inputwidth = round(Int, lentarget / numlayers, RoundUp) * numlayers + encodedwidth # numlayers ^ round(Int, log(numlayers, lentarget), RoundUp)
    inputwidthvix = round(Int, inputwidth / 49, RoundUp)
    inputwidthunder = inputwidth - inputwidthvix
    activation = NNlib.swish
    return (;
        encodedwidth, numlayers, inputwidthunder, inputwidthvix, inputwidth, activation
    )
end

#vixlen = (len - vixlen) / 48
#vixlen = len / 48 - vixlen / 48
#49/48 * vixlen = len / 48
#vixlen = len/49

function info(data)
    hyps = hypers()
    learningrate = 1e-4
    batchlen = 4096
    # underobs = size(hyps.under, 1) - hyps.inputwidthunder
    # not sure if I need to check that vix len back will work, but it seemed to on first hyps
    # data.vixlup[hyps.inputwidthunder]
    # vixobs = size(hyps.under, 1) - hyps.inputwidthunder
    numobs = size(data.under, 1) - hyps.inputwidthunder
    numbatches = round(Int, numobs / batchlen, RoundUp)
    spliti = round(Int, 0.8 * numbatches, RoundDown)
    trainbatchis = 1:spliti
    testbatchis = (spliti+1):numbatches
    return (;
        learningrate, batchlen, numobs, trainbatchis, testbatchis
    )
end

function make_data()
    under = DataUtil.make_data_under()
    vix = DataUtil.make_data_vix()
    numunders = size(under, 1)
    vixlup = Vector{Int}(undef, numunders)
    vixi = 1
    for i in 1:numunders
        if Date(under.ts[i]) == vix.ts[vixi]
            vixlup[i] = vixi
        else
            vixi += 1
            @assert Date(under.ts[i]) == vix.ts[vixi]
            vixlup[i] = vixi
        end
    end
    return (;under=under.x, vix=vix.x, vixlup)
end

function model_encoder(inputwidth, encodedwidth, numlayers, activation)
    step = (encodedwidth - inputwidth) ÷ numlayers
    layerwidths = collect(inputwidth:step:encodedwidth)
    return Chain([Dense(layerwidths[i-1] => layerwidths[i],
                    # i == (numlayers + 1) ? identity : activation;
                    i == 2 ? identity : activation;
                    bias = i == 2
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
    (;inputwidth, encodedwidth, numlayers, activation) = hypers
    return Chain(model_encoder(inputwidth, encodedwidth, numlayers, activation), model_decoder(inputwidth, encodedwidth, numlayers, activation))
end

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
    yhat = model(x)
    return lossfunc(x, yhat)
end
lossfunc(x, yhat) = Flux.Losses.mse(x, yhat)

function run(data; iters=10)
    hyps = hypers()
    derinfo = info(data)
    modelcpu = make_model(hyps)
    global kmodelcpu = modelcpu
    model = modelcpu |> gpu
    global kmodelgpu = model
    opt = AdamW(derinfo.learningrate)
    opt_state = gpu(Flux.setup(opt, model))
    global kopt = opt
    global kopt_state = opt_state

    #for i in 1:encodedwidth
        #setmask(i)
        train(make_batcher(data, hyps, derinfo), model, opt_state, derinfo; iters)
    #end
end

function make_batcher(data, hyps, derinfo)
    return function(batchi, variation)
        return makebatch(data, hyps, derinfo, batchi; variation)
    end
end

function train_continue(data, model=kmodelgpu, opt_state=kopt_state; iters=10)
    global kmodelgpu = gpu(model)
    global kopt_state = gpu(opt_state)
    derinfo = info(data)
    train(make_batcher(data, hypers().seqlen, derinfo.batchlen), kmodelgpu, kopt_state, derinfo; iters)
end

function train(batcher, model, opt_state, derinfo; iters=10)
    variation = 0
    (;learningrate, batchlen, trainbatchis) = derinfo
    # trainbatchis = trainbatchis[1:10]
    # (;seqlen) = hypers()
    # println("lossbase: ", lossbase)
    last_save = now(UTC)
    numbatches = length(trainbatchis)
    lossesmin = fill(Inf, numbatches)
    # global klosses = lossesmin
    lossbases = Vector{Float32}(undef, numbatches)
    for epoch in 1:iters
        if (variation % 10) == 0
            print("Updating lossbases...")
            Threads.@threads for i in eachindex(trainbatchis)
                lossbases[i] = DataUtil.calclossbase(lossfunc, batcher(trainbatchis[i], variation))
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
        toplossbase = calclossbase(lossfunc, topbatchcpu)
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

        variation = (variation + 1) % batchlen
    end
end

# around(i, mx) = ( i = clamp(i, 1, mx-1) ; return (i-1):(i+1) )

#region Persist
checkpoint_save() = DataUtil.checkpoint_save(string.(values(hypers())), kmodelgpu, kopt_state)
checkpoint_load() = DataUtil.checkpoint_save(string.(values(hypers())), kmodelgpu, kopt_state)
#endregion

#region Testing
using DrawUtil
test1(data, ind) = testx(xforindex(ind, data, hypers()))

function testx(x)
    if ndims(x) > 1
        x = dropdims(x; dims=ndims(x))
    end
    xgpu = x |> gpu
    yhatgpu = kmodelgpu(xgpu)
    yhat = yhatgpu |> cpu
    lossbase = DataUtil.calclossbase(lossfunc, reshape(x, (size(x)..., 1)))
    loss = calcloss(kmodelgpu, xgpu)
    ls = loss / lossbase
    @show lossbase loss ls
    draw(:scatter, x)
    draw!(:scatter, yhat)
end
#endregion

#region Data
import MLBatches
function makebatch(data, hyps, derinfo, ind; variation)
    indstart = 1 + (ind - 1) * derinfo.batchlen
    println("Making batch for indstart $(indstart) + $(variation)")
    MLBatches.make_batch(xforindex, derinfo.numobs, derinfo.batchlen, indstart, data, hyps; variation)
end

function xforindex(ind, data, hyps)
    res = Array{Float32}(undef, hyps.inputwidth)
    underfirsti = ind
    underlasti = ind + hyps.inputwidthunder - 1
    h0 = data.under[underlasti + 1]
    i = 1
    for underi in underfirsti:underlasti
        res[i] = 100f0 * log(data.under[underi] / h0)
        i += 1
    end
    vixlasti = data.vixlup[underlasti]
    for vixi in (vixlasti - hyps.inputwidthvix + 1):vixlasti
        res[i] = data.vix[vixi]
        i += 1
    end
    # return (;i, underrange=underfirsti:underlasti, vixrange=(vixlasti - hyps.inputwidthvix + 1):vixlasti)
    return res
end
#endregion

end
