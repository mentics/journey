module Series
# using Flux, Transformers, Transformers.Basic, CUDA
# using Flux: onehot
# using Transformers.Datasets: batched
# using Flux: gradient
# using Flux.Optimise: update!
import Flux: Flux,Dense,ADAM,gradient,gpu
import Flux.Optimise: update!
import Transformers: Transformer,TransformerDecoder,todevice,enable_gpu
import Transformers.Basic: PositionEmbedding
using CUDA
# import CUDA:gpu

#==
full = includes both input and forecast

import Transformers:enable_gpu
gpu_enabled = enable_gpu(true)
using HistData
using Series ; s = Series
s.runRet([d.close for d in dataDaily()]);
==#

function config()
    return (;
        inputDim = 1,
        inputEmbedDim = 512,
        numHeads = 8,
        neuronsPerHead = 64,
        hiddenSize = 2048,
        inputLen = 40,
        forecastLen = 2,
        fullLen = 40 + 2,

        batchSize = 32,
        trainIterMax = 1000,
        trainLossTarget = .1^3,
        dataMaxLength = 2000,
        dataTrainRatio = 0.7
    )
end

import CollUtil:mapRoll2
retMakeSeq(xs) = mapRoll2(xs) do x1, x2; 100*(x2/x1) end
retMakeSeq2(xs) = mapRoll2(xs) do x1, x2; 100*(x2/x1 - 1.0) end
retMakeData(prices, cfg=config()) = splitData(cfg, seqToFulls(cfg.fullLen, retMakeSeq2(prices))[1:(min(cfg.dataMaxLength, length(prices)-1))])

function retRunTrain(data, cfg=config(); reset=false)
    # TODO: take random samples for train and test so no time bias
    (!reset && isdefined(@__MODULE__, :model)) || ( println("Creating new model") ; global model = makeModel(cfg) )
    train!(cfg, model, data[1])
    return (cfg, model, data)
end
what() = isdefined(@__MODULE__, :model)

function retRunTest(model, data, cfg=config())
    test(cfg, model, data[2])
    return (cfg, model, data)
end

# makeSeqSine(len) = [sin(2π*i/10) for i in 1:len]
# function runSine(cfg=config())
#     global model = makeModel(cfg)
#     return run(model, seqToFulls(cfg.fullLen, makeSeqSine(800)), cfg)
# end

splitData(cfg, fulls) = ( lenTrain = round(Int, length(fulls) * cfg.dataTrainRatio) ; (fulls[1:lenTrain], fulls[(lenTrain+1):end]) )

import DrawUtil:draw,draw!
function test(cfg, model, dataTest)
    testOut = check(cfg, model, dataTest)
    # actual = [full[end:end,:] for full in test]
    actual = mapreduce(x->x[end:end,:], vcat, dataTest)
    println("Overall test loss: ", Flux.Losses.mse(testOut, actual))
    display(draw(actual[:]))
    draw!(testOut[:])
end

function seqToFulls(fullLen, seq)
    # Matrix{Float32} here indicates the type going into the input: a one dimensional matrix.
    res = Vector{Matrix{Float32}}()
    for i in 1:(length(seq) - fullLen)
        full = reshape(seq[i:i+fullLen-1],(fullLen,1))
        push!(res, full)
    end
    return res
    # train = [round.(rand(10); digits=3) for i in 1:100]
    # test = [round.(rand(10); digits=3) for i in 1:10]
    # return ((train, train), (test, test)) # x and y are the same because this is just a copy task
end

function makeEe(cfg)
    embed = Dense(cfg.inputDim => cfg.inputEmbedDim) |> gpu
    encode = PositionEmbedding(cfg.inputEmbedDim) |> gpu
    exec = function(x)
        # println("ee1 ", typeof(x), ' ', typeof(embed))
        # julia> loss(src, trg, trg_y)
        # ┌ Info: [2022-08-23T22:44:04] efor
        # │   typeof(x) = Array{Float32, 3}
        # └   typeof(e) = Matrix{Float32} (alias for Array{Float32, 2})

        embx = embed(x)
        # println("ee2")
        # println("check ", typeof(x), ' ', typeof(embx))
        # error("stop")
        encx = encode(embx)
        # println("ee3")
        return embx .+ encx
    end
    return (;exec, layers=(;embed, encode))
end

function makeEncoder(cfg, eeExec)
    encoder1 = Transformer(cfg.inputEmbedDim, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) |> gpu
    encoder2 = Transformer(cfg.inputEmbedDim, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) |> gpu
    exec = function(x)
        e = eeExec(x)
        # t1 = dropout_pos_enc(t1)
        t1 = encoder1(e)
        t2 = encoder2(t1)
        return t2
    end |> gpu
    return (;exec, layers=(;encoder1, encoder2))
end

function makeDecoder(cfg, eeExec)
    # decEncer = Dense(cfg.inputDim, cfg.inputEmbedDim) |> gpu
    decoder1 = TransformerDecoder(cfg.inputEmbedDim, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) |> gpu
    decoder2 = TransformerDecoder(cfg.inputEmbedDim, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) |> gpu
    linear = Dense(cfg.forecastLen * cfg.inputEmbedDim => cfg.forecastLen) |> gpu
    exec = function(shifted, encOut)
        esh = eeExec(shifted)
        # esh = decEncer(shifted)
        t3 = decoder1(esh, encOut)
        t4 = decoder2(esh, t3)
        flat = Flux.flatten(t4)
        p = linear(flat)
        return p
    end |> gpu
    return (;exec, layers=(;decoder1, decoder2, linear))
end

function makeModel(cfg)
    (fee, eeLayers) = makeEe(cfg)
    (fenc, encLayers) = makeEncoder(cfg, fee)
    (fdec, decLayers) = makeDecoder(cfg, fee)
    params = Flux.params(eeLayers..., encLayers..., decLayers...)
    opt = ADAM(1e-4)
    exec = function(x, shifted)
        # println("encoding ", typeof(x))
        encOut = fenc(x)
        # println("decoding ", typeof(x))
        return fdec(shifted, encOut)
    end |> gpu
    function floss(seq)
        x, shifted, y = splitSeq(cfg.inputLen, cfg.forecastLen, seq)
        devx, devshifted, devy = todevice(x, shifted, y) # move to gpu
        return function()
            out = exec(devx, devshifted)
            err = Flux.Losses.mse(out, devy)
            return err
        end
    end
    return (;exec, opt, params, floss)
end

function makeModel2(cfg)
    linear = Dense(cfg.inputLen => 2048) |> gpu
    linear2 = Dense(2048 => cfg.forecastLen) |> gpu
    params = Flux.params(linear)
    opt = ADAM(1e-4)
    function exec(x)
        out1 = linear(x)
        out2 = linear2(out1)
        return out2
    end
    function floss(seq)
        x, _, y = splitSeq(cfg.inputLen, cfg.forecastLen, seq)
        x = x[1,:,:]
        # y = y[1,:,:]
        devx, devy = todevice(x, y)
        return function()
            # println("linear ", typeof(devx), ' ', size(devx))
            out = exec(devx)
            # println("out ", typeof(out), ' ', size(out))
            # println("devy ", typeof(devy), ' ', size(devy))
            err = Flux.Losses.mse(out, devy)
            return err
        end
    end
    return (;exec, opt, params, floss)
end

function train!(cfg, model, fulls)
    println("Training")
    mfulls = reduce(hcat, fulls)
    global batches = Flux.Data.DataLoader(mfulls; batchsize=cfg.batchSize)
    println("Iterating")
    for i in 1:cfg.trainIterMax
        for batch in batches
            sz = size(batch)
            seq = reshape(batch, (cfg.inputDim, sz[1], sz[2])) # size(seq) = (inputDim, fullLen, batchSize)
            floss = model.floss(seq)
            grad = gradient(floss, model.params)
            update!(model.opt, model.params, grad)
        end
        if i % 10 == 0
            err = 0.0
            len = 0
            for batch in batches
                sz = size(batch)
                seq = reshape(batch, (cfg.inputDim, sz[1], sz[2])) # size(seq) = (inputDim, fullLen, batchSize)
                err += model.floss(seq)()
                len += 1
            end
            errAvg = err / len
            println("$(i): loss = $(errAvg)")
            flush(stdout)
            errAvg > cfg.trainLossTarget || ( println("Reached target, stopping.") ; break )
        end
    end
end

function check(cfg, model, fulls::AbstractVector{T})::T where T
    println("Checking")
	seqOut = Array{Float32}(undef, 0, size(fulls[1])[2])
    # println("check ", typeof(seqOut))
    mfulls = reduce(hcat, fulls)
    batches = Flux.Data.DataLoader(mfulls; batchsize=cfg.batchSize)
    for batch in batches
        sz = size(batch)
        seq = reshape(batch, (cfg.inputDim, sz[1], sz[2])) # size(seq) = (inputDim, fullLen, batchSize)
        full = seq[:,1:cfg.fullLen,:]
        full = todevice(full)
        x = full[:,1:cfg.inputLen,:]
        shifted = full[:,cfg.inputLen:sz[1]-1,:]
        println("x ", typeof(x), size(x))
        println("shifted ", typeof(shifted), size(shifted))
        out = model.exec(x, shifted)
        # out = model.exec(x)

        # global out = model.exec(x, shifted)
        # global outEnd = out[end:end,:]
        # println(typeof(out), ' ', size(out))
        # println(typeof(outEnd), ' ', size(outEnd'))
		seqOut = vcat(seqOut, out[end:end,:]')
		# seqOut = vcat(seqOut, collect(out[end,:]))
        # append!(seqOut, collect(out[end:end,:]))
    end
    return seqOut
end

function splitSeq(inputLen, forecastLen, sequence)
    # sequence has 3 dims: inputDim, fullLen, batchSize
	fullLen = size(sequence)[2]
	@assert fullLen == inputLen + forecastLen
	x = sequence[:,1:inputLen,:]
	shifted = sequence[:,inputLen:fullLen-1,:]
	@assert size(shifted)[2] == forecastLen
	y = sequence[:,fullLen-forecastLen+1:fullLen,:]
	@assert size(y)[2] == forecastLen
	if size(y)[1] == 1
	 	return x, shifted, dropdims(y; dims=1)
	else
		return x, shifted, y
	end
end

#region Local
# __init__() = enable_gpu(true)
#endregion

end