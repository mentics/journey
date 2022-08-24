module Series
# using Flux, Transformers, Transformers.Basic, CUDA
# using Flux: onehot
# using Transformers.Datasets: batched
# using Flux: gradient
# using Flux.Optimise: update!
import Flux: Flux,Dense,ADAM,gradient
import Flux.Optimise: update!
import Transformers: Transformer,TransformerDecoder
import Transformers.Basic: PositionEmbedding

#==
full = includes both input and forecast
==#

function config()
    return (;
        inputDim = 1,
        inputEmbedDim = 8,
        numHeads = 2,
        neuronsPerHead = 8,
        hiddenSize = 16,
        inputLen = 20,
        forecastLen = 4,
        fullLen = 20 + 4,

        batchSize = 256,
        trainIterMax = 1000,
        trainLossTarget = .001
    )
end

function run(cfg=config())
    global model = makeModel(cfg)

    dataTrain, _ = makeData(500, 200, cfg.fullLen)

    train!(cfg, model, dataTrain)
    test(cfg, model)
    return model
end

import DrawUtil:draw,draw!
function test(cfg, model)
    _, dataTest = makeData(500, 200, cfg.fullLen)
    testOut = check(cfg, model, dataTest)
    # actual = [full[end:end,:] for full in test]
    actual = mapreduce(x->x[end:end,:], vcat, dataTest)
    println("Overall test loss: ", Flux.Losses.mse(testOut, actual))
    display(draw(actual[:]))
    draw!(testOut[:])
end

function makeData(lenTrain, lenTest, fullLen)
    # lenTrain = 500
    # lenTest = 200
    seqAll = makeSeq(lenTrain + lenTest, fullLen)
    fulls = seqToFulls(seqAll, fullLen)
    @assert length(fulls) == lenTrain + lenTest
    train = fulls[1:lenTrain]
    test = fulls[(lenTrain+1):end]
    @assert length(test) == lenTest
    return (train, test)
end

makeSeq(cntAll, fullLen) = [sin(2π*i/10) for i in 1:(cntAll + fullLen)]

function seqToFulls(seq, fullLen)
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
    embed = Dense(cfg.inputDim => cfg.inputEmbedDim) # |> gpu
    encode = PositionEmbedding(cfg.inputEmbedDim) # |> gpu
    exec = function(x)
        # julia> loss(src, trg, trg_y)
        # ┌ Info: [2022-08-23T22:44:04] efor
        # │   typeof(x) = Array{Float32, 3}
        # └   typeof(e) = Matrix{Float32} (alias for Array{Float32, 2})

        embx = embed(x)
        # println("check ", typeof(x), ' ', typeof(embx))
        # error("stop")
        encx = encode(embx)
        return embx .+ encx
    end
    return (;exec, layers=(;embed, encode))
end

function makeEncoder(cfg, eeExec)
    encoder1 = Transformer(cfg.inputEmbedDim, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) # |> gpu
    encoder2 = Transformer(cfg.inputEmbedDim, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) # |> gpu
    exec = function(x)
        e = eeExec(x)
        # t1 = dropout_pos_enc(t1)
        t1 = encoder1(e)
        t2 = encoder2(t1)
        return t2
    end
    return (;exec, layers=(;encoder1, encoder2))
end

function makeDecoder(cfg, eeExec)
    # decEncer = Dense(cfg.inputDim, cfg.inputEmbedDim) # |> gpu
    decoder1 = TransformerDecoder(cfg.inputEmbedDim, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) # |> gpu
    decoder2 = TransformerDecoder(cfg.inputEmbedDim, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) # |> gpu
    linear = Dense(cfg.forecastLen * cfg.inputEmbedDim => cfg.forecastLen) # |> gpu
    exec = function(shifted, encOut)
        esh = eeExec(shifted)
        # esh = decEncer(shifted)
        t3 = decoder1(esh, encOut)
        t4 = decoder2(esh, t3)
        flat = Flux.flatten(t4)
        p = linear(flat)
        return p
    end
    return (;exec, layers=(;decoder1, decoder2, linear))
end

function makeModel(cfg)
    (fee, eeLayers) = makeEe(cfg)
    (fenc, encLayers) = makeEncoder(cfg, fee)
    (fdec, decLayers) = makeDecoder(cfg, fee)
    params = Flux.params(eeLayers..., encLayers..., decLayers...)
    opt = ADAM(1e-4)
    exec = function(x, shifted)
        encOut = fenc(x)
        return fdec(shifted, encOut)
    end
    return (;exec, opt, params)
end

function loss(model, x, shifted, y)
    out = model.exec(x, shifted)
    err = Flux.Losses.mse(out, y)
    return err
end

function train!(cfg, model, fulls)
    mfulls = reduce(hcat, fulls)
    global batches = Flux.Data.DataLoader(mfulls; batchsize=cfg.batchSize)
    los = 100.0
    for i in 1:cfg.trainIterMax
        for batch in batches
            sz = size(batch)
            seq = reshape(batch, (cfg.inputDim, sz[1], sz[2])) # size(seq) = (inputDim, fullLen, batchSize)
            x, shifted, y = splitSeq(cfg.inputLen, cfg.forecastLen, seq)
            # devx, devshifted, devy = todevice(x, shifted, y) # move to gpu
            grad = gradient(()->loss(model, x, shifted, y), model.params)
            update!(model.opt, model.params, grad)

            if i % 8 == 0
                los = loss(model, x, shifted, y)
                println("loss = $los")
            end
        end
        los > cfg.trainLossTarget || break
    end
end

function check(cfg, model, fulls::AbstractVector{T})::T where T
	seqOut = Array{Float32}(undef, 0, size(fulls[1])[2])
    # println("check ", typeof(seqOut))
    mfulls = reduce(hcat, fulls)
    batches = Flux.Data.DataLoader(mfulls; batchsize=cfg.batchSize)
    for batch in batches
        sz = size(batch)
        seq = reshape(batch, (cfg.inputDim, sz[1], sz[2])) # size(seq) = (inputDim, fullLen, batchSize)
        full = seq[:,1:cfg.fullLen,:]
        # full = todevice(full)
        x = full[:,1:cfg.inputLen,:]
        shifted = full[:,cfg.inputLen:sz[1]-1,:]
        out = model.exec(x, shifted)
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