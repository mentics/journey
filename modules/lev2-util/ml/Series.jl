module Series
# using Flux, Transformers, Transformers.Basic, CUDA
# using Flux: onehot
# using Transformers.Datasets: batched
# using Flux: gradient
# using Flux.Optimise: update!

function config()
    return (;
        inputDim = 1,
        inputEmbed = 8,
        numHeads = 2,
        neuronsPerHead = 8,
        hiddenSize = 16,
        forecastLength = 4
    )
end

function run(cfg=config())
    model = makeModel(cfg)
    train, test = makeData()
    train!(model, train)
    check(model, test)
end

function makeData()
    train = [round.(rand(10); digits=3) for i in 1:100]
    test = [round.(rand(10); digits=3) for i in 1:10]
    return ((train, train), (test, test)) # x and y are the same because this is just a copy task
end

function makeEe(cfg)
    embed = Dense(cfg.inputDim, cfg.inputEmbed) # |> gpu
    encode = PositionEmbedding(cfg.inputEmbed) # |> gpu
    exec = function(x)
        we = embed(x)
        return we .+ encode(we)
    end
    return (;exec, layers=(;embed, encode))
end

function makeEncoder(cfg, eeExec)
    encoder1 = Transformer(cfg.inputEmbed, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) # |> gpu
    encoder2 = Transformer(cfg.inputEmbed, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) # |> gpu
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
    decoder1 = TransformerDecoder(cfg.inputEmbed, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) # |> gpu
    decoder2 = TransformerDecoder(cfg.inputEmbed, cfg.numHeads, cfg.neuronsPerHead, cfg.hiddenSize) # |> gpu
    linear = Dense(cfg.forecastLength * cfg.inputEmbed, cfg.forecastLength) |> gpu
    exec = function(x, m)
        e = eeExec(x)
        t1 = decoder1(e, m)
        t2 = decoder2(t1, m)
        flat = Flux.flatten(t2)
        p = linear(flat)
        return p
    end
    return (;exec, layers=(;decoder1, decoder2, linear))
end

function makeModel(cfg)
    (;ee, eeLayers) = makeEe(cfg)
    (;enc, encLayers) = makeEncoder(cfg, ee)
    (;dec, decLayers) = makeDecoder(cfg, ee)
    params = Flux.params(eeLayers..., encLayers..., decLayers...)
    opt = ADAM(1e-4)
    exec = function(x)
        x1 = enc(x)
        x2 = dec(x, x1)
    end
    return (;exec, opt, params)
end

function loss(x, y)
    enc = encoder_forward(x)
    dec = decoder_forward(x, enc)
    err = Flux.Losses.mse(dec, y)
    return err
end

function train!(model, xy)
    # i = 0
    # for (x, y) in xys
        # x, y = todevice(x, y) # move to gpu
        grad = gradient(()->loss(x, y), ps)
        update!(model.optimizer, model.params, grad)
    #     i += 1
    #     if i % 8 == 0
    #         l = loss(x, y)
    #         println("loss = $l")
    #     end
    # end
end

function check(model, xy)
end

function splitSeq(sequence, enc_seq_len, target_seq_len)
    # sequence has 3 dims: input size, sequence+target length, batch size
    fullLen is seq + targ len
	fullLen = size(sequence)[2]
	@assert fullLen == enc_seq_len + target_seq_len
	src = sequence[:,1:enc_seq_len,:]
	trg = sequence[:,enc_seq_len:fullLen-1,:]
	@assert size(trg)[2] == target_seq_len
	trg_y = sequence[:,fullLen-target_seq_len+1:fullLen,:]
	@assert size(trg_y)[2] == target_seq_len
	if size(trg_y)[1] == 1
	 	return src, trg, dropdims(trg_y; dims=1)
	else
		return src, trg, trg_y
	end
end

#region Local
# __init__() = enable_gpu(true)
#endregion

end