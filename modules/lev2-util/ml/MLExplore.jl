module MLExplore
using Compat
import Random
using Flux, Transformers, CUDA
using Transformers.Layers
using NeuralAttentionlib.Masks
using BaseTypes

# const DATA_SPLIT4 = NamedTuple{(:train, :validation), Tuple{A,B}} where {A<:AbstractArray,B<:AbstractArray}
# const DATA_XY_SPLIT = NamedTuple{(:train, :validation), Tuple{A,B}} where {A<:AbstractArray,B<:AbstractArray}

#=
To run test:
mlx.run()

Dimension convention:
Time X Channels(can be multidimensional) X Batch
Flux expects the last dim to be batch

References:
* https://chengchingwen.github.io/Transformers.jl/dev/tutorial/
* (but this uses old Transformers API:) https://iwasnothing.medium.com/use-transformer-in-julia-8409d2c0b642 and the git repo for that https://github.com/iwasnothing/julia_transformer_ts/blob/main/timeseries-transformer.jl
* Possibly help for attention mask: https://github.com/chengchingwen/NeuralAttentionlib.jl/blob/master/test/mask.jl

=#

const DEV = Ref(gpu)

function makeCfg()
    return (;
        # Data related
        channel_width = 1,

        # Model related
        # General Lengths
        enc_input_width = 180, #300,
        enc_output_width = 180, #300,
        dec_input_width = 2,
        dec_output_width = 2,

        # Transformer Config
        trf_input_width = 128, #512,
        trf_block_count = 2, #4,
        trf_hidden_dim = 128, #512,
        trf_head_count = 8,
        trf_head_dim = 64,
        trf_ffn_dim = 1024, #2048,
        trf_dropout = 0.1,

        # Execution related
        validation_ratio = 0.2,
        batch_len = 128,
        train_iters = 10,
        target_loss = 1e-4,
        learning_rate = 1e-4,
        learning_rate_mult = 1.2,
    )
end

seq_len(cfg) = cfg.enc_input_width + cfg.dec_output_width
input_len(cfg) = cfg.enc_input_width + cfg.dec_input_width - 1

function run(cfg=makeCfg(), input=makeSeqRets())
    enable_gpu(true) # CUDA.functional())
    Random.seed!(1)

    global gcfg = cfg
    global dataTokens = tokenizeData(input)
    # global dataSplit = splitTrainValidation(dataTokens, cfg.validation_ratio)
    # global xySplitBatched = makeBatchesSplit(cfg, dataSplit)
    global xySplitBatched = makeBatches(cfg, dataTokens)
    global model = makeModel1(cfg)
    train!(cfg, model, xySplitBatched)
    starti = 1 # rand(1:(size(dataTokens,1) - input_len(cfg)))
    global gstart = dataTokens[starti:(starti + input_len(cfg) - 1),:]
    gen_count = input_len(cfg)
    global genned = generate(cfg, model, gstart, gen_count)
    global gactual = dataTokens[starti:min(sizeTime(dataTokens),sizeTime(genned)),:]
    plotGenned(genned, gactual)
    return starti, genned
end

# This outputs a lenth x 2 matrix (sequence)
function makeSeqSinCos(length = 20 * 20 * 2 * π + 5)
    res = reduce(vcat, [Float32(sin(i/20.0)) Float32(cos(i/20.0))] for i in 1:length)
    return res
end

function makeSeqTrigSum1(length = 40 * 20 * 2 * π + 5)
    res = reduce(vcat, [Float32((sin(i/6.0) + cos(7.0 + i/11.0))/2)] for i in 1:length)
    return reshape(res, sizeTime(res), 1)
end

function makeSeqTrigSum2(length = 40 * 20 * 2 * π + 5)
    res = reduce(vcat, trigSum2(i) for i in 1:length)
    return reshape(res, sizeTime(res), 2)
end

trigSum2(i) = [Float32((sin(i/6.0) + cos(7.0 + i/11.0) + sin(π/3 + i/252))/3) Float32(sin(i/23.0))]

function makeSeqTrigSum2Delta(length = 40 * 20 * 2 * π + 5)
    res = reduce(vcat, 100 * (trigSum2(i) .- trigSum2(i-1)) for i in 1:length)
    return reshape(res, sizeTime(res), 2)
end

import HistData
function makeSeqRets()
    rets = HistData.makeRets(HistData.dataDaily(), 1)
    return reshape(rets.rets, sizeTime(rets.rets), 1)
end

function makeSeqSpy()
    rets = [row.close for row in HistData.dataDaily()]
    return reshape(rets, sizeTime(rets), 1)
end

# This doesn't have to return the same size as the input
function tokenizeData(m)
    # TODO
    return m
end

# # TODO: this is generic, can be moved to util
# # TODO: this should grab random chunks because using last data could be skewed
# function splitTrainValidation(dataTokens, validationRatio)
#     len = size(dataTokens, 1) # sequence dim is first at this point
#     validation_count = max(1, round(Int, validationRatio * len))
#     train_count = len - validation_count
#     println("Split to $(train_count) training and $(validation_count) validation")
#     return (; train = (@view dataTokens[1:train_count,:]), validation = (@view dataTokens[train_count+1:len,:]))
# end

# TODO: this is generic, can be moved to util
import MaxLFSR
function makeBatches(cfg, dataTokens)
    empty!(Inds[])
    ts_count = size(dataTokens, 1)
    subseq_count = ts_count - seq_len(cfg) - 1
    batch_count = subseq_count ÷ cfg.batch_len
    validation_batch_count = max(1, ceil(Int, cfg.validation_ratio * batch_count))
    training_batch_count = batch_count - validation_batch_count

    if validation_batch_count <= 0
        @show ts_count subseq_count batch_count validation_batch_count train_batch_count
        error("Insufficient data $(ts_count) for seq_size $(seq_len(cfg)) and batch len $(batch_len)")
    end

    ind = 1
    lfsr = MaxLFSR.LFSR(subseq_count; seed=ind)
    println("Creating $(training_batch_count) training batches")
    train_x, ind = batches(cfg, training_batch_count, lfsr, ind)
    println("Creating $(validation_batch_count) validation batches")
    validation_x, ind = batches(cfg, validation_batch_count, lfsr,ind)
    train_x = train_x |> DEV[]
    validation_x = validation_x |> DEV[]
    return (; train=(;x=train_x), validation=(;x=validation_x))
end

const Inds = Ref(Int[])

function batches(cfg, count, lfsr, ind)
    seqlen = seq_len(cfg)
    batchlen = cfg.batch_len
    x = Array{Float32, ndims(dataTokens)+1}[]
    for _ in 1:count
        batch = stack(begin
            # println("Adding to batch for index $ind")
            push!(Inds[], ind)
            seq = dataTokens[ind:(ind + seqlen - 1),:]
            range = 0.0:(π/(seqlen-1)/2):π/2
            ind, _ = iterate(lfsr, ind)
            cat(seq, [cos.(range) sin.(range)]; dims=ndims(seq))
        end for _ in 1:batchlen)
        push!(x, batch)
        @assert size(batch) == (seqlen, sizeChannel(batch)..., batchlen) "$(size(batch)) == $((seqlen, sizeChannel(batch), batchlen))"
    end
    return x, ind
end

# function makeBatches(dataTokens, seq_len, batch_len)
#     ts_count = size(dataTokens, 1)
#     subseq_count = ts_count - seq_len - 1
#     batch_count = subseq_count ÷ batch_len
#     if subseq_count < 0 || batch_count == 0
#         @show ts_count subseq_count batch_count
#         error("Insufficient data $(ts_count) for seq_size $(seq_len) and batch len $(batch_len)")
#     end
#     println("Creating $(batch_count) batches")
#     x = Array{Float32, ndims(dataTokens)+1}[]
#     y = Array{Float32, ndims(dataTokens)+1}[]
#     for batchi in 1:batch_count
#         # Batch is last dim and that's the default for stack
#         batch = stack(dataTokens[(batchi + i):(batchi + i + seq_len - 1),:] for i in 1:batch_len)
#         push!(x, batch)
#         # batch = stack(dataTokens[i:i + seq_len - 1,:] for i in 1:subseq_count)
#         @assert size(batch) == (seq_len, sizeChannel(batch)..., batch_len) "$(size(batch)) == $((seq_len, sizeChannel(batch), batch_len))"
#     end
#     x = x |> DEV[]
#     # y = y |> DEV[]
#     return (; x, y)
# end

function ins_and_outs(cfg, seq)
    enc_input, dec_input = ins(cfg, seq)
    (;enc_input_width, dec_output_width) = cfg
    seq_len = size(seq, 1)
    @assert seq_len == enc_input_width + dec_output_width (@str seq_len == enc_input_width + dec_output_width seq_len enc_input_width dec_output_width)
    dec_output = seq[seq_len - dec_output_width + 1:seq_len,:,:]
    @assert size(dec_output, 1) == dec_output_width
    return enc_input, dec_input, dec_output
end

function ins(cfg, seq)
    (;enc_input_width, dec_input_width) = cfg
    seq_len = size(seq, 1)
    @assert seq_len >= enc_input_width + dec_input_width - 1 (@str seq_len >= enc_input_width + dec_input_width - 1 seq_len enc_input_width dec_input_width)
    enc_input = seq[1:enc_input_width,:,:]
    dec_input = seq[enc_input_width:(enc_input_width + dec_input_width - 1),:,:]
    @assert size(dec_input, 1) == dec_input_width
    return enc_input, dec_input
end

function makeModel1(cfg)
    # The diagram on page 2 of this paper is most informative
    # https://arxiv.org/pdf/2001.08317.pdf

    encoder_input_layer = Dense(cfg.enc_input_width => cfg.trf_input_width) |> DEV[]
    # positional_encoding_layer = SinCosPositionEmbed(cfg.trf_input_width) |> DEV[]
    # positional_encoding_layer = SinCosPositionEmbed(cfg.trf_input_width) |> DEV[]
    # positional_encoding = positional_encoding_layer(cfg.enc_input_width)
    # positional_encoding_layer = FixedLenPositionEmbed(cfg.trf_input_width) |> DEV[]
    encoder_trf = Transformer(TransformerBlock, cfg.trf_block_count, cfg.trf_head_count, cfg.trf_hidden_dim, cfg.trf_head_dim, cfg.trf_ffn_dim; dropout=cfg.trf_dropout) |> DEV[]
    decoder_input_layer = Dense(cfg.dec_input_width => cfg.trf_input_width) |> DEV[]
    decoder_trf = Transformer(TransformerDecoderBlock, cfg.trf_block_count, cfg.trf_head_count, cfg.trf_hidden_dim, cfg.trf_head_dim, cfg.trf_ffn_dim; dropout=cfg.trf_dropout) |> DEV[]
    decoder_output_layer = Dense(cfg.trf_hidden_dim => cfg.dec_output_width) |> DEV[]

    # positional_encoding_layer
    all_layers = [encoder_input_layer, encoder_trf, decoder_trf, decoder_input_layer]

    function embedding(input)
        # we = word_embed(input.token)
        # pe = pos_embed(we)
        enced = encoder_input_layer(input)
        return enced
        println(sizeTime(enced))
        error("stop")
        # posed = positional_encoding_layer(sizeTime(enced))
        return enced .+ posed
    end

    function encoder_forward(input)
        # attention_mask = get(input, :attention_mask, nothing)
        # e = embedding(input)
        # t = encoder_trf(e, attention_mask) # return a NamedTuples (hidden_state = ..., ...)
        e = embedding(input)
        att_mask = CausalMask()
        t = encoder_trf(e, att_mask) # return a NamedTuples (hidden_state = ..., ...)
        return t.hidden_state
    end

    function decoder_forward(dec_input, enc_output)
        # attention_mask = get(input, :attention_mask, nothing)
        # cross_attention_mask = get(input, :cross_attention_mask, nothing)
        # e = embedding(input)
        # t = decoder_trf(e, m, attention_mask, cross_attention_mask) # return a NamedTuple (hidden_state = ..., ...)
        enced = decoder_input_layer(dec_input)
        att_mask = CausalMask()
        cross_attention_mask = nothing
        t = decoder_trf(enced, enc_output, att_mask, cross_attention_mask) # return a NamedTuple (hidden_state = ..., ...)
        # p = embed_decode(t.hidden_state)
        p = decoder_output_layer(t.hidden_state)
        return p
    end

    exec = function(enc_input, dec_input)
        enced = encoder_forward(enc_input)
        deced = decoder_forward(dec_input, enced)
        return deced
    end

    function loss(enc_input, dec_input, y)
        yhat = exec(enc_input, dec_input)
        return Flux.Losses.mse(yhat, y)
    end

    params = Flux.params(all_layers)
    opt = Adam(cfg.learning_rate)

    return (;cfg, exec, opt, params, loss, all_layers)
end

function train!(cfg, model, data)
    Flux.trainmode!(model)
    prevLoss = 1e10
    loss = 0
    batch_count = sizeBatch(data.train.x)
    min_loss = Inf
    for i in 1:cfg.train_iters
        ls = 0
        for batch in 1:batch_count
            x = data.train.x[batch]
            # y = data.train.y[:,:,batch]
            enc_input, dec_input, y = ins_and_outs(cfg, x)
            grad = gradient(() -> model.loss(enc_input, dec_input, y), model.params)
            Flux.update!(model.opt, model.params, grad)
            ls += model.loss(enc_input, dec_input, y)
            yield()
        end
        loss = ls / batch_count

        if loss < min_loss
            model.opt.eta *= cfg.learning_rate_mult
            min_loss = loss
        elseif loss > min_loss + cfg.learning_rate_mult
            # model.opt.eta /= cfg.learning_rate_mult
            model.opt.eta = max(1e-7, model.opt.eta / cfg.learning_rate_mult)
        end

        validation_loss = validationLoss()
        println("iter $(i) training loss: ", loss, " validation loss: ", validation_loss, " learning rate: ", model.opt.eta)
        if loss < cfg.target_loss
            @goto done
        end
        prevLoss = loss
    end
    @label done
    println("Final loss: ", loss)
    Flux.testmode!(model)
end

function validationLoss()
    Flux.testmode!(model)
    batches = xySplitBatched.validation.x
    loss = 0
    for x in batches
        enc_input, dec_input, y = ins_and_outs(gcfg, x)
        loss += model.loss(enc_input, dec_input, y)
        yield()
    end
    Flux.trainmode!(model)
    return loss / sizeBatch(batches)
end

function generate(cfg, model, start, forecast_count)
    Flux.testmode!(model)
    # T x C x B
    start = reshape(start, size(start)..., 1)
    @assert size(start) == (input_len(cfg), sizeChannel(start)..., 1) string("expected ", (input_len(cfg), sizeChannel(start), 1), " but was ", size(start))
    res = gpu(start)
    # enc_input, _ = ins(cfg, res)
    for _=1:forecast_count
        enc_input, dec_input = ins(cfg, res[(end - input_len(cfg) + 1):end,:,:])
        # _, dec_input = ins(cfg, res[(end - input_len(cfg) + 1):end,:,:])
        y = model.exec(enc_input, dec_input)
        yend = y[end:end,:,:]
        res = cat(res, yend; dims=1)
    end
    res_cpu = cpu(res)
    return dropdims(res_cpu, dims=ndims(res_cpu))
end

import GLMakie, DrawUtil
import ColorTypes:RGB,RGBA
function plotGenned(genned, actual)
    display(GLMakie.lines(actual[:,1]; color=RGBA(.5, .0, .0, .5)))
    # display(GLMakie.lines!(actual[:,2]; color=RGBA(.0, .5, .0, .5)))

    display(GLMakie.lines!(genned[:,1]; color=RGBA(.9, .0, .0, .8)))
    # display(GLMakie.lines!(genned[:,2]; color=RGBA(.0, .9, .0, .8)))

    GLMakie.vlines!(input_len(gcfg); color=:green)
end

function plotgen()
    # genned = generate(gcfg, model, gstart, count)
    plotGenned(genned, gactual)
end

# batch is always the last dim
sizeBatch(x) = size(x, ndims(x))
# time is always the first dim
sizeTime(x) = size(x, 1)
sizeChannel(x) = size(x)[2:(ndims(x)-1)]

end