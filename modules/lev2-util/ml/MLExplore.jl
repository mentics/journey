module MLExplore
using Compat, Dates
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

const MULT = Ref(4)

function makeCfg()
    return (;
        # Data related
        channel_width = 4, # 23 + 2, # add 2 for the positional encoding
        encode_width = 16,

        ## Model related
        # General Lengths
        enc_input_width = 100, # number of timesteps in a single input to the encoder
        enc_output_width = 100,
        dec_input_width = 10,
        dec_output_width = 10,

        # Transformer Config
        # trf_input_width = 128 * MULT[],
        trf_block_count = 1 * MULT[],
        # trf_hidden_dim = 300 * MULT[], # does this need to be enc_input_width?
        trf_head_count = 2 * MULT[],
        trf_head_dim = 32 * MULT[],
        trf_ffn_dim = 1024 * MULT[],
        trf_dropout = 0.1,

        ## Execution related
        validation_ratio = 0.2,
        batch_len = 512,
        train_iters = 100,
        target_loss = 1e-4,
        learning_rate = 1,
        learning_rate_mult = 1.2,
    )
end

seq_len_train(cfg) = cfg.enc_input_width + cfg.dec_output_width # includes the target y at the end
input_len(cfg) = cfg.enc_input_width + cfg.dec_input_width - 1

function run(;cfg=makeCfg(), update=false, input=makeSeqTrigSum2Delta()) # makeSeqMarket(;update))
    global gcfg = nothing
    global dataTokens = nothing
    global xySplitBatched = nothing
    global all_layers = nothing
    global model = nothing
    global gstart = nothing
    global genned = nothing
    global gactual = nothing
    global BatchesCache = nothing

    enable_gpu(true) # CUDA.functional())
    Random.seed!(1)

    global gcfg = cfg
    println("Tokenizing data")
    global dataTokens = tokenizeData(input)
    # global dataSplit = splitTrainValidation(dataTokens, cfg.validation_ratio)
    # global xySplitBatched = makeBatchesSplit(cfg, dataSplit)
    println("Making batches")
    global xySplitBatched = makeBatches(cfg, dataTokens; update)
    println("Making model")
    global model = makeModel1(cfg)
    println("Training")
    train!(cfg, model, xySplitBatched)
    return showResult()
end

function showResult()
    cfg = gcfg
    starti = 1 # rand(1:(size(dataTokens,1) - input_len(cfg)))
    global gstart = input_subseq(dataTokens, starti:(starti + input_len(cfg) - 1))
    gen_count = input_len(cfg)
    println("Generating")
    global genned = generate(cfg, model, gstart, gen_count)
    global gactual = input_subseq(dataTokens, starti:min(time_input_size(dataTokens), time_input_size(genned)))
    println("Plotting")
    plotGenned(genned, gactual)
    return starti, genned
end

import SeqMarket
function makeSeqMarket(;update=false)
    return SeqMarket.make(;update).matrix
end

# This outputs a lenth x 2 matrix (sequence)
function makeSeqSinCos(length = 20 * 20 * 2 * π + 5)
    res = reduce(vcat, [Float32(sin(i/20.0)) Float32(cos(i/20.0))] for i in 1:length)
    return res
end

function makeSeqTrigSum1(length = 40 * 20 * 2 * π + 5)
    res = reduce(vcat, [Float32((sin(i/6.0) + cos(7.0 + i/11.0))/2)] for i in 1:length)
    return reshape(res, time_size(res), 1)
end

function makeSeqTrigSum2(length = 40 * 20 * 2 * π + 5)
    res = reduce(vcat, trigSum2(i) for i in 1:length)
    return reshape(res, time_size(res), 2)
end

trigSum2(i) = [Float32((sin(i/6.0) + cos(7.0 + i/11.0) + sin(π/3 + i/252))/3) Float32(sin(i/23.0))]

function makeSeqTrigSum2Delta(length = 40 * 20 * 2 * π + 5)
    res = reduce(vcat, 100 * (trigSum2(i) .- trigSum2(i-1)) for i in 1:length)
    return reshape(res, time_size(res), 2)
end

import HistData
function makeSeqRets()
    rets = HistData.makeRets(HistData.dataDaily(), 1)
    return reshape(rets.rets, time_size(rets.rets), 1)
end

function makeSeqSpy()
    rets = [row.close for row in HistData.dataDaily()]
    return reshape(rets, time_size(rets), 1)
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
function makeBatches(cfg, dataTokens; update=false)
    if !update && hasproperty(@__MODULE__, :BatchesCache)
        println("Using cached batches: $(batch_size(BatchesCache.train.x)) training batches, $(batch_size(BatchesCache.validation.x)) validation batches")
        return BatchesCache
    end
    ts_count = time_input_size(dataTokens)
    # @show ts_count
    subseq_count = ts_count - seq_len_train(cfg) - 1
    batch_count = subseq_count ÷ cfg.batch_len
    validation_batch_count = max(1, ceil(Int, cfg.validation_ratio * batch_count))
    training_batch_count = batch_count - validation_batch_count

    if validation_batch_count <= 0
        @show ts_count subseq_count batch_count validation_batch_count train_batch_count
        error("Insufficient data $(ts_count) for seq_size $(seq_len_train(cfg)) and batch len $(batch_len)")
    end

    ind = 1
    lfsr = MaxLFSR.LFSR(subseq_count; seed=ind)
    println("Creating $(training_batch_count) training batches")
    train_x, ind = batches(cfg, training_batch_count, lfsr, ind)
    println("Creating $(validation_batch_count) validation batches")
    validation_x, ind = batches(cfg, validation_batch_count, lfsr,ind)
    train_x = train_x |> DEV2[]
    validation_x = validation_x |> DEV2[]
    res = (; train=(;x=train_x), validation=(;x=validation_x))
    global BatchesCache = res
    return res
end

function batches(cfg, count, lfsr, ind)
    seqlen = seq_len_train(cfg)
    batchlen = cfg.batch_len
    # x = Array{Float32, ndims(dataTokens)+1}[]
    x = []
    for _ in 1:count
        batch = stack(begin
            seq = input_subseq(dataTokens, ind:(ind + seqlen - 1))
            ind, _ = iterate(lfsr, ind)
            attachPositional(seq, pos_dim(seq))
        end for _ in 1:batchlen)
        push!(x, ins_and_outs(cfg, batch))
        @assert size(batch) == (cfg.channel_width, seqlen, batchlen) "$(size(batch)) == $((cfg.channel_width, seqlen, batchlen))"
    end
    return x, ind
end

# len could be seqlen or inputlen
function attachPositional(seq, dim)
    len = time_input_size(seq)
    # range = 0.0:(π/(len-1)/2):π/2
    # TODO: this is dim order specific
    pos = Array{Float32}(undef, len, 2)
    for i in 1:len
        t = (π / 2) * ((i-1) / (len-1))
        pos[i,1] = cos(t)
        pos[i,2] = sin(t)
    end
    cat(seq, pos; dims=dim)
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
    seq_len = time_size(seq)
    @assert seq_len == enc_input_width + dec_output_width (@str seq_len == enc_input_width + dec_output_width seq_len enc_input_width dec_output_width)
    # dec_output = selectdim(seq, time_dim(seq), (seq_len - dec_output_width + 1):seq_len)
    dec_output = Array(selectdim(seq, time_dim(seq), (seq_len - dec_output_width + 1):seq_len))
    @assert size(dec_output, time_dim(seq)) == dec_output_width
    return (;enc_input, dec_input, dec_output)
end

function ins(cfg, seq)
    (;enc_input_width, dec_input_width) = cfg
    # enc_input = selectdim(seq, time_dim(seq), 1:enc_input_width)
    enc_input = Array(selectdim(seq, time_dim(seq), 1:enc_input_width))
    # the following might not go all the way to the end if this is being used for ins_and_outs which would include the y (actual)
    # dec_input = selectdim(seq, time_dim(seq), enc_input_width:(enc_input_width + dec_input_width - 1))
    dec_input = Array(selectdim(seq, time_dim(seq), enc_input_width:(enc_input_width + dec_input_width - 1)))
    @assert size(dec_input, time_dim(seq)) == dec_input_width
    return (;enc_input, dec_input)
end

function makeModel1(cfg)
    # The diagram on page 2 of this paper is most informative
    # https://arxiv.org/pdf/2001.08317.pdf

    # encoder_input_layer = Dense(cfg.enc_input_width * cfg.channel_width => cfg.trf_input_width * cfg.channel_width) |> DEV[]
    encoder_input_layer = Dense(cfg.channel_width => cfg.encode_width) |> DEV[]
    encoder_trf = Transformer(TransformerBlock, cfg.trf_block_count, cfg.trf_head_count, cfg.encode_width, cfg.trf_head_dim, cfg.trf_ffn_dim; dropout=cfg.trf_dropout) |> DEV[]

    # decoder_input_layer = Dense(cfg.dec_input_width => cfg.trf_input_width) |> DEV[]
    decoder_input_layer = Dense(cfg.channel_width => cfg.encode_width) |> DEV[]

    decoder_trf = Transformer(TransformerDecoderBlock, cfg.trf_block_count, cfg.trf_head_count, cfg.encode_width, cfg.trf_head_dim, cfg.trf_ffn_dim; dropout=cfg.trf_dropout) |> DEV[]

    # decoder_output_layer = Dense(cfg.trf_hidden_dim => cfg.dec_output_width) |> DEV[]
    decoder_output_layer = Dense(cfg.encode_width => cfg.channel_width) |> DEV[]

    # positional_encoding_layer
    all_layers = (;encoder_input_layer, encoder_trf, decoder_input_layer, decoder_trf, decoder_output_layer)

    function embedding(input)
        # NOTE: this is an experiment changing input to channels x time x batch
        # sz = size(input)
        # chan_len = (*)(sz[1:end-2]...)
        # input_flat = reshape(input, sz[1] * chan_len, sz[end])
        enced = encoder_input_layer(input)
        # enced = reshape(enced_flat, cfg.trf_input_width, sz[2:end-1]..., sz[end])
        return enced
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
        # println("Decoder input ", size(dec_input))
        enced = decoder_input_layer(dec_input)
        att_mask = CausalMask()
        cross_attention_mask = nothing
        t = decoder_trf(enced, enc_output, att_mask, cross_attention_mask) # return a NamedTuple (hidden_state = ..., ...)
        # p = embed_decode(t.hidden_state)
        p = decoder_output_layer(t.hidden_state)
        return p
    end

    function exec(enc_input, dec_input)
        enced = encoder_forward(enc_input)
        deced = decoder_forward(dec_input, enced)
        return deced
    end

    function loss(enc_input, dec_input, y)
        yhat = exec(enc_input, dec_input)
        # @show size(y) size(yhat)
        return Flux.Losses.mse(yhat, y)
    end

    function gen(seq; batched=false)
        # @show size(seq)
        if !batched
            seq_pos = reshape(seq_pos, size(seq_pos)..., 1)
        end
        posd = pos_dim(seq)
        seq_pos = attachPositional(seq, posd)
        enc_input, dec_input = ins(cfg, seq_pos)
        # @show size(enc_input)
        res = exec(enc_input, dec_input)
        # @show size(res)
        res2 = selectdim(res, posd, 1:(pos_size(res)-2)) # remove positional
        # @show size(res)
        return res2
    end

    params = Flux.params(all_layers)
    opt = Adam(cfg.learning_rate)

    return (;cfg, exec, gen, opt, params, loss, all_layers)
end

function train!(cfg, model, data)
    Flux.trainmode!(model)
    prevLoss = 1e10
    loss = 0
    batch_count = batch_size(data.train.x)
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
            model.opt.eta = min(loss / 100, model.opt.eta * cfg.learning_rate_mult)
            min_loss = loss
        elseif loss > min_loss * cfg.learning_rate_mult
            # model.opt.eta /= cfg.learning_rate_mult
            model.opt.eta = max(1e-7, model.opt.eta / cfg.learning_rate_mult)
            min_loss = loss
        end

        validation_loss = validationLoss()
        println("$(now()): Iteration $(i) training loss: ", loss, " validation loss: ", validation_loss, " learning rate: ", model.opt.eta)
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
    return loss / batch_size(batches)
end

function generate(cfg, model, start, forecast_count)
    Flux.testmode!(model)
    start = reshape(start, size(start)..., 1)
    @assert size(start) == (input_len(cfg), size(start)[2:end]...) string("expected ", (input_len(cfg), channel_size(start)), " but was ", size(start))
    res = gpu(start)
    # enc_input, _ = ins(cfg, res)
    for _ in 1:forecast_count
        len = time_size(res)
        input = selectdim(res, time_dim(res), (len - input_len(cfg) + 1):len)
        y = model.gen(input; batched=true)
        yend = selectdim(y, time_dim(y), time_size(y))
        # println("yhat gen size ", size(y), " for res ", size(res), " with yend ", size(yend))
        res = cat(res, yend; dims=time_dim(res))
    end
    res_cpu = cpu(res) # bring it back to cpu so we can use it
    return dropdims(res_cpu, dims=ndims(res_cpu)) # drop the batch dim
end
# function generate(cfg, model, start, forecast_count)
#     Flux.testmode!(model)
#     # T x C x B
#     # start = reshape(start, size(start)..., 1)
#     start = permutedims(reshape(start, size(start)..., 1), [2,1,3])
#     # @assert size(start) == (input_len(cfg), size(start)[2:end]...) string("expected ", (input_len(cfg), sizeChannel(start)), " but was ", size(start))
#     res = gpu(start)
#     # enc_input, _ = ins(cfg, res)
#     for _=1:forecast_count
#         # y = model.gen(res[(end - input_len(cfg) + 1):end,:,:]; batched=true)
#         y = model.gen(res[:,(end - input_len(cfg) + 1):end,:]; batched=true)
#         # yend = y[end:end,:,:]
#         yend = y[:,end:end,:]
#         # res = cat(res, yend; dims=1)
#         res = cat(res, yend; dims=2)
#     end
#     res_cpu = cpu(res)
#     return dropdims(res_cpu, dims=ndims(res_cpu))
# end

import GLMakie, DrawUtil
import ColorTypes:RGB,RGBA
function plotGenned(genned, actual)
    display(GLMakie.lines(selectdim(actual, 2, 1); color=RGBA(.5, .0, .0, .5)))
    # display(GLMakie.lines!(actual[:,2]; color=RGBA(.0, .5, .0, .5)))

    display(GLMakie.lines!(selectdim(genned, 2, 1); color=RGBA(.9, .0, .0, .8)))
    # display(GLMakie.lines!(genned[:,2]; color=RGBA(.0, .9, .0, .8)))

    GLMakie.vlines!(input_len(gcfg); color=:green)
end

function plotgen()
    # genned = generate(gcfg, model, gstart, count)
    plotGenned(genned, gactual)
end

# batch is always the last dim
batch_dim(x) = ndims(x)
batch_size(x) = size(x, batch_dim(x))
# time is always the first dim
time_dim(x) = 1
time_size(x) = size(x, time_dim(x))
time_input_size(x) = 1

channel_dims(x) = 2:(ndims(x) - 1)
channel_size(x) = size(x)[channel_dims(x)]
# channel_input_dim(x) = 1
# channel_input_size(x) = 2:ndims(x)
# lengthChannel(x) = (*)(size(x)[2:(end-1)]...)
pos_dim(x) = 2
pos_size(x) = channel_size(x)[1]
pos_input_size(x) = 2

input_subseq(seq, inds) = selectdim(seq, ndims(seq), inds)
# seq[starti:(starti + input_len(cfg) - 1),:]

end