module MLExplore
using Compat, Dates
import Random
using Zygote, Flux, Optimisers, Transformers, CUDA
using Transformers.Layers
using NeuralAttentionlib.Masks
using BaseTypes
using BaseUtil, CudaUtil

const DEV2 = Ref{Function}(CudaUtil.DEV)

Base.@kwdef struct Config
        # Data related
        channel_width::Int
        embed_width::Int

        ## Model related
        # General Lengths
        enc_input_width::Int
        enc_output_width::Int
        dec_input_width::Int
        dec_output_width::Int

        # Transformer Config
        trf_input_width::Int
        trf_block_count::Int
        trf_head_count::Int
        trf_head_dim::Int
        trf_ffn_dim::Int
        trf_dropout::Float32

        ## Execution related
        validation_ratio::Float32
        batch_len::Int
        train_iters::Int
        target_loss::Float32
        learning_rate::Float32
        learning_rate_mult::Float32
        learning_rate_max_div::Float32
end

struct MyModel2
    embed_layer::Dense
    encoder_input_layer::Dense
    encoder_trf::Transformer
    decoder_input_layer::Dense
    decoder_trf::Transformer
    decoder_output_layer::Dense
end

function (model::MyModel2)(enc_input, dec_input)
    enced = encoder_forward(model, enc_input)
    deced = decoder_forward(model, dec_input, enced)
    return deced
end

Flux.@functor MyModel2

@kwdef struct Context3
    cfg::Config
    model::MyModel2
    opt_state::Any # Flux.Optimise.Optimiser state
end

function makeCfg()
    return Config(
        # Data related
        channel_width = 23 + 2, # add 2 for the positional encoding
        embed_width = 256,

        ## Model related
        # General Lengths
        enc_input_width = 256 + 32, # number of timesteps in a single input to the encoder
        enc_output_width = 256 + 32,
        dec_input_width = 4,
        dec_output_width = 4,

        # Transformer Config
        trf_input_width = 256,
        trf_block_count = 1,
        trf_head_count = 8, # Reducing this too much might decrease loss at the risk of overfitting?
        trf_head_dim = 64,
        trf_ffn_dim = 2048,
        trf_dropout = 0.1,

        ## Execution related
        validation_ratio = 0.2,
        batch_len = 128, # should develop rule of thumb for batch_len, maybe start with same as number of timesteps?
        train_iters = 2,
        target_loss = 1e-4,
        learning_rate = 1e-3,
        learning_rate_mult = 1.0, # 1.2,
        learning_rate_max_div = 1, # 20,
    )
end

seq_len_train(cfg) = cfg.enc_input_width + cfg.dec_output_width # includes the target y at the end
input_len(cfg) = cfg.enc_input_width + cfg.dec_input_width - 1

function run(;cfg=makeCfg(), updateSeq=false, updateBatches=false, input=makeSeqMarket(;update=updateSeq))
    global gcfg = nothing
    global dataTokens = nothing
    global xySplitBatched = nothing
    global model = nothing
    global gstart = nothing
    global genned = nothing
    global gactual = nothing
    global BatchesCache = nothing
    GC.gc(true)

    # return
    enable_gpu(true)
    Random.seed!(1)

    global gcfg = cfg
    println("Tokenizing data")
    global dataTokens = tokenizeData(input)
    println("Making batches")
    global xySplitBatched = makeBatches(cfg, dataTokens; update=updateBatches)
    println("Making model")
    global ctx = setupModel(cfg)
    println("Training")
    train!(ctx, xySplitBatched)
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

# function makeSeqTrigSum2(length = 40 * 20 * 2 * π + 5)
#     res = reduce(vcat, trigSum2(i) for i in 1:length)
#     return reshape(res, sizeTime(res), 2)
# end

# trigSum2(i) = [Float32((sin(i/6.0) + cos(7.0 + i/11.0) + sin(π/3 + i/252))/3) Float32(sin(i/23.0))]
trigSum2(i) = [Float32((sin(i/6.0) + cos(7.0 + i/11.0) + sin(π/3 + i/252))/3), Float32(sin(i/23.0))]

function makeSeqTrigSum2Delta(length = 40 * 20 * 2 * π + 5)
    res = reduce(hcat, 100 * (trigSum2(i) .- trigSum2(i-1)) for i in 1:length)
    # return reshape(res, 2, sizeTime(res))
end

# This doesn't have to return the same size as the input
function tokenizeData(m)
    # TODO
    return m
end

# TODO: this is generic, can be moved to util
import MaxLFSR
function makeBatches(cfg, dataTokens; update=false)
    if !update && isdefined(@__MODULE__, :BatchesCache) && !isnothing(BatchesCache)
        println("Using cached batches: $(batch_size(BatchesCache.train.x)) training batches, $(batch_size(BatchesCache.validation.x)) validation batches")
        return BatchesCache
    end
    empty!(Inds[])
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
    train_x = train_x # |> DEV2[]
    validation_x = validation_x # |> DEV2[]
    res = (; train=(;x=train_x), validation=(;x=validation_x))
    global BatchesCache = res
    return res
end

const Inds = Ref(Int[])

function batches(cfg, count, lfsr, ind)
    seqlen = seq_len_train(cfg)
    batchlen = cfg.batch_len
    # x = Array{Float32, ndims(dataTokens)+1}[]
    x = []
    for _ in 1:count
        batch = stack(begin
            # println("Adding to batch for index $ind")
            push!(Inds[], ind)
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
    # TODO: this is ugly
    if ndims(seq) < 3
        len = time_input_size(seq)
    else
        len = time_size(seq)
    end
    # range = 0.0:(π/(len-1)/2):π/2
    # TODO: this is dim order specific
    pos = Array{Float32}(undef, 2, len)
    for i in 1:len
        t = (π / 2) * ((i-1) / (len-1))
        pos[1,i] = cos(t)
        pos[2,i] = sin(t)
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

using JLD2
const BasePath = "C:/data/db/stage/"
lastStateFile() = joinpath(mkpath(joinpath(BasePath, "checkpoints")), "latest.jdl2")

function setupModel(cfg)
    model, opt = makeModel(cfg)
    model = model |> DEV2[]
    opt = opt |> DEV2[]

    count = sum(length, Flux.params(model))
    println("Created model with $(count) parameters")

    return Context3(cfg, model, opt)
end

function makeModel(cfg)
    # The diagram on page 2 of this paper is most informative
    # https://arxiv.org/pdf/2001.08317.pdf

    embed_layer = Dense(cfg.channel_width => cfg.embed_width, tanh; bias=false)
    encoder_input_layer = Dense(cfg.embed_width => cfg.trf_input_width, relu; bias=false)
    encoder_trf = Transformer(TransformerBlock, cfg.trf_block_count, cfg.trf_head_count, cfg.trf_input_width, cfg.trf_head_dim, cfg.trf_ffn_dim; dropout=cfg.trf_dropout)
    decoder_input_layer = Dense(cfg.embed_width => cfg.trf_input_width, relu; bias=false)
    decoder_trf = Transformer(TransformerDecoderBlock, cfg.trf_block_count, cfg.trf_head_count, cfg.trf_input_width, cfg.trf_head_dim, cfg.trf_ffn_dim; dropout=cfg.trf_dropout)
    decoder_output_layer = Dense(cfg.trf_input_width => cfg.channel_width, tanh; bias=false)
    model = MyModel2(embed_layer, encoder_input_layer, encoder_trf, decoder_input_layer, decoder_trf, decoder_output_layer)

    return @coal loadModel(model) (model, Flux.setup(Optimisers.AdamW(cfg.learning_rate), model))
end

function saveModel(model, opt_state)
    state = Flux.state(model |> cpu)
    jldsave(lastStateFile(); state, opt_state = opt_state |> cpu)
end

function loadModel(model, file = lastStateFile())
    if isfile(file)
        println("loading model state latest checkpoint")
        state, opt_state = JLD2.load(file, "state", "opt_state")
        Flux.loadmodel!(model, state)
        return model |> DEV2[], opt_state |> DEV2[]
    else
        println("no latest checkpoint found")
        return
    end
end

function encoder_forward(model, input)
    println("ef: ", typeof(model), ' ', typeof(input))
    in1 = model.embed_layer(input)
    enced = model.encoder_input_layer(in1)
    input = enced # permutedims(enced, [2,1,3])
    att_mask = CausalMask()
    # @show size(input) size(e)
    t = model.encoder_trf(input, att_mask) # return a NamedTuples (hidden_state = ..., ...)
    nt_output = t # permutedims(t.hidden_state, [2,1,3])
    # println("type of enc output ", typeof(nt_output)) # , " size: ", size(output))
    return nt_output
end

function decoder_forward(model, dec_input, nt_enc_output)
    println("decoder forward: ", typeof(model), ' ', typeof(dec_input), ' ', typeof(nt_enc_output))
    in1 = model.embed_layer(dec_input)
    enced = model.decoder_input_layer(in1)
    input = enced # permutedims(enced, [2,1,3])
    # @show size(dec_input) size(nt_enc_output.hidden_state) size(enced) size(input)
    # t = decoder_trf(input, nt_enc_output.hidden_state, CausalMask(), nt_enc_output.attention_mask) # return a NamedTuple (hidden_state = ..., ...)
    t = model.decoder_trf(input, nt_enc_output.hidden_state, nothing, nothing) # return a NamedTuple (hidden_state = ..., ...)
    p = model.decoder_output_layer(t.hidden_state)
    output = p # permutedims(p, [2,1,3])
    return output
end

function model_loss(model, enc_input, dec_input, y)
    yhat = model(enc_input, dec_input)
    return Flux.Losses.mse(yhat, y)
end

function gen(model,seq; batched=false)
    if !batched
        seq = reshape(seq, size(seq)..., 1)
    end
    posd = pos_dim(seq)
    seq_pos = attachPositional(seq, posd)
    enc_input, dec_input = ins(cfg, seq_pos)
    res = model(enc_input, dec_input)
    res2 = selectdim(res, posd, 1:(pos_size(res)-2)) # remove positional
    return res2
end

# function train!(cfg, model, data)
#     Flux.trainmode!(model)
#     prevLoss = 1e10
#     loss = 0
#     batch_count = batch_size(data.train.x)
#     min_loss = Inf
#     for i in 1:cfg.train_iters
#         ls = 0
#         # @time "        training of $batch_count batches" begin
#         for batch in 1:batch_count
#             println("    - Running batch $batch")
#             enc_input, dec_input, y = map(DEV2[], data.train.x[batch])
#             grad = gradient(() -> (ls += model.loss(enc_input, dec_input, y) ; return ls), model.params)
#             sleep(0.01)
#             Flux.update!(model.opt, model.params, grad)
#             # ls += model.loss(enc_input, dec_input, y)
#             sleep(0.01)
#         end
#         # end
#         loss = ls / batch_count

#         if loss < min_loss
#             model.opt.eta = min(loss / cfg.learning_rate_max_div, model.opt.eta * cfg.learning_rate_mult)
#             min_loss = loss
#         elseif loss > min_loss * cfg.learning_rate_mult
#             # model.opt.eta /= cfg.learning_rate_mult
#             model.opt.eta = max(1e-7, model.opt.eta / cfg.learning_rate_mult)
#             min_loss = loss
#         end

#         validation_loss = validationLoss()
#         println("$(now()): Iteration $(i) training loss: ", loss, " validation loss: ", validation_loss, " learning rate: ", model.opt.eta)
#         if loss < cfg.target_loss
#             @goto done
#         end
#         prevLoss = loss
#     end
#     @label done
#     println("Final loss: ", loss)
#     Flux.testmode!(model)
# end

function train!(ctx, data)
    (;cfg, model::MyModel2, opt_state) = ctx
    Flux.trainmode!(model)
    prevLoss = 1e10
    loss = 0
    batch_count = batch_size(data.train.x)
    min_loss = Inf

    # Create GPU arrays in advance and reuse them
    ex = data.train.x[1]
    enc_input = CuArray(ex.enc_input)
    dec_input = CuArray(ex.dec_input)
    y = CuArray(ex.dec_output)

    try
        for i in 1:cfg.train_iters
            ls = 0
            # @time "        training of $batch_count batches" begin
            for batch in 1:batch_count
                println("    - Running batch $batch")
                b = data.train.x[batch]
                copyto!(enc_input, b.enc_input)
                copyto!(dec_input, b.dec_input)
                copyto!(y, b.dec_output)
                loss_add, grad = Zygote.withgradient((m) -> model_loss(m, enc_input, dec_input, y), model)
                ls += loss_add
                opt_state, model = Optimisers.update!(opt_state, model, grad)
                sleep(0.01)
            end
            # end
            loss = ls / batch_count

            # if loss < min_loss
            #     model.opt.eta = min(loss / cfg.learning_rate_max_div, model.opt.eta * cfg.learning_rate_mult)
            #     min_loss = loss
            # elseif loss > min_loss * cfg.learning_rate_mult
            #     # model.opt.eta /= cfg.learning_rate_mult
            #     model.opt.eta = max(1e-7, model.opt.eta / cfg.learning_rate_mult)
            #     min_loss = loss
            # end

            validation_loss = validationLoss()
            println("$(now()): Iteration $(i) training loss: ", loss, " validation loss: ", validation_loss, " learning rate: ", model.opt.eta)
            if loss < cfg.target_loss
                @goto done
            end
            prevLoss = loss
        end
        @label done
    finally
        saveModel(model, opt_state)
    end
    println("Final loss: ", loss)
    Flux.testmode!(model)
end

function validationLoss()
    Flux.testmode!(model)
    batches = xySplitBatched.validation.x
    loss = 0
    # @time "        validation loss across $(length(batches)) batches" begin
    for x in batches
        # enc_input, dec_input, y = ins_and_outs(gcfg, x)
        enc_input, dec_input, y = map(DEV2[], x)
        loss += model.loss(enc_input, dec_input, y)
        yield()
    end
    # end
    Flux.trainmode!(model)
    return loss / batch_size(batches)
end


function modelgen(cfg, model, seq; batched=false)
    if !batched
        seq = reshape(seq, size(seq)..., 1)
    end
    posd = pos_dim(seq)
    seq_pos = attachPositional(seq, posd)
    enc_input, dec_input = ins(cfg, seq_pos)
    res = model.exec(gpu(enc_input), gpu(dec_input))
    res2 = selectdim(res, posd, 1:(pos_size(res)-2)) # remove positional
    return res2
end

function generate(cfg, model, start, forecast_count)
    # TODO: Can we "adjust" the values in the non-primary input data that is forecasted?
    Flux.testmode!(model)
    start = reshape(start, size(start)..., 1)
    # @assert size(start) == (input_len(cfg), size(start)[2:end]...) string("expected ", (input_len(cfg), channel_size(start)), " but was ", size(start))
    res = gpu(start)
    # enc_input, _ = ins(cfg, res)
    for _=1:forecast_count
        len = time_size(res)
        input = selectdim(res, time_dim(res), (len - input_len(cfg) + 1):len)
        y = modelgen(cfg, model, input; batched=true)
        yend = selectdim(y, time_dim(y), time_size(y))
        # println("yhat gen size ", size(y), " for res ", size(res), " with yend ", size(yend))
        res = cat(res, yend; dims=time_dim(res))
        sleep(0.01)
    end
    res_cpu = cpu(res) # bring it back to cpu so we can use it
    return dropdims(res_cpu, dims=ndims(res_cpu)) # drop the batch dim
end

import GLMakie, DrawUtil
import ColorTypes:RGB,RGBA
function plotGenned(genned, actual)
    # @show size(genned) size(actual)
    display(GLMakie.lines(selectdim(actual, channel_input_dims(actual)[1], 1); color=RGBA(.5, .0, 1.0, .5)))
    # display(GLMakie.lines!(actual[:,2]; color=RGBA(.0, .5, .0, .5)))

    display(GLMakie.lines!(selectdim(genned, channel_input_dims(genned)[1], 1); color=RGBA(.9, .0, .0, .8)))
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
time_dim(x) = ndims(x) - 1
time_size(x) = size(x, time_dim(x))
time_input_size(x) = size(x, ndims(x))

channel_dims(x) = 1:(ndims(x) - 2)
channel_size(x) = size(x)[channel_dims(x)]
channel_input_dims(x) = 1:(ndims(x) - 1)
# lengthChannel(x) = (*)(size(x)[2:(end-1)]...)
pos_dim(x) = 1
pos_size(x) = channel_size(x)[1]
pos_input_size(x) = 1

input_subseq(seq, inds) = selectdim(seq, ndims(seq), inds)

end