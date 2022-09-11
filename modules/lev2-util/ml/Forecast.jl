module Forecast
import Flux:Flux,gpu #: Flux,Dense,ADAM,gradient,gpu
import CUDA:CuIterator
import Flux.Optimise
import Statistics:median
import MLUtil:MLUtil,N
# import Transformers: Transformer,TransformerDecoder,todevice,enable_gpu

Base.IteratorSize(::Type{<:CuIterator{B}}) where B = Base.IteratorSize(B)
Base.HasLength(::Type{<:CuIterator{B}}) where B = Base.HasLength(B)
Base.HasShape{N}(::Type{<:CuIterator{B}}) where {B,N} = Base.HasShape{N}(B)
Base.IteratorEltype(::Type{<:CuIterator{B}}) where B = Base.IteratorEltype(B)

Base.size(itr::T) where T<:CuIterator = Base.size(itr.batches)
Base.length(itr::T) where T<:CuIterator = Base.length(itr.batches)
Base.eltype(itr::T) where T<:CuIterator = Base.eltype(itr.batches)

#==
Input params:
inputLen
inputWidth
castLen
binCnt
batchLen
==#

#region TimeSeries
function train(cfg, model, params, opt, loss, seq; cb=nothing)
    batchIter = materialize(MLUtil.makeBatchIter(cfg, seq))
    cfg.useCpu || (batchIter = batchIter |> gpu)
    # println("batchIter: ", typeof(batchIter))
    tracker = trainProgress(() -> loss(first(batchIter)), cfg.lossTarget, 1.0; cb)
    # params = Flux.params(model)
    for i in 1:cfg.maxIter
        for b in batchIter
            Flux.reset!(model)
            global grad = Flux.gradient(() -> loss(b), params)
            # for p in params
            #     @assert p in grad.params
            # end
            Flux.Optimise.update!(opt, params, grad)
        end
        if tracker(i)
            break
        end
    end

    report("Train", model, loss, batchIter)
end

function trainProgress(loss, lossTarget, seconds; cb=nothing)
    return Flux.throttle(seconds; leading=true, trailing=true) do i
        los = loss()
        println("Progress $(i): loss = ", los)
        isnothing(cb) || cb()
        return los < lossTarget
    end
end

function test(cfg, model, loss, seq)
    # batchIter = makeBatchIter(cfg, seqTest)
    batchIter = MLUtil.makeBatchIter(cfg, seq)
    # cfg.useCpu || (batchIter = CuIterator(batchIter))
    report("Test", model, loss, batchIter)
end

function report(title, model, loss, batchIter)
    # global so = batchIter
    med = map(batchIter) do b
        Flux.reset!(model)
        return loss(b |> gpu)
    end |> median
    println("$(title) median loss: ", med)
end

# fs.model(fc.singleXY(fs.cfg, fs.seq, 1)[1])
# fs.loss(fc.singleXY(fs.cfg, fs.seq, 1))
function singleXY(cfg, seq, inputOffset)
    bufX = Array{N}(undef, cfg.inputWidth, cfg.inputLen, 1)
    bufY = Array{N}(undef, length(cfg.outputInds), cfg.castLen, 1)
    return toBatchXY!(bufX, bufY, cfg, seq, inputOffset)
    # outputOffset = inputOffset + cfg.inputLen
    # for i in 1:cfg.inputLen
    #     bufX[:,i] .= seq[:, inputOffset + i]
    # end
    # for i in 1:length(cfg.outputInds)
    #     bufY[:,i] .= seq[cfg.outputInds, outputOffset + i]
    # end
    # return (bufX, bufY)
end

# function makeBufXY(cfg)
#     bufX = Array{N}(undef, cfg.inputWidth, cfg.inputLen, cfg.batchLen)
#     bufY = Array{N}(undef, cfg.binCnt, cfg.castLen, cfg.batchLen)
#     return (bufX, bufY)
# end

# function toBatchXY!(bufX, bufY, cfg, seq, inputOffset)
#     _, inputLen, batchLen = size(bufX)
#     outputLen = size(bufY)[2]
#     outputOffset = inputOffset + inputLen
#     for b in 1:batchLen
#         for i in 1:inputLen
#             bufX[:,i,b] .= seq[:, inputOffset + b + i - 1]
#         end
#         for i in 1:outputLen
#             bufY[:,i,b] .= toh(cfg.binDef, seq[cfg.outputInds, outputOffset + b + i - 1])
#         end
#     end
#     return (bufX, bufY)
# end

# function makeBatchIter(cfg, seq)
#     bufX, bufY = makeBufXY(cfg)
#     batchCount = size(seq)[2] ÷ cfg.batchLen - 1
#     # @show  batchCount cfg.batchLen
#     return (toBatchXY!(bufX, bufY, cfg, seq, (i-1) * cfg.batchLen) for i in 1:batchCount)
# end
#endregion

function toh(def, val)
    b = map(x -> toBin(def, x), val)
    return Flux.onehotbatch(b, 1:def.num)
end

# function yoh(cfg, y)
#     def = BinDef(cfg.binCnt)
#     reshape(Flux.onehotbatch(map(x -> toBin(def, x), y), 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, size(y)[end]))
#     # reshape(Flux.onehotbatch(y, 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, size(y)[end]))
# end

function materialize(iter)
    res = typeof(first(iter))[]
    for batch in iter
        push!(res, deepcopy(batch)) # must copy because iter reuses buffers
    end
    return res
end

end