module Forecast
import Flux
import Flux.Optimise
import Statistics:median
import CudaUtil:DEV
import MLUtil:MLUtil,N

# Base.IteratorSize(::Type{<:CuIterator{B}}) where B = Base.IteratorSize(B)
# Base.HasLength(::Type{<:CuIterator{B}}) where B = Base.HasLength(B)
# Base.HasShape{N}(::Type{<:CuIterator{B}}) where {B,N} = Base.HasShape{N}(B)
# Base.IteratorEltype(::Type{<:CuIterator{B}}) where B = Base.IteratorEltype(B)

# Base.size(itr::T) where T<:CuIterator = Base.size(itr.batches)
# Base.length(itr::T) where T<:CuIterator = Base.length(itr.batches)
# Base.eltype(itr::T) where T<:CuIterator = Base.eltype(itr.batches)

function train(cfg, model, params, opt, loss, seq; cb=nothing)
    batches = MLUtil.materialize(MLUtil.makeBatchIter(cfg, seq)) |> DEV
    # println("batchIter: ", typeof(batchIter))
    tracker = trainProgress(() -> loss(first(batches)), cfg.lossTarget, 1.0; cb)
    # params = Flux.params(model)
    for i in 1:cfg.maxIter
        for b in batches
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

    report("Train", model, loss, batches)
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
        return loss(b |> DEV)
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

# function toh(def, val)
#     b = map(x -> toBin(def, x), val)
#     return Flux.onehotbatch(b, 1:def.num)
# end

# function yoh(cfg, y)
#     def = BinDef(cfg.binCnt)
#     reshape(Flux.onehotbatch(map(x -> toBin(def, x), y), 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, size(y)[end]))
#     # reshape(Flux.onehotbatch(y, 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, size(y)[end]))
# end

end