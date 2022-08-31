module Forecast
import Flux #: Flux,Dense,ADAM,gradient,gpu
# using CUDA
# import Flux.Optimise: update!
# import Transformers: Transformer,TransformerDecoder,todevice,enable_gpu

#==
Input params:
inputLen
inputWidth
castLen
binCnt
batchLen
==#

const N1 = Float32

#region TimeSeries
function train!(model, loss, cfg, seqm; cb=nothing)
    # batchIter = CuIterator(makeBatchIter(cfg, seqm))
    batchIter = makeBatchIter(cfg, seqm)
    # batchCheck = first(batchIter)
    tracker = trainProgress(() -> loss(first(batchIter)), cfg.lossTarget, 1.0; cb)
    params = Flux.params(model)
    opt = Flux.Adam()
    for i in 1:1000
        for b in batchIter
            Flux.reset!(model)
            grad = Flux.gradient(() -> loss(b), params)
            Flux.update!(opt, params, grad)
        end
        if tracker(i)
            break
        end
    end
end

function trainProgress(loss, lossTarget, seconds; cb=nothing)
    return Flux.throttle(seconds; leading=true, trailing=true) do i
        los = loss()
        println("Progress $(i): loss = ", los)
        isnothing(cb) || cb()
        return los < lossTarget
    end
end

function test(cfg, loss, seqm)
    seqmTest = @view seqm[:, (cfg.batchLen + cfg.inputLen + cfg.castLen):end]
    batchIter = makeBatchIter(cfg, seqmTest)
    # batchTest = first(batchIter)
    for batch in batchIter
        println("Test loss: ", loss(batch))
    end
end

# fs.model(fc.singleXY(fs.cfg, fs.seqm, 1)[1])
# fs.loss(fc.singleXY(fs.cfg, fs.seqm, 1))
function singleXY(cfg, seqm, inputOffset)
    bufX = Array{N1}(undef, cfg.inputWidth, cfg.inputLen, 1)
    bufY = Array{N1}(undef, length(cfg.outputInds), cfg.castLen, 1)
    return toBatchXY!(bufX, bufY, cfg.outputInds, seqm, inputOffset)
    # outputOffset = inputOffset + cfg.inputLen
    # for i in 1:cfg.inputLen
    #     bufX[:,i] .= seqm[:, inputOffset + i]
    # end
    # for i in 1:length(cfg.outputInds)
    #     bufY[:,i] .= seqm[cfg.outputInds, outputOffset + i]
    # end
    # return (bufX, bufY)
end

function makeBufXY(cfg)
    bufX = Array{N1}(undef, cfg.inputWidth, cfg.inputLen, cfg.batchLen)
    bufY = Array{N1}(undef, length(cfg.outputInds), cfg.castLen, cfg.batchLen)
    return (bufX, bufY)
end

function toBatchXY!(bufX, bufY, outputInds, seqm, inputOffset)
    _, inputLen, batchLen = size(bufX)
    outputLen = size(bufY)[2]
    outputOffset = inputOffset + inputLen
    for b in 1:batchLen
        for i in 1:inputLen
            bufX[:,i,b] .= seqm[:,inputOffset + b + i]
        end
        for i in 1:outputLen
            bufY[:,i,b] .= seqm[outputInds, outputOffset + b + i]
        end
    end
    return (bufX, bufY)
end

function makeBatchIter(cfg, seqm)
    bufX, bufY = makeBufXY(cfg)
    return (toBatchXY!(bufX, bufY, cfg.outputInds, seqm, i * cfg.batchLen) for i in 1:cfg.batchCount)
end
#endregion

# TODO: move where?
function binLog(binCnt, vals)
    bins = Vector{Float64}(undef, binCnt - 1)
    sorted = sort!(log.(vals))
    binSize = length(sorted) ÷ binCnt
    bin = 1
    for i in binSize:binSize:(length(sorted) - binSize)
        bins[bin] = sorted[i]
        bin += 1
    end
    return bins
end

function findBin(bins, val::Float64)
    valLog = log(val)
    for i in eachindex(bins)
        if valLog < bins[i]
            return i
        end
    end
    return length(bins) + 1
end

end