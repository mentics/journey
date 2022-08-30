module Forecast
import Flux #: Flux,Dense,ADAM,gradient,gpu
# import Flux.Optimise: update!
# import Transformers: Transformer,TransformerDecoder,todevice,enable_gpu

#==
Input params:
inputLen
inputWidth
castLen
binCnt
batchSize
==#

const N = Float64

#region TimeSeries
function train!(model, loss, cfg, seqm)
    batchIter = makeBatchIter(cfg, seqm)
    params = Flux.params(model)
    opt = Adam()
    for b in batchIter
        grad = gradient(() -> loss(b), params)
        update!(opt, params, grad)
        cb()
    end
end

function makeBufXY(cfg)
    bufX = Array{N}(undef, cfg.inputWidth, cfg.inputLen, cfg.batchSize)
    bufY = Array{N}(undef, length(cfg.outputInds), cfg.castLen, cfg.batchSize)
    return (bufX, bufY)
end

function toBatchXY!(bufX, bufY, outputInds, seqm, inputOffset)
    _, inputLen, batchSize = size(bufX)
    outputLen = size(bufY)[2]
    outputOffset = inputOffset + inputLen
    for b in 1:batchSize
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
    return (toBatchXY!(bufX, bufY, cfg.outputInds, seqm, i) for i in 1:cfg.batchCount)
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