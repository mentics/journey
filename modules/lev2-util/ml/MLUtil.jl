module MLUtil

export BinDef

const N = Float32

unflatten(dims::NTuple{N,Int}) where N = x -> reshape(x, dims)

function BinDef(num::Integer, left=-.1, right=.1)
    # left = -0.1
    # right = 0.1
    span = right - left
    binWidth = span / (num-2)
    return (; left, right, span, num, binWidth)
end

function toBin(def, val)::Int
    max(1, min(def.num, 1 + round(Int, (val - def.left) / def.binWidth, RoundUp)))
end

# # This creates equal sized bins for all the vals
# function binLog(binCnt, vals)
#     bins = Vector{Float64}(undef, binCnt - 1)
#     sorted = sort!(log.(vals))
#     binSize = length(sorted) ÷ binCnt
#     bin = 1
#     for i in binSize:binSize:(length(sorted) - binSize)
#         bins[bin] = sorted[i]
#         bin += 1
#     end
#     return bins
# end

# function findBinLog(bins, val::Float64)
#     valLog = log(val)
#     for i in eachindex(bins)
#         if valLog < bins[i]
#             return i
#         end
#     end
#     return length(bins) + 1
# end

# subview(seq, split) = @view sub[:,1:split]

function splitTrainTest(seq::Tuple, testHoldOut)
    _, len = size(seq[1])
    split = round(Int, (1 - testHoldOut) * len)
    train = map(seq) do sub
        @view sub[:,1:split]
    end
    test = map(seq) do sub
        @view sub[:,(split+1):len]
    end
    return (train, test)
end

function splitTrainTest(seq, testHoldOut)
    _, len = size(seq)
    split = round(Int, (1 - testHoldOut) * len)
    # batchCount = (1 - cfg.testHoldOut) * length(seq) / cfg.batchLen
    # seqTest = @view seq[:, (cfg.batchLen * cfg.batchCount + cfg.inputLen + cfg.castLen):end]
    return ((@view seq[:,1:split]), (@view seq[:,(split+1):len]))
end

function makeBufs(cfg, seq::Tuple)
    bufsX = map(seq) do ss
        Array{eltype(ss)}(undef, size(ss)[1], cfg.inputLen, cfg.batchLen)
    end
    bufsCast = (
        Array{N}(undef, size(seq[2])[1], cfg.castLen, cfg.batchLen),
        Array{N}(undef, size(seq[3])[1], cfg.castLen, cfg.batchLen)
    )
    bufY = Array{N}(undef, cfg.binCnt, cfg.castLen, cfg.batchLen)
    return (;bufsX, bufsCast, bufY)
end

import Flux:onehotbatch
function toBatch!(bufs, cfg, seq, inputOffset)
    bufsX, bufsCast, bufY = bufs
    # TODO: create views instead?
    for indBuf in eachindex(bufsX)
        bufX = bufsX[indBuf]
        ss = seq[indBuf]
        for b in 1:cfg.batchLen
            for i in 1:cfg.inputLen
                bufX[:,i,b] .= ss[:, inputOffset + b + i - 1]
            end
        end
    end
    outputLen = size(bufY)[2] # cfg.castLen?
    outputOffset = inputOffset + cfg.inputLen
    for b in 1:cfg.batchLen
        for i in 1:outputLen
            ind = outputOffset + b + i - 2 + 1
            bufY[:,i,b] .= onehotbatch(seq[1][cfg.outputInds, ind], 1:cfg.binCnt)
            bufsCast[1][:,i,b] .= seq[2][:,ind]
            bufsCast[2][:,i,b] .= seq[3][:,ind]
        end
    end
    return (;bufsX, bufsCast, bufY)
end

function makeBatchIter(cfg, seq::Tuple)
    bufs = makeBufs(cfg, seq)
    batchCount = size(seq[1])[2] ÷ cfg.batchLen - 1
    # @show  batchCount cfg.batchLen
    return (toBatch!(bufs, cfg, seq, (i-1) * cfg.batchLen) for i in 1:batchCount)
end

end