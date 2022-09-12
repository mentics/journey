module MLUtil

export BinDef

const N = Float32

function materialize(iter)
    res = typeof(first(iter))[]
    for batch in iter
        push!(res, deepcopy(batch)) # must copy because iter reuses buffers
    end
    return res
end

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

function makeBufs(cfg, seq::Union{Tuple,NamedTuple})
    sample = Tuple(selectdim(x, length(size(x)), 1) for x in seq)
    # sizes = size.(sample)
    bufsX = Tuple(Array{eltype(sc)}(undef, size(sc)..., cfg.inputLen, cfg.batchLen) for sc in sample)
    cast = cfg.toCast(sample)
    # if cast isa Tuple
        bufsCast = Tuple(Array{eltype(cc)}(undef, size(cc)..., cfg.castLen, cfg.batchLen) for cc in cast)
    # else
    #     bufsCast = Array{eltype(cast)}(undef, size(cast)..., cfg.castLen, cfg.batchLen)
    # end
    y = cfg.toY(sample)
    bufsY = Tuple(Array{eltype(yc)}(undef, size(yc)..., cfg.castLen, cfg.batchLen) for yc in y)
    # if length(y) > 1
    #     bufsY = Array{N}(undef, length(y), cfg.binCnt, cfg.castLen, cfg.batchLen)
    # else
    #     bufsY = Array{N}(undef, cfg.binCnt, cfg.castLen, cfg.batchLen)
    # end
    return (;bufsX, bufsCast, bufsY)
end

import Flux:onehotbatch
function batchAt!(bufs, cfg, seq, seqOffset)
    bufsX, bufsCast, bufsY = bufs
    for b in 1:cfg.batchLen
        start = seqOffset + b
        slix = slice(seq, start+1:start+cfg.inputLen)
        for i in eachindex(bufsX)
            selectdim(bufsX[i], length(size(bufsX[i])), b) .= slix[i]
        end
        cstart = start+cfg.inputLen
        slic = slice(seq, cstart:(cstart-1)+cfg.castLen)
        cast = cfg.toCast(slic)
        for i in eachindex(bufsCast)
            selectdim(bufsCast[i], length(size(bufsCast[i])), b) .= cast[i]
        end
        # println(size(cfg.toY(slic)[1]))
        # println(typeof(bufsY))
        # println(size(bufsY))
        y = cfg.toY(slic)
        for i in eachindex(bufsY)
            selectdim(bufsY[i], length(size(bufsY[i])), b) .= y[i]
        end
    end
    return (;bufsX, bufsCast, bufsY)
end

function toBatch!(bufs, cfg, seq, inputOffset)
    bufsX, bufsCast, bufsY = bufs
    for indBuf in eachindex(bufsX)
        bufX = bufsX[indBuf]
        ss = seq[indBuf]
        for b in 1:cfg.batchLen
            for i in 1:cfg.inputLen
                bufX[:,i,b] .= ss[:, inputOffset + b + i - 1]
            end
        end
    end
    outputLen = size(bufsY)[2] # cfg.castLen?
    outputOffset = inputOffset + cfg.inputLen
    for b in 1:cfg.batchLen
        for i in 1:outputLen
            ind = outputOffset + b + i - 2 + 1
            bufsY[:,i,b] .= onehotbatch(seq[1][cfg.outputInds, ind], 1:cfg.binCnt)
            if length(cfg.castInds) == 1
                bufsCast[:,i,b] .= seq[cfg.castInds[1]][:,ind]
            else
                for castIndInd in eachindex(cfg.castInds)
                    bufsCast[castIndInd][:,i,b] .= seq[cfg.castInds[castIndInd]][:,ind]
                end
            end
        end
    end
    return (;bufsX, bufsCast, bufsY)
end

slice(seq::Tuple, inds) = Tuple(selectdim(c, ndims(c), inds) for c in seq)
slice(seq, inds) = selectdim(seq, ndims(seq), inds)

function makeBatchIter(cfg, seq::Union{Tuple,NamedTuple})
    bufs = makeBufs(cfg, seq)
    batchCount = size(seq[1])[2] ÷ cfg.batchLen - 1
    # @show  batchCount cfg.batchLen
    return (batchAt!(bufs, cfg, seq, (i-1) * cfg.batchLen) for i in 1:batchCount)
end

function makeBatchIterOld(cfg, seq::Union{Tuple,NamedTuple})
    bufs = makeBufs(cfg, seq)
    batchCount = size(seq[1])[2] ÷ cfg.batchLen - 1
    # @show  batchCount cfg.batchLen
    return (toBatch!(bufs, cfg, seq, (i-1) * cfg.batchLen) for i in 1:batchCount)
end

end