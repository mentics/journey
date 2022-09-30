module MLUtil

export MU, N

const MU = @__MODULE__
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
    len = size(seq[1])[end]
    split = round(Int, (1 - testHoldOut) * len)
    train = map(seq) do sub
        @view sub[:,1:split]
    end
    test = map(seq) do sub
        @view sub[:,split+1:len]
    end
    return (train, test)
end

function splitTrainTest(m, testHoldOut)
    len = size(m)[end]
    split = round(Int, (1 - testHoldOut) * len)
    return ((@view m[:,1:split]), (@view m[:,split+1:len]))
end

# function seqInds(cfg, seq)
#     len = size(seq[1])[end]
#     split = round(Int, (1 - cfg.testHoldOut) * len) - cfg.inputLen
#     return (1:split, split+1:len)
# end

# function splitTrainTest(seq, testHoldOut)
#     _, len = size(seq)
#     split = round(Int, (1 - testHoldOut) * len)
#     # batchCount = (1 - cfg.testHoldOut) * length(seq) / cfg.batchLen
#     # seqTest = @view seq[:, (cfg.batchLen * cfg.batchCount + cfg.inputLen + cfg.castLen):end]
#     return ((@view seq[:,1:split]), (@view seq[:,(split+1):len]))
# end

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

function makeBatch!(bufs, cfg, seq, batchNum)
    bufsX, bufsCast, bufsY = bufs
    # for (b, start) in enumerate(batchStarts(cfg, batchNum))
    for i in 1:cfg.batchLen
        # slix = slice(seq, start+1:start+cfg.inputLen)
        slix = sliceLastDim(seq, indsX(cfg, batchNum, i))
        for tupi in eachindex(bufsX)
            # ???: input length dim is always the last dim (before batch dim)
            selectdim(bufsX[tupi], ndims(bufsX[tupi]), i) .= slix[tupi]
        end
        # cstart = start+cfg.inputLen
        # slic = slice(seq, cstart:(cstart-1)+cfg.castLen)
        slic = sliceLastDim(seq, indsCast(cfg, batchNum, i))
        cast = cfg.toCast(slic)
        for tupi in eachindex(bufsCast)
            selectdim(bufsCast[tupi], ndims(bufsCast[tupi]), i) .= cast[tupi]
        end
        # println(size(cfg.toY(slic)[1]))
        # println(typeof(bufsY))
        # println(size(bufsY))
        y = cfg.toY(slic)
        for tupi in eachindex(bufsY)
            selectdim(bufsY[tupi], ndims(bufsY[tupi]), i) .= y[tupi]
        end
    end
    return (;bufsX, bufsCast, bufsY)
end

batchCount(cfg, seq) = length(indsBatch(cfg, seq))
indsBatch(cfg, seq) = (1:cfg.batchLen:(size(seq[1])[end]-cfg.inputLen-cfg.castLen))[1:end-1]
indsX(cfg, b, i) = (1:cfg.inputLen) .+ (cfg.batchLen*(b-1)+i-1)
indsCast(cfg, b, i) = (1:cfg.castLen) .+ (cfg.batchLen*(b-1)+i-1+cfg.inputLen)

# function toBatch!(bufs, cfg, seq, inputOffset)
#     bufsX, bufsCast, bufsY = bufs
#     for indBuf in eachindex(bufsX)
#         bufX = bufsX[indBuf]
#         ss = seq[indBuf]
#         for b in 1:cfg.batchLen
#             for i in 1:cfg.inputLen
#                 bufX[:,i,b] .= ss[:, inputOffset + b + i - 1]
#             end
#         end
#     end
#     outputLen = size(bufsY)[2] # cfg.castLen?
#     outputOffset = inputOffset + cfg.inputLen
#     for b in 1:cfg.batchLen
#         for i in 1:outputLen
#             ind = outputOffset + b + i - 2 + 1
#             bufsY[:,i,b] .= onehotbatch(seq[1][cfg.outputInds, ind], 1:cfg.binCnt)
#             if length(cfg.castInds) == 1
#                 bufsCast[:,i,b] .= seq[cfg.castInds[1]][:,ind]
#             else
#                 for castIndInd in eachindex(cfg.castInds)
#                     bufsCast[castIndInd][:,i,b] .= seq[cfg.castInds[castIndInd]][:,ind]
#                 end
#             end
#         end
#     end
#     return (;bufsX, bufsCast, bufsY)
# end

import Random
function makeBatcher(cfg, seq::Union{Tuple,NamedTuple}, shuffle=false)
    bufs = makeBufs(cfg, seq)
    return shuffle ? (makeBatch!(bufs, cfg, seq, b) for b in Random.shuffle(1:length(indsBatch(cfg, seq)))) :
                     (makeBatch!(bufs, cfg, seq, b) for b in 1:length(indsBatch(cfg, seq)))
end

# function makeBatchIterOld(cfg, seq::Union{Tuple,NamedTuple})
#     bufs = makeBufs(cfg, seq)
#     batchCount = size(seq[1])[2] ÷ cfg.batchLen - 1
#     # @show  batchCount cfg.batchLen
#     return (toBatch!(bufs, cfg, seq, (i-1) * cfg.batchLen) for i in 1:batchCount)
# end

# slice(seq::Tuple, inds) = Tuple(selectdim(c, ndims(c), inds) for c in seq)
sliceLastDim(seq::Tuple, inds) = Tuple(sliceLastDim(c, inds) for c in seq)
sliceLastDim(seq, inds) = selectdim(seq, ndims(seq), inds)

# # https://github.com/mcreel/NeuralNetsForIndirectInference.jl/blob/c501362d083eae1ff239353eb185d20dd79b3472/SV/Train.jl#L7
# function QuantileRegressionLoss(yhat, y, quantile)
#     err = y .- yhat
#     return sum(max.(quantile .* err, (quantile .- 1.0) .* err)) # not sure about the q .- 1.0, not sure how quantile not being scalar would make sense in the other operations
# end

function argmax0(x)
    mx, ind = findmax(x)
    # if abs(mx) < eps(N)
    return mx == 0 ? 0 : ind
end

function zargmax(x::AbstractArray; dims)
    return mapslices(argmax0, x; dims)
    # mx, ind = findmax(x; dims)
    # # if abs(mx) < eps(N)
    # return mx == 0 ? 0 : ind
end

import Flux
onehot0(x, labels) = Flux.OneHotVector{UInt32,length(labels)}(something(findfirst(isequal(x), labels), 0))
onehotbatch0(x, labels) = Flux.OneHotArray(UInt32[something(findfirst(isequal(i), labels), 0) for i in x], length(labels))

end