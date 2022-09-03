module Forecast
import Flux:Flux,gpu #: Flux,Dense,ADAM,gradient,gpu
# import CUDA:CuIterator
import Flux.Optimise
# import Transformers: Transformer,TransformerDecoder,todevice,enable_gpu

#==
Input params:
inputLen
inputWidth
castLen
binCnt
batchLen
==#

const N = Float32

#region TimeSeries
function train!(model, opt, loss, cfg, seqm; cb=nothing)
    # batchIter = CuIterator(makeBatchIter(cfg, seqm))
    # batchIter = makeBatchIter(cfg, seqm)
    # batchCheck = first(batchIter)
    batchIter = materialize(makeBatchIter(cfg, seqm))
    cfg.useCpu || (batchIter = batchIter |> gpu)
    tracker = trainProgress(() -> loss(first(batchIter)), cfg.lossTarget, 1.0; cb)
    params = Flux.params(model)
    for i in 1:cfg.maxIter
        for b in batchIter
            Flux.reset!(model)
            grad = Flux.gradient(() -> loss(b), params)
            for p in params
                @assert p in grad.params
            end
            Flux.Optimise.update!(opt, params, grad)
        end
        if tracker(i)
            break
        end
    end

    for b in batchIter
        Flux.reset!(model)
        println("Train loss: ", loss(b))
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

function test(cfg, model, loss, seq)
    # batchIter = makeBatchIter(cfg, seqmTest)
    batchIter = makeBatchIter(cfg, seq)
    cfg.useCpu || (batchIter = CuIterator(batchIter))
    for batch in batchIter
        Flux.reset!(model)
        println("Test loss: ", loss(batch))
    end
end

function makeViews(seq, testHoldOut)
    _, len = size(seq)
    split = round(Int, (1 - testHoldOut) * len)
    # batchCount = (1 - cfg.testHoldOut) * length(seq) / cfg.batchLen
    # seqmTest = @view seq[:, (cfg.batchLen * cfg.batchCount + cfg.inputLen + cfg.castLen):end]
    return ((@view seq[:,1:split]), (@view seq[:,(split+1):len]))
end

# fs.model(fc.singleXY(fs.cfg, fs.seqm, 1)[1])
# fs.loss(fc.singleXY(fs.cfg, fs.seqm, 1))
function singleXY(cfg, seqm, inputOffset)
    bufX = Array{N}(undef, cfg.inputWidth, cfg.inputLen, 1)
    bufY = Array{N}(undef, length(cfg.outputInds), cfg.castLen, 1)
    return toBatchXY!(bufX, bufY, cfg, seqm, inputOffset)
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
    bufX = Array{N}(undef, cfg.inputWidth, cfg.inputLen, cfg.batchLen)
    bufY = Array{N}(undef, cfg.binCnt, cfg.castLen, cfg.batchLen)
    return (bufX, bufY)
end

function toBatchXY!(bufX, bufY, cfg, seqm, inputOffset)
    binDef = BinDef(cfg.binCnt)
    _, inputLen, batchLen = size(bufX)
    outputLen = size(bufY)[2]
    outputOffset = inputOffset + inputLen
    for b in 1:batchLen
        for i in 1:inputLen
            bufX[:,i,b] .= seqm[:, inputOffset + b + i - 1]
        end
        for i in 1:outputLen
            bufY[:,i,b] .= toh(binDef, seqm[cfg.outputInds, outputOffset + b + i - 1])
        end
    end
    return (bufX, bufY)
end

function makeBatchIter(cfg, seq)
    bufX, bufY = makeBufXY(cfg)
    batchCount = size(seq)[2] ÷ cfg.batchLen - 1
    # @show  batchCount cfg.batchLen
    return (toBatchXY!(bufX, bufY, cfg, seq, (i-1) * cfg.batchLen) for i in 1:batchCount)
end
#endregion

function BinDef(num)
    left = -0.15
    right = 0.15
    span = right - left
    binWidth = span / (num-2)
    return (; left, right, span, num, binWidth)
end

function toBin(def, val)::Int
    max(1, min(def.num, 1 + round(Int, (val - def.left) / def.binWidth, RoundUp)))
end

function toh(def, val)
    b = map(x -> toBin(def, x), val)
    return Flux.onehotbatch(b, 1:def.num)
end

# function yoh(cfg, y)
#     def = BinDef(cfg.binCnt)
#     reshape(Flux.onehotbatch(map(x -> toBin(def, x), y), 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, size(y)[end]))
#     # reshape(Flux.onehotbatch(y, 1:cfg.binCnt), (cfg.binCnt, cfg.castLen, size(y)[end]))
# end

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

function materialize(iter)
    res = typeof(first(iter))[]
    for batch in iter
        push!(res, deepcopy(batch)) # must copy because iter reuses buffers
    end
    return res
end

end