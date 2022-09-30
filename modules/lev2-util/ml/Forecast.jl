module Forecast
import Flux
import Flux.Optimise
import Statistics:median
import MLUtil:MLUtil,N

# Base.IteratorSize(::Type{<:CuIterator{B}}) where B = Base.IteratorSize(B)
# Base.HasLength(::Type{<:CuIterator{B}}) where B = Base.HasLength(B)
# Base.HasShape{N}(::Type{<:CuIterator{B}}) where {B,N} = Base.HasShape{N}(B)
# Base.IteratorEltype(::Type{<:CuIterator{B}}) where B = Base.IteratorEltype(B)

# Base.size(itr::T) where T<:CuIterator = Base.size(itr.batches)
# Base.length(itr::T) where T<:CuIterator = Base.length(itr.batches)
# Base.eltype(itr::T) where T<:CuIterator = Base.eltype(itr.batches)

# train(cfg, mod, batcher; maxIter=0) = train(cfg, mod.layers, mod.params, mod.loss, batcher, mod.opt; maxIter)
import StatsBase:sample,mean
function train(cfg, mod, batcher; maxIter=0, cb=nothing)::Nothing
    running = Ref(true)
    (;name, layers, loss, opt, dev) = mod
    params = Flux.params(layers)
    batches = MLUtil.materialize(batcher) |> dev
    println("Number of batches: ", length(batches))
    tracker = trainProgress(() -> mean(loss.(sample(batches, 20; replace=false))), cfg.lossTarget, 1.0; cb)
    saver = Flux.throttle(i -> running[] && save(mod, name, i), 120; leading=false, trailing=true)
    try
        for i in 1:(maxIter != 0 ? maxIter : cfg.maxIter)
            for b in batches
                # Flux.reset!(layers)
                sleep(.001)
                grad = Flux.gradient(() -> loss(b), params)
                # for p in params
                #     @assert p in grad.params
                # end
                sleep(.001)
                Flux.Optimise.update!(opt, params, grad)
            end
            saver(i)
            if tracker(i)
                break
            end
        end
    finally
        running[] = false
    end
    report(name * "-train", mod, batches)
end

function trainProgress(loss, lossTarget, seconds; cb=nothing)
    return Flux.throttle(seconds; leading=true, trailing=true) do i
        los = loss()
        println("Progress $(i): loss = ", los)
        isnothing(cb) || cb()
        return los < lossTarget
    end
end

function test(mod, batcher)::Nothing
    report(mod.name * "-test", mod, batcher)
end

function report(title, mod, batchIter)::Nothing
    med = map(batchIter) do b
        # Flux.reset!(layers)
        return mod.loss(b |> mod.dev)
    end |> median
    println("$(title) median loss: ", med)
    return
end

SaveDir = mkpath(joinpath("C:/data/tmp/", "ml", "save"))
savePath(name, args)::String = ( mkpath(joinpath(SaveDir, name)) ; joinpath(SaveDir, name, join(args, '-') * ".bson") )
import BSON
save(mod, name::AbstractString, args...)::Nothing = save(savePath(name, args), mod.layers, mod.opt)
function save(path::AbstractString, layers, opt)::Nothing
    print("Saving ", path, "... ")
    model = Flux.cpu(layers)
    BSON.@save path model opt
    println("done.")
end

# load!(intoLayers, name, args...) = load!(intoLayers, savePath(name, args))
import FluxLayers, CUDA, NNlib
function load!(path, intoModel)
    # BSON.@load path m o
    print("Loading ", path, "... ")
    d = BSON.load(path, @__MODULE__)
    Flux.loadmodel!(intoModel, d[:model])
    println("done.")
    return d[:opt]
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

import CollUtil
function stitchCasts(batcher, exec, toOut, castInd, dev)
    b1 = first(batcher) |> dev
    yh1 = toOut(exec(b1.bufsX, b1.bufsCast) |> Flux.cpu)
    # yh1 = exec(b1.bufsX, b1.bufsCast) |> Flux.cpu
    yh = Array{eltype(yh1)}(undef, size(yh1)[1:end-2]..., 0)
    y = similar(yh)
    seqDim = ndims(yh1)-1
    for cbatch in batcher
        batch = cbatch |> dev
        cyh = toOut(exec(batch.bufsX, batch.bufsCast) |> Flux.cpu)
        # cyh = exec(batch.bufsX, batch.bufsCast) |> Flux.cpu
        yh = hcat(yh, selectdim(cyh, seqDim, castInd))
        cy = toOut(batch.bufsY[1] |> Flux.cpu)
        y = hcat(y, selectdim(cy, seqDim, castInd))
    end
    return (yh, y)
end

end