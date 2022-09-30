module FarPrev
# import SliceMap:slicemap
import CudaUtil
import Flux
import Statistics:median,mean
import Forecast
using MLUtil
using FluxLayers

export far
const far = @__MODULE__

import SeqFin
config = SeqFin.config
makeSeq = SeqFin.make

const Device = Ref{Function}(CudaUtil.DEV)
use(sym=:gpu) = sym == :gpu ? (Device[] = DEV) : (Device[] = Flux.cpu)

function run(maxIter=0)::Nothing
    init()
    train(maxIter)
    test()
    disp(seq, config.castLen ÷ 2)
end
function init()::Nothing
    global config, seq, seqTrain, seqTest, batchers = makeSeq()
    global mod = makeModel(config)
    move(Device[])
    # checkSeq()
    return
end
train(maxIter=0)::Nothing = Forecast.train(config, mod, batchers.train; maxIter)
test()::Nothing = Forecast.test(mod, batchers.test)
import FileUtil
function save()::Nothing
    path = joinpath(Forecast.SaveDir, mod.name, "manual.bson")
    Forecast.save(path, mod.layers, mod.opt)
    return
end
function load()::Nothing
    path = FileUtil.mostRecentFile(joinpath(Forecast.SaveDir, mod.name))
    opt = Forecast.load!(path, mod.layers)
    global mod = merge(mod, (;opt))
    return
end

function move(dev)::Nothing
    global mod = to(mod, dev)
    return
end
function to(mod, dev)
    layers = dev(mod.layers)
    exec = makeExec(config, layers)
    loss = makeLoss(config, exec)
    return (;mod.name, layers, exec, loss, mod.opt, dev)
end

function makeModel(cfg)
    name = "Test1"
    stage1Width = 32
    combLen = 3
    stage1CombWidth = 128
    stage1OutWidth = 4096
    castWidth = 4
    castCombWidth = 8
    castOutWidth = 256
    outHidden = 8192
    outWidth = 1
    outSize = (outWidth, cfg.castLen, cfg.batchLen)
    inputBatchSize = (cfg.inputLen, cfg.batchLen)
    castMidWidth = castWidth * cfg.castLen + castCombWidth * (cfg.castLen - (combLen-1))

    # TODO: introduce normalization layer

    act0 = Flux.swish # identity
    act1 = Flux.swish # identity # Flux.sigmoid_fast
    act2 = identity # Flux.swish # identity # Flux.tanh_fast
    act3 = identity # Flux.swish # Flux.relu

    encer = cfg.encoder
    encerCast = cfg.encoderCast

    densePerInput = Flux.Dense(cfg.inputEncedWidth => stage1Width, act2)
    combInput = fl.SeqComb((cfg.inputEncedWidth, combLen) => stage1CombWidth, act1)
    denseStage1Out = Flux.Dense(stage1Width * cfg.inputLen + stage1CombWidth * (cfg.inputLen - (combLen-1)) => stage1OutWidth, act2)

    densePerCast = Flux.Dense(cfg.castEncedWidth => castWidth, act2)
    combCast = fl.SeqComb((cfg.castEncedWidth, combLen) => castCombWidth, act1)
    denseCastOut = Flux.Dense(castMidWidth => castOutWidth, act2)

    combine = Flux.Dense(stage1OutWidth + castOutWidth => outHidden, act2)
    last = Flux.Dense(outHidden => outWidth * cfg.castLen, act3)

    layers = (;encer, encerCast, densePerInput, combInput, denseStage1Out, densePerCast, combCast, denseCastOut, combine, last)
    params = Flux.params(layers...)
    println("num params: ", sum(length.(params)))

    exec = makeExec(cfg, layers)
    loss = makeLoss(cfg, exec)
    opt = Flux.Adam()
    return (;name, layers, exec, loss, opt, dev=Flux.cpu)
end

function makeExec(cfg, layers)
    (;encer, encerCast, densePerInput, combInput, denseStage1Out, densePerCast, combCast, denseCastOut, combine, last) = layers

    stage1Width = 32
    combLen = 3
    stage1CombWidth = 128
    stage1OutWidth = 4096
    castWidth = 4
    castCombWidth = 8
    castOutWidth = 256
    outHidden = 8192
    outWidth = 1
    outSize = (outWidth, cfg.castLen, cfg.batchLen)
    inputBatchSize = (cfg.inputLen, cfg.batchLen)
    castMidWidth = castWidth * cfg.castLen + castCombWidth * (cfg.castLen - (combLen-1))

    return function(x, cast)
        # proc x alone
        @assert size.(x, 1) == cfg.inputWidths string(size.(x, 1), ' ', cfg.inputWidths)
        x1 = encer(x)
        @assert size(x1) == (cfg.inputEncedWidth, inputBatchSize...) string(size(x1), ' ', (cfg.inputEncedWidth, inputBatchSize...))
        x2 = densePerInput(x1)
        @assert size(x2) == (stage1Width, inputBatchSize...) string(size(x2), ' ', (stage1Width, inputBatchSize...))
        x3 = combInput(x1)
        @assert size(x3) == (stage1CombWidth, cfg.inputLen - (combLen-1), cfg.batchLen) string(size(x3), ' ', (stage1Width, cfg.inputLen - (combLen-1), cfg.batchLen))
        x4 = cat(Flux.flatten(x2), Flux.flatten(x3); dims=1)
        stage1Out = denseStage1Out(x4)
        @assert size(stage1Out) == (stage1OutWidth, cfg.batchLen) string(size(stage1Out), ' ', (stage1OutWidth, cfg.batchLen))

        # proc cast alone
        @assert size.(cast, 1) == cfg.castWidths string(size.(cast, 1), ' ', cfg.castWidths)
        cast1 = encerCast(cast)
        @assert size(cast1) == (cfg.castEncedWidth, cfg.castLen, cfg.batchLen)
        cast2 = densePerCast(cast1)
        @assert size(cast2) == (castWidth, cfg.castLen, cfg.batchLen)
        cast3 = combCast(cast1)
        @assert size(cast3) == (castCombWidth, cfg.castLen - (combLen-1), cfg.batchLen) string(size(cast3), ' ', (castCombWidth, cfg.castLen - (combLen-1), cfg.batchLen))
        cast4 = cat(Flux.flatten(cast2), Flux.flatten(cast3); dims=1)
        @assert size(cast4) == (castMidWidth, cfg.batchLen)
        castOut = denseCastOut(cast4)
        @assert size(castOut) == (castOutWidth, cfg.batchLen) string(size(castOut), ' ', (castOutWidth, cfg.batchLen))

        # combine x and cast
        stage2In = cat(stage1Out, castOut; dims=1)
        @assert size(stage2In) == (stage1OutWidth + castOutWidth, cfg.batchLen)
        stage2Mid = combine(stage2In)
        @assert size(stage2Mid) == (outHidden, cfg.batchLen)
        stage2Out = last(stage2Mid)
        @assert size(stage2Out) == (outWidth * cfg.castLen, cfg.batchLen)
        out1 = reshape(stage2Out, outSize)
        # out .+= MU.N(1.0)
        # out .*= cast[1][1:1,:,:] # TODO: generalize, this masks it to only days when the market was open
        # out = out .+ MU.N(1.0)
        # out = out .* MU.N(100.0)
        out2 = out1 .* cast[1][1:1,:,:] # TODO: generalize, this masks it to only days when the market was open
        @assert size(out2) == outSize
        return out2
    end
end

# TODO: Consider passing in previous value so we can check if we matched above or below
function makeLoss(cfg, exec)
    outWidth = 1
    return function(batch)
        x, cast, y = batch
        @assert size(y[1]) == (1, cfg.castLen, cfg.batchLen) string(size(y[1]), ' ', (1, cfg.castLen, cfg.batchLen))
        yy = dropdims(y[1]; dims=1)
        # yy = yy .- MU.N(1.0)
        yhat = exec(x, cast)
        @assert size(yhat) == (outWidth, cfg.castLen, cfg.batchLen)
        # y is actual value for each castLen and batchLen
        # yhat is outWidth number of predicted guesses of y
        @assert eltype(y[1]) == eltype(yhat) string(eltype(y[1]), ' ', eltype(yhat))

        err = 0.0
        for yh in (yhat[i,:,:] for i in axes(yhat, 1))
            err += Flux.Losses.mse(yh, yy)
        end
        return err
    end
end

# function makeModelOld(cfg)
#     stage1WidthOut = 2048
#     castHidden = 1024
#     stage2Hidden = 2048
#     outWidth = 1
#     inputBatchSize = (cfg.inputLen, cfg.batchLen)
#     outSize = (outWidth, cfg.castLen, cfg.batchLen)

#     encer = cfg.encoder
#     encerCast = cfg.encoderCast

#     denseInput1 = Flux.Dense(cfg.inputEncedWidth * cfg.inputLen => stage1WidthOut)
#     denseCast1 = Flux.Dense(cfg.castEncedWidth * cfg.castLen => castHidden)
#     combine = Flux.Dense(stage1WidthOut + castHidden => stage2Hidden)
#     denseStage2 = Flux.Dense(stage2Hidden => outWidth * cfg.castLen)

#     exec = (x, cast) -> begin
#         # proc x alone
#         @assert size.(x, 1) == cfg.inputWidths string(size.(x, 1), ' ', cfg.inputWidths)
#         x1 = encer(x)
#         @assert size(x1) == (cfg.inputEncedWidth, inputBatchSize...) string(size(x1), ' ', (cfg.inputEncedWidth, inputBatchSize...))
#         x2 = Flux.flatten(x1)
#         @assert size(x2) == (cfg.inputEncedWidth * cfg.inputLen, cfg.batchLen)
#         stage1out = denseInput1(x2)
#         @assert size(stage1out) == (stage1WidthOut, cfg.batchLen)

#         # proc cast alone
#         @assert size.(cast, 1) == cfg.castWidths string(size.(cast, 1), ' ', cfg.castWidths)
#         cast1 = encerCast(cast)
#         @assert size(cast1) == (cfg.castEncedWidth, cfg.castLen, cfg.batchLen)
#         cast2 = Flux.flatten(cast1)
#         @assert size(cast2) == (cfg.castEncedWidth * cfg.castLen, cfg.batchLen)
#         cast3 = denseCast1(cast2)
#         @assert size(cast3) == (castHidden, cfg.batchLen)

#         # combine x and cast
#         stage2in = cat(stage1out, cast3; dims=1)
#         @assert size(stage2in) == (stage1WidthOut + castHidden, cfg.batchLen)
#         stage2mid = combine(stage2in)
#         @assert size(stage2mid) == (stage2Hidden, cfg.batchLen)
#         stage2out = denseStage2(stage2mid)
#         @assert size(stage2out) == (outWidth * cfg.castLen, cfg.batchLen)
#         out1 = reshape(stage2out, outSize)
#         out2 = out1 .* cast[1][1:1,:,:] # TODO: generalize
#         @assert size(out2) == outSize
#         return out2
#     end

#     # Consider passing in previous value so we can check if we matched above or below
#     function loss(batch)
#         x, cast, y = batch
#         # @assert size(y[1]) == (1, cfg.castLen, cfg.batchLen) string(size(y[1]), ' ', (1, cfg.castLen, cfg.batchLen))
#         yy = dropdims(y[1]; dims=1)
#         global yhat = exec(x, cast)
#         # @assert size(yhat) == (outWidth, cfg.castLen, cfg.batchLen)
#         # y is actual value for each castLen and batchLen
#         # yhat is outWidth number of predicted guesses of y
#         # @assert eltype(y[1]) == eltype(yhat) string(eltype(y[1]), ' ', eltype(yhat))
#         # err = sum(mapslices(yh -> Flux.Losses.mse(yh, yy), yhat; dims=[2,3]))
#         # err = sum(slicemap(yh -> Flux.Losses.mse(yh, yy), yhat; dims=[2,3]))

#         err = 0.0
#         for yh in (yhat[i,:,:] for i in axes(yhat, 1))
#             err += Flux.Losses.mse(yh, yy)
#         end

#         # yh = slicemap(x -> [mean(x)], yhat; dims=1)
#         # @assert size(yh) == size(yy) string(size(yh), ' ', size(yy))
#         # err = Flux.Losses.mse(yh, yy)

#         # err = 0.0
#         # a = axes(yy)
#         # for i in a[end-1], j in a[end]
#         # # for i in CartesianIndices(yy)
#         #     # med = median(yhat[:,i])
#         #     # err += (med - yy[i])^2
#         #     med = mean(yhat[:,i,j])
#         #     err += (med - yy[i,j])^2
#         # end
#         return err
#     end

#     layers = (;encer, encerCast, denseInput1, denseCast1, combine, denseStage2)
#     params = Flux.params(layers...)
#     println("num params: ", sum(length.(params)))
#     opt = Flux.Adam()
#     return (;layers, params, exec, loss, opt)
# end

import MLUtil
function stitch(sq=seq, castInd=1)
    batcher = MLUtil.makeBatcher(config, sq, false)
    toYhat = x->mapslices(median, x; dims=1)
    res = Forecast.stitchCasts(batcher, mod.exec, toYhat, castInd, Device[])
    return res
end

using DrawUtil
function disp(sq=seq, castInd=1, count=typemax(Int))::Nothing
    yh, y = stitch(sq, castInd)
    ys = filter(x -> abs(x) > 0.00001, first(dropdims(y; dims=1), count))
    yhs = filter(x -> abs(x) > 0.00001, first(dropdims(yh; dims=1), count))
    display(draw(ys; color=:white))
    draw!(yhs; color=:green)
    return
    # origY = Int.(fr.seq[1][4,:])

    # for b in batchers.test
    #     global yhat = mapslices(argmax, mod.exec(b.bufsX, b.bufsCast) |> Flux.cpu; dims=1)
    #     global y = mapslices(argmax, b.bufsY[1] |> Flux.cpu; dims=1)
    #     draw(y[]; color=:white)
    #     draw!(yhat; color=:green)
    #     break
    # end
end

run1() = ( b = first(batchers.test) |> mod.dev ; mod.exec(b.bufsX, b.bufsCast))

end