module Far
import SliceMap:slicemap
import CudaUtil:DEV
import Flux
import Forecast

import SeqFin
config = SeqFin.config
makeSeq = SeqFin.make

function run()
    init()
    train()
    test()
end
function init()
    global config, seq, origY, batchers = makeSeq()
    global mod = makeModel(config)
    # checkSeq()
    return
end
train() = Forecast.train(config, mod, batchers.train)
test() = Forecast.test(config, mod, batchers.test)

function makeModel(cfg)
    stage1WidthOut = 512
    outWidth = 100
    inputBatchSize = (cfg.inputLen, cfg.batchLen)
    outSize = (outWidth, cfg.castLen, cfg.batchLen)

    encer = cfg.encoder |> DEV
    encerCast = cfg.encoderCast |> DEV

    dense1 = Flux.Dense(cfg.inputEncedWidth * cfg.inputLen => stage1WidthOut) |> DEV
    combine = Flux.Dense(stage1WidthOut + cfg.castEncedWidth * cfg.castLen => outWidth * cfg.castLen) |> DEV

    exec = (x, cast) -> begin
        # proc x alone
        @assert size.(x, 1) == cfg.inputWidths string(size.(x, 1), ' ', cfg.inputWidths)
        x1 = encer(x)
        @assert size(x1) == (cfg.inputEncedWidth, inputBatchSize...) string(size(x1), ' ', (cfg.inputEncedWidth, inputBatchSize...))
        x2 = Flux.flatten(x1)
        @assert size(x2) == (cfg.inputEncedWidth * cfg.inputLen, cfg.batchLen)
        stage1out = dense1(x2)
        @assert size(stage1out) == (stage1WidthOut, cfg.batchLen)

        # proc cast alone
        @assert size.(cast, 1) == cfg.castWidths string(size.(cast, 1), ' ', cfg.castWidths)
        cast1 = encerCast(cast)
        @assert size(cast1) == (cfg.castEncedWidth, cfg.castLen, cfg.batchLen)
        cast2 = Flux.flatten(cast1)
        @assert size(cast2) == (cfg.castEncedWidth * cfg.castLen, cfg.batchLen)

        # combine x and cast
        stage2in = cat(stage1out, cast2; dims=1)
        @assert size(stage2in) == (stage1WidthOut + cfg.castEncedWidth * cfg.castLen, cfg.batchLen)
        stage2out = combine(stage2in)
        @assert size(stage2out) == (outWidth * cfg.castLen, cfg.batchLen)
        out = reshape(stage2out, outSize)
        @assert size(out) == outSize
        return out
    end

    # Consider passing in previous value so we can check if we matched above or below
    function loss(batch)
        x, cast, y = batch
        # @assert size(y[1]) == (1, cfg.castLen, cfg.batchLen) string(size(y[1]), ' ', (1, cfg.castLen, cfg.batchLen))
        yy = dropdims(y[1]; dims=1)
        global yhat = exec(x, cast)
        # @assert size(yhat) == (outWidth, cfg.castLen, cfg.batchLen)
        # y is actual value for each castLen and batchLen
        # yhat is outWidth number of predicted guesses of y
        # @assert eltype(y[1]) == eltype(yhat) string(eltype(y[1]), ' ', eltype(yhat))
        # err = sum(mapslices(yh -> Flux.Losses.mse(yh, yy), yhat; dims=[2,3]))
        # err = sum(slicemap(yh -> Flux.Losses.mse(yh, yy), yhat; dims=[2,3]))
        err = 0.0
        for yh in (yhat[i,:,:] for i in axes(yhat, 1))
            err += Flux.Losses.mse(yh, yy)
        end
        return err
    end

    encer = cfg.encoder |> DEV
    encerCast = cfg.encoderCast |> DEV

    layers = (;encer, encerCast, dense1, combine)
    params = Flux.params(layers...)
    opt = Flux.Adam()
    return (;layers, params, exec, loss, opt)
end

end