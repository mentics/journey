module Far

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
    checkSeq()
    return
end
train() = Forecast.train(config, mod, batchers.train)
test() = Forecast.test(config, mod, batchers.test)

function makeModel(cfg)
    castSize = sum(cfg.castWidths) * cfg.castLen

    encer = cfg.encoder |> DEV
    encerCast = cfg.encoderCast |> DEV

    exec = (x, cast) -> begin
        x1 = encer(x)
        @assert size(x1) == cfg.encedSize "Size mismatch $(size(x1)) $(cfg.encedSize)"


        ef = Flux.flatten(e)
        # @assert size(ef) == (tfSize, cfg.batchLen)
        enc1 = tenc1(ef)
        enc2 = tenc2(enc1)
        enc2a = decin(enc2)

        castin1 = encerCast(cast...) # cat(ForecastUtil.encodeDur(cast[1]), ForecastUtil.encodeDates(cast[2]); dims=1)
        # @assert size(castin1) == (sum(cfg.castWidths), cfg.castLen, cfg.batchLen)
        # println(size(castin1))
        castin2 = decinCast(Flux.flatten(castin1))
        dec1 = tdec1(enc2a, castin2)
        dec2 = tdec2(dec1, castin2)
        dec3 = reshape(dec2, (cfg.binCnt, cfg.castLen, cfg.batchLen))
        out = linout(dec3)
        return out
    end
    # TODO: consider  Flux.Losses.label_smoothing(Flux.onehot(1, 1:10), .1)
    function loss(batch)
        x, cast, y = batch
        pred = exec(x, cast)
        # NOTE: check if pred is all zeros anywhere along the way
        r = Flux.logitcrossentropy(pred, y[1])
        return r
    end

    layers = (;encer, tenc1, tenc2, decin, encerCast, decinCast, tdec1, tdec2, linout)
    # params = union(Flux.params(tenc1, tenc2, tdec1, tdec2, linout), paramsEncoder)
    params = Flux.params(layers...)
    opt = Flux.Adam()
    # reset = () -> for layer in layers; Flux.reset!(layer) end
    return (;layers, params, exec, loss, opt)
end

end