module Forecast
import Flux #: Flux,Dense,ADAM,gradient,gpu
# import Flux.Optimise: update!
# import Transformers: Transformer,TransformerDecoder,todevice,enable_gpu

#==
Input:
    close - closePrev
    vix
    day of the week
    day of the quarter
    day of the month
Output:
    bincnt x castlen matrix prob forecast

Layers:
    softmax (which algo to make it reasonably smooth probs?)
==#

function makeModel()
    model = Chain(Flux.flatten,
          Dense(cfg.inputWidth * cfg.inputLen => 4096),
          Dense(4096 => 4096),
          Dense(4096 => cfg.outputWidth * cfg.outputLen),
          unflatten(cfg.batchOutputSize))
    loss(x, y) = Flux.Losses.mse(model(x), y)
    return (;model, loss)
end

#region TimeSeries
function timeSeries(xyIter, model, loss)
    params = Flux.params(model)
end

function toXyIter()
end
#endregion

end