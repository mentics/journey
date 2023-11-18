module CombinedModule

import HistoricalModel

#region Config
data_params() = (;
)

model_params() = (;
    historical_model_version = HistoricalModel.VERSION,
)

train_hypers() = (;
    batch_size = 1024,
    learning_rate_func = MLRun.learning_rate_linear_decay,
)

function config()
    return (;
        VERSION,
        data_params()...,
        model_params()...,
        train_hypers()...,
    )
end
#endregion Config

#region MLRun Interface
make_data() = _make_data(mod.config())

function make_model()
    cfg = mod.config()

    return Chain(; encoder=model_encoder(cfg), decoder=model_decoder(cfg))
end

function run_model(model, batch)
    hist = run_encode(model, batch)
    (;under, under_mask, vix, vix_mask) = batch
    (yhat_under_raw, yhat_vix_raw) = model((under, vix))
    yhat_under = yhat_under_raw .* under_mask
    yhat_vix = yhat_vix_raw .* vix_mask
    return (yhat_under, yhat_vix)
end

function run_encode(model, batch)
    # It's expected that the input is already masked, so no need to multiply it by the mask
    (;under, vix) = batch
    return model.layers.encoder((under, vix))
end

MLRun.make_loss_func(mod::typeof(@__MODULE__)) = function(model, batch)
    (yhat_under, yhat_vix) = MLRun.run_model(mod, model, batch)

    # l = Flux.mse(yhat_under, under) + Flux.mse(yhat_vix, vix)
    (;under, vix) = batch
    l = Flux.mae(yhat_under, under) + Flux.mae(yhat_vix, vix)
    # global kloss_vals = (;under, under_mask, vix, vix_mask, yhat_under_1, yhat_vix_1, yhat_under, yhat_vix, loss=l)
    return l
end
#endregion MLRun Interface


end