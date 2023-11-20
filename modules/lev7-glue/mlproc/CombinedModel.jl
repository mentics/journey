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
function mlrun()
    inference_model = training_model = make_model()
    run = x -> run_model(training_model, x)

    global state = Trainee4(;
        name=NAME,
        version=VERSION,
        training_model,
        inference_model,
        run_model = run,
        infer = run,
        batches = make_data(),
        get_learning_rate = TrainUtil.learning_rate_linear_decay(), # TrainUtil.lr_cycle_decay(),
        get_loss = calc_loss,
        mod = @__MODULE__
    )
    return state
end

to_draw_x(batch, ind) = batch.under.v[1:end,ind]
to_draw_yh(yhat, ind) = yhat.under.v[1:end,ind]
#endregion MLRun Interface

#region mlrun impl
function make_model()
    cfg = mod.config()
    return Chain(; ...)
end

# TODO: get_batch has to return (;hist=(;v, meta_under, meta_vix), ts=(;...), etc...)

function run_model(model, batch)
    model(batch.x)
    hist = run_encode(model, batch)
    (;under, under_mask, vix, vix_mask) = batch
    (yhat_under_raw, yhat_vix_raw) = model((under, vix))
    yhat_under = yhat_under_raw .* under_mask
    yhat_vix = yhat_vix_raw .* vix_mask
    return (yhat_under, yhat_vix)
end

calc_loss(model, batch)
    return Flux.Losses.logitcrossentropy(run_model(model, batch.x), batch.y; dims=1)
end
#endregion MLRun Interface

function setup_data()
    # Combine the following features:
    #   - Encode time stamp
    #   - MarketDur until expiration MarketDurUtil/Types
    #   - tsx: tex?, extrin, extrindt, vol
    #   - HistoricalModel
end

end