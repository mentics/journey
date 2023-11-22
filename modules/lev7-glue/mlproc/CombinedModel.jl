module CombinedModel
import Tables, Dates
using DataFrames, Arrow
using Flux, MLUtils, CUDA
import HistoricalModel as hm
using IndexUtil, TrainUtil, ModelUtil, FileUtil
using MLRun
import Calendars as cal
import DataFiles as dat

NAME = @__MODULE__
MOD_VERSION = "b4l4d4s32"

#region Config
data_params() = (;
    bins_count = 200,
    ret_min = -0.15,
    ret_max = 0.15,
    input_width = 48
)

model_params() = (;
    historical_model_version = HistoricalModel.VERSION,
    activation = NNlib.swish,
    hidden_width = 512,
    block_count = 4,
    layers_per_block = 4,
    dropout = 0.4,
    use_bias = false
)

train_hypers() = (;
    batch_size = 32,
    holdout = 0.1,
)

function config()
    return (;
        MOD_VERSION,
        data_params()...,
        model_params()...,
        train_hypers()...,
    )
end
#endregion Config

#region MLRun Interface
function mlrun()
    inference_model = training_model = make_model() |> gpu
    run = x -> run_model(training_model, x)

    global state = Trainee7(;
        name=NAME,
        version=MOD_VERSION,
        training_model,
        inference_model,
        run_model = run,
        infer = run,
        batches = make_data(),
        get_learning_rate = TrainUtil.learning_rate_linear_decay(), # TrainUtil.lr_cycle_decay(),
        get_loss = calc_loss,
        cfg = config(),
        mod = @__MODULE__
    )
    return state
end

to_draw_x(batch, ind) = batch.x[:,ind]
to_draw_yh(yhat, ind) = softmax(yhat[:,ind])
#endregion MLRun Interface

#region mlrun impl
import HistoricalModel
function make_model()
    cfg = config()
    input = Dense(cfg.input_width => cfg.hidden_width; bias=cfg.use_bias)
    through_width = cfg.hidden_width

    blocks1_count = floor(Int, cfg.block_count / 2)
    blocks2_count = cfg.block_count - blocks1_count
    blocks1 = [SkipConnection(ModelUtil.make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks1_count]
    dropout = Dropout(cfg.dropout)
    blocks2 = [SkipConnection(ModelUtil.make_block(cfg, through_width, cfg.hidden_width, cfg.layers_per_block), +) for _ in 1:blocks2_count]

    output = Dense(through_width => cfg.bins_count; bias=false)
    return Chain(;input, blocks=Chain(blocks1..., dropout, blocks2...), output)
end

function run_model(model, batch)
    yhat = model(batch.x)
    return yhat
end

function calc_loss(model, batch)
    yhat = run_model(model, batch)
    return Flux.Losses.logitcrossentropy(yhat, batch.y; dims=1)
end
#endregion MLRun Interface

#region Data
COMBINED_INPUT_PATH = joinpath(FileUtil.root_shared(), "mlrun", "CombinedModel", "data", "CombinedModel-input.arrow")
using SearchSortedNearest
function make_data_input()
    (;bins_count, ret_min, ret_max) = config()
    bins = ModelUtil.make_bins(bins_count, ret_min, ret_max)

    # Restrict to ts's for which we have encoded hist data
    hist = hm.load_data_encoded()
    hist_valid_ts = Set(hist.ts)

    df = dat.tsx_df()
    filter!(:ts => (ts -> ts in hist_valid_ts), df)
    dropmissing!(df, [:ret, :extrin])
    len_before = length(df[!,1])

    # TODO: consider including extrin directly in the future
    transform!(df, [:extrin, :tex] => ((xtrin, tex) -> xtrin ./ sqrt.(tex)) => :vol)
    # transform!(df, :ret => (r -> Float32.(r .- 1.0)) => :ret)
    transform!(df, :ret => (r -> searchsortednearest.(Ref(bins), r .- 1)) => :y_bin)

    # Add temporal columns
    transform!(df, [:ts, :expiration] => ((ts, xpir) -> dur_to_input.(cal.calcDur.(ts, xpir))) => [:closed, :pre, :open, :post, :weekend, :holiday])
    transform!(df, [:ts] => (ts -> broadcast.(Float32, ModelUtil.to_temporal.(ts))) => [:week_day, :month_day, :quarter_day, :year_day, :hour])

    # Add historical columns
    dfhist = DataFrame(hist)
    leftjoin!(df, dfhist; on=:ts)

    select!(df, :y_bin, Not(:y_bin, :ts, :expiration, :ret, :tex, :extrin, :extrindt, :iv_mean, :logret))
    dropmissing!(df)

    @assert len_before == length(df[!,1]) "len_before == length(df[!,1]): $(len_before) == $(length(df[!,1]))"
    dat.save(COMBINED_INPUT_PATH, df)
    return df
end

dur_to_input(dur) = Float32.(([getfield(dur, nam) for nam in propertynames(dur)]) ./ (Dates.Day(dat.XPIRS_RANGE2.stop)))

COMBINED_INPUT_CACHE = nothing
function load_input(;refresh=false)
    if refresh || isnothing(COMBINED_INPUT_CACHE)
        global COMBINED_INPUT_CACHE = Arrow.Table(COMBINED_INPUT_PATH)
    end
    return COMBINED_INPUT_CACHE
end
clear_input() = global COMBINED_INPUT_CACHE = nothing

function make_data()
    cfg = config()
    input = load_input()
    (;batch_count, inds_batches, inds_holdout) = IndexUtil.inds_for_batches(length(input[1]), cfg.batch_size, cfg.holdout)
    # TODO: write directly to CuArray's?
    get_batch = function(iepoch, ibatch)
        inds = inds_batches[ibatch]
        return batch_from_inds(input, inds)
    end
    holdout = batch_from_inds(input, inds_holdout)
    return Batches2(;get=get_batch, count=batch_count, holdout)
end

function batch_from_inds(input, inds)
    y = Flux.onehotbatch(input.y_bin[inds], 1:config().bins_count)
    col_inds = 2:length(Tables.columnnames(input))
    x = reduce(vcat, [Tables.getcolumn(input, i)[inds]' for i in col_inds])
    return (;x, y) |> gpu
end

# function batch_from_inds(input, inds)
#     hist = reduce(hcat, [col[inds] for col in input.hist_columns])
#     temporal = nothing
#     options = nothing
#     return (; hist, temporal, options) |> gpu
# end
#endregion Data

end
