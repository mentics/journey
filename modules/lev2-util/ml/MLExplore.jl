module MLExplore
using Flux, Transformers, CUDA
using Compat
import Random

# Fluxml expects batch to be the last dimension

# const DATA_SPLIT4 = NamedTuple{(:train, :validation), Tuple{A,B}} where {A<:AbstractArray,B<:AbstractArray}
# const DATA_XY_SPLIT = NamedTuple{(:train, :validation), Tuple{A,B}} where {A<:AbstractArray,B<:AbstractArray}

# All orig data has dims:
# dim1: time. So the "rows" of the data are time.
# dimN: all the other dims (channel(s)?) are a tensor for that time and opaque to processing
# but it must be at least [n,m] so no simple scalar data.
# we don't have a separate output for now, we just forecast a timeseries
# but the scoring function might only use part of the output.


#=
To run test:
mlx.run()
=#

function run(cfg=makeCfg(), input=makeOrigSeqSinCos())
    enable_gpu(CUDA.functional())
    Random.seed!(1)

    global dataTokens = tokenizeData(input)
    global dataSplit = splitTrainValidation(dataTokens)
    global xySplitBatched = map(d -> makeBatches(cfg.block_size, d), dataSplit)
    global model = makeModel1(cfg)
    train!(cfg, model, xySplitBatched)
    # global start = rand(Float32, cfg.block_size, 2, 1)
    global starti = 1 # rand(1:(size(dataTokens,1) - cfg.block_size - 1))
    global start = dataTokens[starti:(starti+cfg.block_size-1),:]
    global genned = generate(model, start, 10)
    return starti, genned
end

function makeCfg()
    return (;
        # Model related
        block_size = 8,

        # Execution related
        batch_size = 32,
        trainIterCount = 10000,
        target_loss = 1e-10,
        learning_rate = 1e-1,
    )
end

# This outputs a lenth x 2 matrix (sequence)
function makeOrigSeqSinCos(length = 1000)
    res = reduce(vcat, [Float32(sin(i/100.0)) Float32(cos(i/100.0))] for i in 1:length)
    return res
end

# This doesn't have to return the same size as the input
function tokenizeData(m)
    # TODO
    return m
end

# TODO: this is generic, can be moved to util
function splitTrainValidation(m, validationRatio = 0.1)
    len = size(m, 1)
    split = round(Int, (1 - validationRatio) * len)
    # println("len $(len) split $(split)")
    return (; train = (@view m[1:split,:]), validation = (@view m[split+1:len,:]))
end

# TODO: this is generic, can be moved to util
function makeBatches(block_size, dataTokens)
    ts_count = size(dataTokens, 1)
    num = ts_count - block_size - 1
    @assert size(dataTokens) == (ts_count,2) "$(size(dataTokens)) == $((num,2))"
    # Batch is last dim and that's the default for stack
    x = stack(dataTokens[i:i + block_size - 1,:] for i in 1:num)
    y = stack(dataTokens[i:i + block_size - 1,:] for i in 1:num)
    @assert size(x) == (block_size, 2, num) "$(size(x)) == $((block_size, 2, num))"
    return (; x, y)
end

function makeModel1(cfg)
    model = Flux.Dense(cfg.block_size => cfg.block_size)
    # return Flux.Dense(cfgModel.block_size => 1)
    params = Flux.params(model)
    opt = ADAM(cfg.learning_rate)
    exec = function(x)
        return model(x)
    end
    function loss(x, y)
        yhat = exec(x)
        return Flux.Losses.mse(yhat, y)
        # return Flux.Losses.logitcrossentropy(yhat, y)
    end
    return (;cfg, exec, opt, params, loss)
end

function train!(cfg, model, data)
    prevLoss = 1e10
    for i=0:cfg.trainIterCount
        ind = 1 + i % size(data.train.x, ndims(data.train.x))
        x = data.train.x[:,:,ind]
        y = data.train.y[:,:,ind]
        grad = gradient(() -> model.loss(x, y), model.params)
        Flux.update!(model.opt, model.params, grad)
        loss = model.loss(x, y)
        if loss < prevLoss / 2
            println("loss: ", loss)
            prevLoss = loss
        end
    end
end

function generate(model, start, forecast_count)
    # T x C x B
    start = reshape(start, size(start)..., 1)
    @assert size(start) == (8, 2, 1) string("expected ", (8, 2, 1), " but was ", size(start))
    res = start
    for _=1:forecast_count
        y = model.exec(res[end-7:end,:,:])
        # println("y size: ", size(y))
        yend = y[end:end,:,:]
        # println("yend size: ", size(yend))
        # logitsLast = logits[:,end,:]
        # probs = Flux.softmax(logitsLast)
        res = cat(res, yend; dims=1)
    end
    return res
end

end