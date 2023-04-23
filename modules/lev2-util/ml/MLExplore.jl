module MLExplore
using Flux, Transformers, CUDA
using Compat

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

# This outputs a lenth x 2 matrix (sequence)
function makeOrigSeqSinCos(length = 1000)
    res = reduce(vcat, [Float32(sin(i/100.0)) Float32(cos(i/100.0))] for i in 1:length)
    return res
end

function makeParams()
    return (;
        model=(;
            block_size = 8,
        ),
        train=(;
            batch_size = 32,
        )
    )
end

function run(params=makeParams(), input=makeOrigSeqSinCos())
    enable_gpu(CUDA.functional())
    dataTokens = tokenizeData(input)
    dataSplit = splitTrainValidation(dataTokens)
    xySplitBatched = map(d -> makeBatches(params.model.block_size, d), dataSplit)
    global model = makeModel1(params.model)
    train!(params, model, xySplitBatched)
    start = rand(Float32, params.model.block_size, 2, 1)
    generate(model, start, 10);
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
    println("len $(len) split $(split)")
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

function intoNewDim(itr)
    x1, itr = Iterators.peel(itr)
    sz = size(x1)
    return reduce(vcat, itr; init=reshape(x1, 1, sz...))
end

function makeModel1(paramsModel)
    return Flux.Dense(paramsModel.block_size => paramsModel.block_size)
    # return Flux.Dense(paramsModel.block_size => 1)
end

function train!(params, model, data)
    global dd = data
    x1 = data.train.x[:,:,1]
    y1 = data.train.y[:,:,1]
    println(typeof(x1), ' ', size(x1))
    yhat1 = model(x1)
    Flux.Losses.logitcrossentropy(yhat1, y1)
end

function generate(model, start, forecast_count)
    # T x C x B
    @assert size(start) == (8, 2, 1) string("expected ", (8, 2, 1), " but was ", size(start))
    res = start
    for _=1:forecast_count
        y = model(res[end-7:end,:,:])
        println("y size: ", size(y))
        yend = y[end:end,:,:]
        println("yend size: ", size(yend))
        # logitsLast = logits[:,end,:]
        # probs = Flux.softmax(logitsLast)
        res = cat(res, yend; dims=1)
    end
    return res
end

end