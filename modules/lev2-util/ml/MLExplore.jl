module MLExplore
using Flux, Transformers, CUDA

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
    res = [[sin(i/100.0) cos(i/100.0)] for i in 1:length]

    # res = Array{Float64}(undef, 0, 2)
    # for i in 1:length
    #     x1 = sin(i/100.0)
    #     x2 = cos(i/100.0)
    #     vcat(res, [x1 x2])
    # end

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
    model = makeModel1(params.model)
    train!(params, model, xySplitBatched)
end

# This doesn't have to return the same size as the input
function tokenizeData(m)
    # TODO
    return m
end

# TODO: this is generic, can be moved to util
function splitTrainValidation(m, validationRatio = 0.1)
    len = length(m)
    split = round(Int, (1 - validationRatio) * len)
    return (; train = (@view m[1:split,:]), validation = (@view m[split+1:len,:]))
end

# TODO: this is generic, can be moved to util
function makeBatches(block_size, dataTokens)
    num = length(dataTokens) - block_size - 1
    x = [dataTokens[i:i + block_size - 1] for i in 1:num]
    y = [dataTokens[i + 1:i + block_size] for i in 1:num]

    # x = Array{Float64}(undef, 0, ndims(dataTokens) + 1)
    # y = Array{Float64}(undef, 0, ndims(dataTokens) + 1)
    # for _ in 1:num
    #     # TODO: use LFSR to get both random, and complete, and no duplication
    #     i = rand(1:num)
    #     x = dataTokens[i:i + block_size]
    #     y = dataTokens[i + 1:i + block_size + 1]
    # end

    return (; x, y)
end

function makeModel1(paramsModel)
    return Flux.Dense(paramsModel.block_size => paramsModel.block_size)
end

function train!(params, model, data)
    x1 = data.train.x[1]
    y1 = data.train.y[1]
    yhat1 = model(x1)
    Flux.Losses.crossentropy(yhat1, y)
end

end