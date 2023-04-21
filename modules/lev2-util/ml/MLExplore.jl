module MLExplore
using Flux, Transformers, CUDA

enable_gpu(CUDA.functional())

# All orig data has dims:
# dim1: time. So the "rows" of the data are time.
# dimN: all the other dims are a tensor for that time and opaque to processing
# but it must be at least [n,m] so no simple scalar data.
# we don't have a separate output for now, we just forecast a timeseries
# but the scoring function might only use part of the output.

function makeOrigData1(length = 1000)
    res = Array{Float64}(undef, 0, 2)
    for i in 1..length
        x1 = sin(i/100.0)
        x2 = cos(i/100.0)
        vcat(res, [x1 x2])
    end
    return res
end

function run(input)
    dataRaw = splitTest(input)
    dataEncoded = map(encodeData, dataRaw)
    train!(dataEncoded)
end

function encodeData(m)
    # TODO
    return m
end

function model()

end

function train!(model)

end

function splitTest(m, testRatio = 0.1)
    split = round(Int, (1 - testRatio) * len)
    return (; train = (@view m[1:split,:]), test = (@view m[split+1:len,:]))
end

end