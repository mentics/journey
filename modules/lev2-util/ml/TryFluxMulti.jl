module TryFluxMulti
using Flux

const N = Float32

# TYPE_ARR = Array
INPUT_WIDTH = 5
INPUT_LEN = 70
INPUT_SIZE = (INPUT_WIDTH, INPUT_LEN)
OUTPUT_WIDTH = INPUT_WIDTH
OUTPUT_LEN = 20
OUTPUT_SIZE = (OUTPUT_WIDTH, OUTPUT_LEN)
INPUT = Array{N, length(INPUT_SIZE)}
OUTPUT = Array{N, length(OUTPUT_SIZE)}
BATCH_COUNT = 10
BATCH_SIZE = 64
BATCH_INPUT_SIZE = (INPUT_SIZE..., BATCH_SIZE)
BATCH_OUTPUT_SIZE = (OUTPUT_SIZE..., BATCH_SIZE)
BATCH_INPUT = Array{eltype(INPUT), max(1,ndims(INPUT))+1}
BATCH_OUTPUT = Array{eltype(OUTPUT), max(1,ndims(OUTPUT))+1}
BatchBufferInput = BATCH_INPUT(undef, INPUT_WIDTH, INPUT_LEN, BATCH_SIZE)
BatchBufferOutput = BATCH_OUTPUT(undef, OUTPUT_WIDTH, OUTPUT_LEN, BATCH_SIZE)

function run()
    global model = makeModel2()
    opt = Descent()
    train(model, opt)
    return model
end

function valueAt(seqIndex::Int)::NTuple{INPUT_WIDTH,N}
    s5 = sin(2π*seqIndex/100)
    s4 = sin(2π*seqIndex/10)
    s3 = sin((s5 > 0 ? -1 : 1) * 2π*seqIndex/200)
    s2 = sin((s4 > 0 ? -1 : 1) * 2π*seqIndex/37)
    s1 = s2 + s3
    return (s1, s2, s3, s4, s5)
end

function batchInput!(buf::BATCH_INPUT, seqStart::Int)
    for b in 1:BATCH_SIZE
        for i in 1:INPUT_LEN
            buf[:,i,b] .= valueAt(b + seqStart + i)
        end
    end
    return
end

function batchOutput!(buf::BATCH_OUTPUT, seqStart::Int)
    seqStart += INPUT_LEN
    for b in 1:BATCH_SIZE
        for i in 1:OUTPUT_LEN
            buf[:,i,b] .= valueAt(b + seqStart + i)
        end
    end
    return
end

function batchTrain(i::Int)::Tuple{BATCH_OUTPUT,BATCH_OUTPUT}
    batchInput!(BatchBufferInput, i)
    batchOutput!(BatchBufferOutput, i)
    return (BatchBufferInput, BatchBufferOutput)
end

function batchTest(i::Int)::Tuple{BATCH_OUTPUT,BATCH_OUTPUT}
    ind = i+7171
    batchInput!(BatchBufferInput, ind)
    batchOutput!(BatchBufferOutput, ind)
    return (BatchBufferInput, BatchBufferOutput)
end

# unsq(x) = Flux.unsqueeze(x; dims=INPUT_SIZE)
# (2,3,2)
unflatten(dims::NTuple{N,Int}) where N = x -> reshape(x, dims)

function makeModel()
    exec = Chain(Flux.flatten,
          Dense(INPUT_WIDTH * INPUT_LEN => OUTPUT_WIDTH * OUTPUT_LEN),
          unflatten(BATCH_OUTPUT_SIZE))
    params = Flux.params(exec)
    loss(x::Union{INPUT,BATCH_INPUT}, y::Union{OUTPUT,BATCH_OUTPUT}) = Flux.Losses.mse(exec(x), y)
    return (;exec, loss, params)
end

function makeModel2()
    exec = Chain(Flux.flatten,
          Dense(INPUT_WIDTH * INPUT_LEN => 4096),
          Dense(4096 => OUTPUT_WIDTH * OUTPUT_LEN),
        #   Dense(INPUT_WIDTH * INPUT_LEN => OUTPUT_WIDTH * OUTPUT_LEN),
          unflatten(BATCH_OUTPUT_SIZE))
    params = Flux.params(exec)
    loss(x::Union{INPUT,BATCH_INPUT}, y::Union{OUTPUT,BATCH_OUTPUT}) = Flux.Losses.mse(exec(x), y)
    return (;exec, loss, params)
end

function train(model, opt)
    dataIter = (batchTrain(i) for i in 1:BATCH_COUNT)
    progress = trainProgress(model, first(dataIter), .01, .1)
    for _ in 1:1000
        Flux.train!(model.loss, model.params, dataIter, opt)
        if progress()
            break
        end
    end
    println("Last train loss: ", model.loss(first(dataIter)...))
end

function trainProgress(model, dataCheck, lossTarget, seconds)
    return Flux.throttle(seconds) do
        los = model.loss(dataCheck...)
        return los < lossTarget
    end
end

import DrawUtil:draw,draw!
function test(model)
    println("Testing")
    dataIter = (batchTest(i) for i in 1:BATCH_COUNT)
    err = 0.0
    len = 0
    actual = Array{N, length(OUTPUT_SIZE)}(undef, OUTPUT_WIDTH, 0) # fill(0, length(OUTPUT_SIZE))...)
    predict = Array{N, length(OUTPUT_SIZE)}(undef, OUTPUT_WIDTH, 0) # fill(0, length(OUTPUT_SIZE))...)
    for b in dataIter
        err += model.loss(b...)
        len += 1
        actual = hcat(actual, b[2][:,:,end])
        predict = hcat(predict, model.exec(b[1])[:,:,end])
    end
    errMean = err / len
    println("Testing mean loss (len=$(len)) = $(errMean)")
    display(draw(actual[1,:]))
    draw!(predict[1,:])
    return (actual, predict)
end

end