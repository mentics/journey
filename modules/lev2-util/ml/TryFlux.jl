module TryFlux
using Flux

const Numb = Float32

TYPE_ARR = Array
INPUT = Numb
OUTPUT = Numb
INPUT_SIZE = 1
OUTPUT_SIZE = 1
BATCH_COUNT = 1
BATCH_SIZE = 64
BATCH_INPUT = TYPE_ARR{eltype(INPUT), max(1,ndims(INPUT))+1}
BATCH_OUTPUT = TYPE_ARR{eltype(OUTPUT), max(1,ndims(OUTPUT))+1}

actual(x::INPUT)::OUTPUT = 4x + 2

# using TryFlux
# TryFlux.test()

using Flux
function test()
    model = Dense(1 => 1)
    params = Flux.params(model)
    function loss(x, y)
        Flux.reset!(model)
        Flux.Losses.mse(model(x), y)
    end
    Flux.train!(loss, params, [([1.0],[1.0])], Descent())
end

function run()
    model = makeModel()
    opt = Descent()
    train(model, opt)
    return model
end

function batchTrain(i::Int)::Tuple{BATCH_OUTPUT,BATCH_OUTPUT}
    x = convert.(Numb, hcat(0:5...))
    y = actual.(x)
    return (x, y)
end

function batchTest(i::Int)::Tuple{BATCH_OUTPUT,BATCH_OUTPUT}
    x = convert.(Numb, hcat(6.0:10.0...))
    y = actual.(x)
    return (x, y)
end

function makeModel()
    layer1 = Dense(INPUT_SIZE => OUTPUT_SIZE)
    exec = layer1
    params = Flux.params(layer1)
    loss(x::Union{INPUT,BATCH_INPUT}, y::Union{OUTPUT,BATCH_OUTPUT}) = begin
        Flux.reset!(exec)
        Flux.Losses.mse(exec(x), y)
    end
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
end

function trainProgress(model, dataCheck, lossTarget, seconds)
    return Flux.throttle(seconds) do
        los = model.loss(dataCheck...)
        return los < lossTarget
    end
end

function test(model)
    println("Testing")
    dataIter = (batchTest(i) for i in 1:BATCH_COUNT)
    err = 0.0
    len = 0
    for b in dataIter
        err += model.loss(b...)
        len += 1
    end
    errMean = err / len
    println("Testing mean loss = $(errMean)")
end

end