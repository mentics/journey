module TryFluxMulti
using Flux
import MLUtil
using CUDA

Base.IteratorSize(::Type{<:CuIterator}) = Base.SizeUnknown()

const N = Float32

function config()
    inputWidth = 5
    inputLen = 70
    outputLen = 2
    batchCount = 100
    batchSize = 128

    inputSize = (inputWidth, inputLen)
    outputWidth = inputWidth
    outputSize = (outputWidth, outputLen)
    InputType = Array{N, length(inputSize)}
    OutputType = Array{N, length(outputSize)}

    batchInputSize = (inputSize..., batchSize)
    batchOutputSize = (outputSize..., batchSize)
    BatchInputType = Array{eltype(InputType), max(1,ndims(InputType))+1}
    BatchOutputType = Array{eltype(OutputType), max(1,ndims(OutputType))+1}
    bufIn = BatchInputType(undef, inputWidth, inputLen, batchSize)
    bufOut = BatchOutputType(undef, outputWidth, outputLen, batchSize)

    return (; inputWidth, inputLen, outputLen, batchCount, batchSize,
            inputSize, outputWidth, outputSize,
            batchInputSize, batchOutputSize, batchBufferInput, batchBufferOutput,
            bufIn, bufOut)
end

function run()
    cfg = config()
    global model = makeModel(cfg)
    opt = Adam()
    train(cfg, model, opt)
    return model
end

function valueAt(seqIndex::Int) # ::NTuple{cfg.inputWidth,N}
    s5 = sin(2π*seqIndex/100)
    s4 = sin(2π*seqIndex/10)
    s3 = sin((s5 > 0 ? -1 : 1) * 2π*seqIndex/200)
    s2 = sin((s4 > 0 ? -1 : 1) * 2π*seqIndex/37)
    s1 = s2 + s3
    return (s1, s2, s3, s4, s5)
end

function batchInput!(buf, seqStart::Int) # cfg.batchInputType
    _, len, batchSize = size(buf)
    for b in 1:batchSize
        for i in 1:len
            buf[:,i,b] .= valueAt(b + seqStart + i)
        end
    end
    return
end

function batchOutput!(buf, seqStart::Int) # cfg.batchOutputType
    _, len, batchSize = size(buf)
    seqStart += len
    for b in 1:batchSize
        for i in 1:len
            buf[:,i,b] .= valueAt(b + seqStart + i)
        end
    end
    return
end

function batchTrain(model, i::Int)::Tuple{BATCH_OUTPUT,BATCH_OUTPUT}
    batchInput!(model.bufIn, i)
    batchOutput!(model.bufOut, i)
    return (model.bufIn, model.bufOut)
end

function batchTest(model, i::Int)::Tuple{BATCH_OUTPUT,BATCH_OUTPUT}
    ind = i+7171
    batchInput!(model.bufIn, ind)
    batchOutput!(model.bufOut, ind)
    return (model.bufIn, model.bufOut)
end

function makeModel1()
    exec = Chain(Flux.flatten,
          Dense(cfg.inputWidth * cfg.inputLen => cfg.outputWidth * cfg.outputLen),
          MLUtil.unflatten(cfg.batchOutputSize))
    params = Flux.params(exec)
    loss(x::Union{INPUT,BATCH_INPUT}, y::Union{OUTPUT,BATCH_OUTPUT}) = Flux.Losses.mse(exec(x), y)
    return (;exec, loss, params)
end

function makeModel(cfg)
    exec = Chain(Flux.flatten,
          Dense(cfg.inputWidth * cfg.inputLen => 4096),
          Dense(4096 => 4096),
          Dense(4096 => cfg.outputWidth * cfg.outputLen),
          MLUtil.unflatten(cfg.batchOutputSize)) |> gpu
    params = Flux.params(exec)
    # loss(x::Union{<:INPUT,<:BATCH_INPUT}, y::Union{<:OUTPUT,<:BATCH_OUTPUT})::Float32 = Flux.Losses.mse(exec(x), y)
    loss(x, y) = Flux.Losses.mse(exec(x), y)
    lossm(x, y, mexec) = Flux.Losses.mse(mexec(x), y)
    return (;exec, loss, params, lossm)
end

function train(cfg, model, opt)
    dataIter = CuIterator(batchTrain(cfg, i) for i in 1:BATCH_COUNT)
    dataCheck = deepcopy(batchTrain(cfg, 1)) |> gpu
    progress = trainProgress(model, dataCheck, .000001, 1)
    for i in 1:1000
        Flux.train!(model.loss, model.params, dataIter, opt)
        if progress(i)
            break
        end
    end
    println("Last train loss: ", model.loss(dataCheck...))
end

function trainProgress(model, dataCheck, lossTarget, seconds)
    return Flux.throttle(seconds) do i
        los = model.loss(dataCheck...)
        println("$(i): loss = ", los)
        return los < lossTarget
    end
end

import DrawUtil:draw,draw!
function test(cfg, model)
    println("Testing")
    execCpu = model.exec |> cpu
    dataIter = (batchTest(model, i) for i in 1:BATCH_COUNT)
    err = 0.0
    len = 0
    actual = cfg.InputType(undef, cfg.outputWidth, 0)
    predict = cfg.OutputType(undef, cfg.outputWidth, 0)
    for b in dataIter
        err += model.lossm(b..., execCpu)
        len += 1
        actual = hcat(actual, b[2][:,:,end])
        predict = hcat(predict, execCpu(b[1])[:,:,end])
    end
    errMean = err / len
    println("Testing mean loss (len=$(len)) = $(errMean)")
    display(draw(actual[1,:]; color=:green))
    draw!(predict[1,:]; color=:yellow)
    return (actual, predict)
end

end
