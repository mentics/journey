module TryFluxArchitextures
using FluxArchitectures
using DrawUtil

function run()
    poollength = 10; horizon = 15; datalength = 1000;
    input, target = get_data(:exchange_rate, poollength, datalength, horizon)

    global model = LSTnet(size(input, 1), 2, 3, poollength, 120, init=Flux.zeros32, initW=Flux.zeros32)
    function loss(x, y)
        # println(size(x), ' ', size(y))
        Flux.mse(model(x), y')
    end

    cb = function ()
        Flux.reset!(model)
        global pred = vec(model(input)' |> cpu)
        global tgt = collect(cpu(target))
        # println("pred ", typeof(pred), " | ", size(pred))
        # println("target ", typeof(target), " | ", size(target))
        p1 = draw(pred; label="Predict")
        display(p1)
        p1 = draw!(tgt; label="Data")#, title="Loss $(loss(input, target))")
        # display(plot(p1))
    end

    Flux.train!(loss, Flux.params(model), Iterators.repeated((input, target), 1000), ADAM(0.01); cb=Flux.throttle(cb, .5))

    println("end: ", loss(input, target))
end

function run1()
    @info "Loading data"
    poollength = 10
    horizon = 15
    datalength = 1000
    global input, target = get_data(:exchange_rate, poollength, datalength, horizon)

    @info "Creating model and loss"
    inputsize = size(input, 1)
    convlayersize = 2
    recurlayersize = 3
    skiplength = 120
    global model = LSTnet(inputsize, convlayersize, recurlayersize, poollength, skiplength, init=Flux.zeros32, initW=Flux.zeros32)

    function loss(x, y)
        # println("loss: ", typeof(x), "\n", typeof(y))
        # Flux.reset!(model)
        l = Flux.mse(model(x), y')
        return l
    end

    cb = function ()
        Flux.reset!(model)
        global pred = vec(model(input)' |> cpu)
        Flux.reset!(model)
        global tgt = collect(cpu(target))
        # println("pred ", typeof(pred), " | ", size(pred))
        # println("target ", typeof(target), " | ", size(target))
        p1 = draw(pred; label="Predict")
        display(p1)
        p1 = draw!(tgt; label="Data")#, title="Loss $(loss(input, target))")
        # display(plot(p1))
    end

    Flux.reset!(model)

    @info "Start loss" loss = loss(input, target)
    @info "Starting training"
    # Flux.train!(loss, Flux.params(model), Iterators.repeated((input, target), 20), ADAM(0.01)) # , cb=cb)
    Flux.train!(loss, Flux.params(model), [(input, target)], ADAM(0.01)) # , cb=cb)
    @info "Final loss" loss = loss(input, target)
end

function run2()
    @info "Loading data"
    poollength = 10
    horizon = 15
    datalength = 500
    input, target = get_data(:solar, poollength, datalength, horizon) |> gpu

    @info "Creating model and loss"
    inputsize = size(input, 1)
    encodersize = 10
    decodersize = 10
    global model = DARNN(inputsize, encodersize, decodersize, poollength, 1) |> gpu

    function loss(x, y)
        # println("loss: ", typeof(x), "\n", typeof(y))
        Flux.reset!(model)
        return Flux.mse(model(x), y')
    end

    cb = function ()
        Flux.reset!(model)
        global pred = vec(model(input)' |> cpu)
        Flux.reset!(model)
        global tgt = collect(cpu(target))
        # println("pred ", typeof(pred), " | ", size(pred))
        # println("target ", typeof(target), " | ", size(target))
        p1 = draw(pred; label="Predict")
        display(p1)
        p1 = draw!(tgt; label="Data")#, title="Loss $(loss(input, target))")
        # display(plot(p1))
    end

    @info "Start loss" loss = loss(input, target)
    @info "Starting training"
    Flux.train!(loss, Flux.params(model), Iterators.repeated((input, target), 20), ADAM(0.007), cb=cb)
    # Flux.train!(loss, Flux.params(model), [(input, target)], ADAM(0.007), cb=cb)
    @info "Final loss" loss = loss(input, target)
end

end