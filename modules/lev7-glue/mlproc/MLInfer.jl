module MLInfer
using ModelUtil

params_infer() = (;)

function setup(trainee::Trainee, params=params_infer())
    model = trainee.make_model() |> dev
    println("Model has param count: ", sum(length, Flux.params(model)))
    ModelUtil.load_infer(model, trainee.get_inference_model(model), trainee)
    return model
end

end