module TFT
import Flux:Dropout,Dense,sigmoid
import CollUtil

# https://towardsdatascience.com/temporal-fusion-transformer-googles-model-for-interpretable-time-series-forecasting-5aa17beb621

# Gated Residual Network
struct GatedResidualNetwork{D1,D2,DROP}

end

# https://github.com/unit8co/darts/blob/master/darts/models/forecasting/tft_submodels.py#L276
struct GateAddNorm{GLU,AN}
    glu::GLU
    addNorm::AN
end
GateAddNorm(inputSize, hiddenSize, skipSize, dropOut) = GateAddNorm()
function (m::GateAddNorm)(x)

end

struct AddNorm{}

end
AddNorm(inputSize, skipSize=inputSize, trainableAdd=true) = AddNorm()
function (m::AddNorm)(x)
end

# Gated Linear Unit
# https://github.com/Rishit-dagli/GLU/blob/main/glu_tf/glu.py
# https://github.com/pytorch/pytorch/blob/8c9d7fabd60b7cbb84277d1db87e9a9c78fde266/torch/_refs/nn/functional/__init__.py#L618
struct GatedLinearUnit{DROP,D}
    drop::DROP
    dense::D
end
# GatedLinearUnit: inputSize => hiddenSize
GatedLinearUnit(dropRate, inputSize, hiddenSize) = GatedLinearUnit(Dropout(dropRate), Dense(inputSize, 2 * hiddenSize))
function (m::GatedLinearUnit)(x)
    DoubleSize = CollUtil.tupSetFirst(size(x), length(m.dense.bias))
    println(DoubleSize)
    OutputSize = CollUtil.tupSetFirst(DoubleSize, DoubleSize[1] ÷ 2)
    x2 = m.drop(x)
    @assert size(x2) == size(x)
    x3 = m.dense(x)
    @assert size(x3) == DoubleSize (size(x3), DoubleSize)
    # TODO: Is it right to on first dim when inputs could have variuos dims? But dense is doubling first dim, so... maybe that's right. We could pass in a dim as param and use that?
    len = size(x3)[1]
    @assert iseven(len)
    split = len ÷ 2
    x4 = selectdim(x3, 1, 1:split)
    @assert size(x4) == OutputSize (size(x4), OutputSize, len, split)
    gate = sigmoid(selectdim(x3, 1, split+1:len))
    @assert size(gate) == OutputSize
    x5 = x4 .* gate
    @assert size(x5) == OutputSize
    return x5
end
function testGlu()
    glu = TFT.GatedLinearUnit(0.0, 8, 12)
    glu(rand(8))
    glu(rand(8, 17))
end

end