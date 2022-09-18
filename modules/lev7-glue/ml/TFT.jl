module TFT
import Flux:Dropout,Dense,sigmoid,LayerNorm,@functor
import CollUtil
import LayerUtil:LU

# https://towardsdatascience.com/temporal-fusion-transformer-googles-model-for-interpretable-time-series-forecasting-5aa17beb621

# Gated Residual Network
struct GatedResidualNetwork{D1,D2,DROP}

end

# https://github.com/unit8co/darts/blob/master/darts/models/forecasting/tft_submodels.py#L276
struct GateAddNorm{GLU,AN}
    glu::GLU
    addNorm::AN
end
GateAddNorm(inputSize, hiddenSize, dropOut) = GateAddNorm(GatedLinearUnit(inputSize, hiddenSize, dropOut), AddNorm(hiddenSize))
@functor GateAddNorm
function (m::GateAddNorm)(x, skip)
    @assert
    x2 = m.glu(x)
    out = m.addNorm(x2, skip)
    return out
end
LU.getInputSize(m::GateAddNorm) = getInputSize(m.glu)
LU.getOutputSize(m::GateAddNorm) = getOutputSize(m.addNorm)

# https://github.com/unit8co/darts/blob/master/darts/models/forecasting/tft_submodels.py#L241
struct AddNorm{M,N}
    mask::M
    norm::N
end
# Other values not supported yet: , skipSize=inputSize, trainableAdd=true
AddNorm(inputSize) = AddNorm(zeros(inputSize), LayerNorm(inputSize))
@functor AddNorm
function (m::AddNorm)(x, skip)
    @assert size(x, 1) == size(m.mask, 1)
    @assert size(skip, 1) == size(m.mask, 1)
    skip2 = skip .* sigmoid.(m.mask) * 2.0
    out = m.norm(x + skip2)
    return out
end
LU.getInputSize(m::AddNorm) = ( sz = size(m.mask) ; (sz, sz) )
LU.getOutputSize(m::AddNorm) = ( sz = size(m.mask) ; (sz, sz) )

# Gated Linear Unit
# https://github.com/Rishit-dagli/GLU/blob/main/glu_tf/glu.py
# https://github.com/pytorch/pytorch/blob/8c9d7fabd60b7cbb84277d1db87e9a9c78fde266/torch/_refs/nn/functional/__init__.py#L618
struct GatedLinearUnit{DROP,D}
    drop::DROP
    dense::D
end
# GatedLinearUnit: inputSize => hiddenSize
GatedLinearUnit(dropRate, inputSize, hiddenSize) = GatedLinearUnit(Dropout(dropRate), Dense(inputSize, 2 * hiddenSize))
@functor GatedLinearUnit
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
LU.getInputSize(m::GatedLinearUnit) = getInputSize(m.dense)
LU.getOutputSize(m::GatedLinearUnit) = getOutputSize(m.dense)
function testGlu()
    glu = TFT.GatedLinearUnit(0.0, 8, 12)
    glu(rand(8))
    glu(rand(8, 17))
end

end