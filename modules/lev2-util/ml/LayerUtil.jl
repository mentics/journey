module LayerUtil
import Flux:Dense

export LU
const LU = @__MODULE__

getInputSize(m::Dense) = size(m.weight, 1)
getOutputSize(m::Dense) = size(m.weight, 2)

end