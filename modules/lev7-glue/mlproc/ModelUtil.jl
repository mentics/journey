module ModelUtil
import Flux

export SplitLayer

struct SplitLayer{L,W}
    layers::L
    split::W
end
Flux.@functor SplitLayer

function (m::SplitLayer)(x)
    return (
        m.layers[1](x[1:m.split,:]),
        m.layers[2](x[(m.split+1):end,:])
    )
end

end