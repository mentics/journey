module CudaUtil
import CUDA
import Flux

CUDA.allowscalar(false)

const DEV = CUDA.has_cuda() && (CUDA.runtime_version() >= v"11.2") ? Flux.gpu : Flux.cpu

# copyto_itr!(gbufs, data.prep_input(obss, bufs))
function copyto_itr!(gbufs, bufs)
    for (gb, b) in zip(gbufs, bufs)
        copyto!(gb, b)
    end
    return gbufs
end

end