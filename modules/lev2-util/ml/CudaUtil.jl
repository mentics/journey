module CudaUtil
import CUDA
import Flux

CUDA.allowscalar(false)

export DEV

const DEV = CUDA.has_cuda() && (CUDA.version() >= v"11.2") ? Flux.gpu : Flux.cpu

end