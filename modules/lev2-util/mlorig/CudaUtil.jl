module CudaUtil
import CUDA
import Flux

CUDA.allowscalar(false)

const DEV = CUDA.has_cuda() && (CUDA.runtime_version() >= v"11.2") ? Flux.gpu : Flux.cpu

end