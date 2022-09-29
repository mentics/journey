@assert Threads.nthreads() >= 12
using Pkg
Pkg.activate(".")
include("../repl-ml.jl")
include("../repl-trade.jl")
exit()
