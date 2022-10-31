@assert Threads.nthreads() >= 12
using Pkg
Pkg.activate(".")
include("../repl-all.jl")
exit()
