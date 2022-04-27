basePath = joinpath(homedir(), ".julia", "config")
mkpath(basePath)
open(joinpath(basePath, "startup.jl"), "w") do io
    write(io, "push!(LOAD_PATH, \"./pkgs\")")
end
import Pkg
Pkg.instantiate()
import TimeZones
TimeZones.build()
