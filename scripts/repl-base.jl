using Revise

include("inc.jl")
include("logs.jl")

try
using LogUtil
LogUtil.init("C:/data/log")
LogUtil.deleteAll()
catch
  # ignore
end

# save = Dict()
# save[:dir] = 1
nothing