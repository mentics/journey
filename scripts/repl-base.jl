using Revise

include("inc.jl")

try
using LogUtil
LogUtil.init("C:/data/log")
LogUtil.deleteAll()
catch
  # ignore
end

save = Dict()
save[:dir] = 1