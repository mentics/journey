using Revise

include("repl-inc.jl")

try
using LogUtil
LogUtil.init("C:/data/log")
LogUtil.deleteAll()
catch
  # ignore
end
