using Revise

include("inc.jl")
# include("logs.jl")

try
using LogUtil
LogUtil.init("C:/data/log")
LogUtil.deleteAll()
catch
  # ignore
end

# save = Dict()
# save[:dir] = 1

# ENV["PATH"] = "D:\\app\\dev\\jdk11\\bin;" * ENV["PATH"]
# ENV["JAVA_HOME"] = "D:\\app\\dev\\jdk11"
# not on windows: ENV["JULIA_COPY_STACKS"] = 1
nothing