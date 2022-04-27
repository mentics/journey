# run for SchedStrat:
#   julia --sysimage=C:/data/tmp/sysimage-journey.dll --threads=auto --project --load=repl-sched.jl

include("repl-inc.jl")
try
using LogUtil
LogUtil.init("C:/data/log/sched")
LogUtil.deleteAll()
catch
  # ignore
end

using Globals
using Markets
urpon()

using Sched, SchedStrat
# SchedStrat.start()