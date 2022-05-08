# run for SchedStrat:
# cd /c/data/julia/journey ; julia --sysimage=C:/data/tmp/sysimage-journey.dll --threads=auto --project --load=scripts/repl-sched.jl

include("inc.jl")
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
setvr(.7)

using Sched, SchedBg
SchedBg.add()
Sched.start()
