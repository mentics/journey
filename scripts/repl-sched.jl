# run for SchedStrat:
# cd /c/data/julia/journey/ ;
# julia --sysimage=C:/data/tmp/sysimage-journey.dll --threads=auto --project --load=scripts/repl-sched.jl

# CODE_DIR = "../.."
# include("../../scripts/inc.jl")
include("inc.jl")
try
using LogUtil
LogUtil.init("C:/data/log/sched")
LogUtil.deleteAll()
catch
  # ignore
end

using Calendars

using Globals
using Markets
Markets.urpon()
setvr(.7)

using Sched, SchedBg
SchedBg.add()
Sched.start()

using SchedStrat
ss = SchedStrat

using DateUtil
using Snapshots

using SystemUtil
setAllowSuspend(false)