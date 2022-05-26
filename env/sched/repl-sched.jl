# run for SchedStrat:
# cd /c/data/julia/journey/env/sched/ ; julia --sysimage=C:/data/tmp/sysimage-journey.dll --threads=auto --project --load=repl-sched.jl

CODE_DIR = "../.."
include("../../scripts/inc.jl")
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
