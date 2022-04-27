# run for SchedStrat:
#   julia --sysimage=C:/data/tmp/sysimage-journey.dll --threads=auto --project --load=repl-sched.jl
# G: & cd "G:/My Drive/sync/julia/journey" & julia --sysimage=C:/data/tmp/sysimage-journey.dll --project -O0 --compile=min --startup-file=no src/todo.jl

include("../repl-inc.jl")
try
using LogUtil
LogUtil.init("C:/data/log/sched")
LogUtil.deleteAll()
catch
  # ignore
end

using Todo
Todo.checkTodo()