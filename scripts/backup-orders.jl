include("inc.jl")

try
using LogUtil
LogUtil.init("C:/data/log/backup")
# LogUtil.deleteAll()
catch
  # ignore
end

import ProcSched
ProcSched.backupOrders()