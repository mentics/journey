include("repl-inc.jl")

try
using LogUtil
LogUtil.init("C:/data/log/daily")
LogUtil.deleteAll()
catch
  # ignore
end

# Sleep 10 seconds in case this is the first time for the background scheduler to run backupOrders
sleep(10)

using Store, TimeZones
pathLatest = joinpath(dirOrderBackup(), sort!(readdir(dirOrderBackup(); sort=false); rev=true)[1])
tsBak = astimezone(TimeZones.unix2zdt(mtime(pathLatest)), localzone())

io = IOBuffer()
println(io, "Most recent bak order: $(tsBak)")
status = today() == Date(tsBak) ? "ok" : "WARNING"

using Emails
sendEmail("***REMOVED***", "Daily $(today()) report $(status)", String(take!(io)))
