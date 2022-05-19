include("inc.jl")

try
using LogUtil
LogUtil.init("C:/data/log/daily")
LogUtil.deleteAll()
catch
  # ignore
end

# Sleep 90 seconds in case this is the first time for the background scheduler to run backupOrders and started at 1:59 (one minute before schedule)
# sleep(90)

# TODO: move to util
using Store, TimeZones
pathLatest = joinpath(dirOrderBackup(), sort!(readdir(dirOrderBackup(); sort=false); rev=true)[1])
tsBak = astimezone(TimeZones.unix2zdt(mtime(pathLatest)), localzone())

io = IOBuffer()
println(io, "Most recent bak order: $(tsBak)")
status = (now(localzone()) - tsBak) < Hour(24)  ? "ok" : "WARNING"

# That time must match SchedBg.whenBackupOrders
# if isAfterLocal(Time(14, 0))
#     status = today() == Date(tsBak) ? "ok" : "WARNING"
# else
# end

using Emails
sendEmail("***REMOVED***", "Daily $(today()) report $(status)", String(take!(io)))
