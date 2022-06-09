module SchedBg
using Dates
using Globals, LogUtil, DateUtil
using Sched, Calendars
import ProcSched, Snapshots, SchedStrat

# To be used with repl-sched.jl

# TODO: make const?
Jobs = [
    ("run-backupOrders", SchedBg, "ProcSched.backupOrders", "whenBackupOrders", true),
    ("run-procExpired", SchedBg, "ProcSched.procExpired", "whenProcExpired", true),
    ("run-snapshots", SchedBg, "snaveWrapper", "whenSnapshots", false),
    ("run-strat", SchedBg, "stratWrapper", "whenStrat", false),
]

# TODO: only run on weekdays?
whenBackupOrders(from::DateTime, isMktOpen::Bool, tsMktChange::DateTime) = nextLocalTime(from, Time(14, 0))

whenProcExpired(from::DateTime, isMktOpen::Bool, tsMktChange::DateTime) = nextLocalTime(from, Time(6, 15))

whenSnapshots(from::DateTime, isMktOpen::Bool, tsMktChange::DateTime) = nextMarketPeriod(from, isMktOpen, tsMktChange, Hour(1), Second(73), Second(45))
snaveWrapper() = isMarketOpen() && Snapshots.snave()

whenStrat(from::DateTime, isMktOpen::Bool, tsMktChange::DateTime) = nextMarketPeriod(from, isMktOpen, tsMktChange, Second(300) ÷ max(1, length(SchedStrat.UseExps)), Second(0), Minute(4))
stratWrapper() = isMarketOpen() && SchedStrat.run()

function add()
    for job in Jobs
        Sched.add(job...)
    end
end

function remove()
    for job in Jobs
        Sched.remove(job[1])
    end
end

end