module SchedBg
using Dates
using Globals, LogUtil, DateUtil
using Sched, ProcSched, Snapshots
using Calendars, Expirations

Jobs = [
    ("run-backupOrders", SchedBg, "ProcSched.backupOrders", "whenBackupOrders", true),
    ("run-procExpired", SchedBg, "ProcSched.procExpired", "whenProcExpired", true),
    ("run-snapshots", SchedBg, "Snapshots.snave", "whenSnapshots", false),
    # ("check-DevMode", @__MODULE__, "checkDevMode", "checkDevModeWhen", true)
]

# TODO: only run on weekdays?
whenBackupOrders(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = nextLocalTime(from, Time(14, 0))

whenProcExpired(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = nextLocalTime(from, Time(6, 15))

whenSnapshots(from::DateTime, isMktOpen::Bool, tsMktChange::DateTime) = nextMarketPeriod(Hour(1), Second(73), Second(5))

whenStrat(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = nextMarketPeriod()
whenMarket(from, isMktOpen, nextMktChange, PERIOD_UPDATE ÷ length(UseExps))

function nextMarketPeriod(period::Period, before::Period, afterOpen::Period)
    if isMktOpen
        nextHour = round(from, PERIOD, RoundUp)
        timeNext = nextHour - BEFORE
        timeNext < tsMktChange && return timeNext
        @log error "Next snapshot time after market when during market"
    end
    return tsMktChange + Minute(7)
end

function start()
    for job in Jobs
        Sched.add(job...)
    end
end

function stop()
    for job in Jobs
        Sched.remove(job[1])
    end
end

restart() = ( stop() ; start() )

devModeChecks = [:devoffRunLast, :anRunLast, :soRunLast, :ctRunLast, :todoRunLast]
function checkDevMode()
    @log debug "checkDevMode"
    devon() || return
    recent = maximum(devModeChecks) do sym
        Globals.has(sym) ? Globals.get(sym) : Sched.NEVER
    end
    if (now(UTC) - recent) > Minute(10)
        @info "Turning on dev mode due to inactivity"
        devon()
    end
    return
end
checkDevModeWhen(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = isMktOpen ? from + Minute(1) : nextMktChange + Second(7)

end