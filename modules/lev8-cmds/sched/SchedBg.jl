module SchedBg
using Dates
using Globals, LogUtil
using Sched, ProcSched, Snapshots
using Calendars, Expirations

Jobs = [
    ("update-$(Calendars)", Calendars, "updateCalendar", "whenUpdate", false),
    # ("run-$(Snapshots)", Snapshots, "snave", "whenUpdate", false),
    ("run-procExpired", ProcSched, "procExpired", "whenProcExpired", true),
    ("run-backupOrders", ProcSched, "backupOrders", "whenBackupOrders", true),
    # ("check-DevMode", @__MODULE__, "checkDevMode", "checkDevModeWhen", true)
]

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