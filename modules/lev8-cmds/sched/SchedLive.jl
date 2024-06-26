module SchedLive
# using LogUtil
using Sched
using Expirations, Markets, Chains

# TODO: make const?
Jobs = [
    ("update-$(Expirations)", Expirations, "update", "whenUpdate", true),
    ("update-$(Markets)", Markets, "update", "whenUpdate", true),
    ("update-$(Chains)", Chains, "update", "whenUpdate", true)
    # ("check-DevMode", @__MODULE__, "checkDevMode", "checkDevModeWhen", true)
]

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

# devModeChecks = [:devoffRunLast, :anRunLast, :soRunLast, :ctRunLast, :todoRunLast]
# function checkDevMode()
#     @log debug "checkDevMode"
#     devon() || return
#     recent = maximum(devModeChecks) do sym
#         Globals.has(sym) ? Globals.get(sym) : Sched.NEVER
#     end
#     if (now(UTC) - recent) > Minute(10)
#         @info "Turning on dev mode due to inactivity"
#         devon()
#     end
#     return
# end
# checkDevModeWhen(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = isMktOpen ? from + Minute(1) : nextMktChange + Second(7)

end