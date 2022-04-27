module SchedLive
using Sched
using Expirations, Markets, Chains

# TODO: make const?
Jobs = [
    ("update-$(Expirations)", Expirations, "update", "whenUpdate", true),
    ("update-$(Markets)", Markets, "update", "whenUpdate", true),
    ("update-$(Chains)", Chains, "update", "whenUpdate", true)
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

end