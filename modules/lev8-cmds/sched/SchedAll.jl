module SchedAll
using Sched, SchedLive, SchedBg

function start()
    SchedBg.start()
    SchedLive.start()
    Sched.restart()
end

function stop()
    SchedBg.stop()
    SchedLive.stop()
end

restart() = ( stop() ; start() )

end