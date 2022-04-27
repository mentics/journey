module OrderWatching
using Dates
using DateUtil, ThreadUtil, LogUtil

export startWatcher, restartWatcher, stopWatcher

const timer = Ref{Union{Nothing,Timer}}(nothing)
const handler = Ref{Union{Nothing,Function}}(nothing)
const lk = ReentrantLock()
const enabled = Ref{Bool}(true) # so we can disable for tests

function startWatcher()
    runSync(lk) do
        enabled[] && stopTimer()
        onTimer(nothing)
        enabled[] && startTimer()
    end
end

function restartWatcher()
    runSync(lk) do
        @log info "Restarting order watcher" CHECK_INTERVAL
        stopTimer()
        startTimer()
    end
end

function stopWatcher()
    runSync(lk) do
        stopTimer()
    end
end

#region Local
const DELAY = 5.0
const CHECK_INTERVAL = 60.0

setEnabled(en) = enabled[] = en

function startTimer()
    if !isnothing(timer[]) && isopen(timer[])
        @log debug "Already running: Orders watcher timer, checking each $(CHECK_INTERVAL) seconds after $(DELAY) seconds"
    else
        global timer[] = Timer(wrapErr(onTimer), DELAY; interval = CHECK_INTERVAL)
        @log info "Started: Orders watcher timer, checking each $(CHECK_INTERVAL) seconds after $(DELAY) seconds"
    end
end

function stopTimer()
    if isnothing(timer[])
        @log debug "Not running: Orders watcher timer"
    else
        close(timer[])
        global timer[] = nothing
        @log info "Stopped: Orders watcher timer"
    end
end

function onTimer(timer)
    runSync(lk) do
        if !handler[]()
            stopTimer()
        end
    end
end
#endregion

end