module ThreadUtil
import Base.Threads:@spawn
import ThreadPools

export Atomic, resetAtomics, runSync, wrapErr, spawnWrapped, bg
export sleepWait, lockNotify

# TODO: this is built in now or something?
mutable struct Atomic{T}
    @atomic count::T
end
resetAtomics(ats::Atomic{Int}...) = for at in ats; @atomic at.count = 0 end

# TODO: consolidate (syncOut in RunStats) and put somewhere?
function runSync(f, lk, args...)
    lock(lk)
    try
        return f(args...)
    finally
        unlock(lk)
    end
end

export sync_output
const lock_output = ReentrantLock()
sync_output(f) = runSync(f, lock_output)

function wrapErr(f)
    function(args...)
        try
            f(args...)
        catch e
            if e isa InterruptException
                rethrow(e)
            else
                showerror(stderr, e, catch_backtrace()) ; println(stderr)
            end
        end
    end
end

function spawnWrapped(f, args...)
    f2 = wrapErr(f)
    Threads.@spawn f2(args...)
end

function lockNotify(cond::Threads.Condition)
    lock(cond)
    try
        notify(cond)
    finally
        unlock(cond)
    end
end

using Dates # TODO: remove after testing
function sleepWait(cond::Threads.Condition, seconds::Float64)
    lock(cond)
    try
        tim = Timer(tim -> lockNotify(cond), seconds)
        # before = now(UTC)
        wait(cond)
        # after = now(UTC)
        # isopen(tim) && ( @info "interrupted sleep wait" (after - before) seconds stacktrace() ; close(tim) )
        # isopen(tim) && close(tim)
        close(tim)
    finally
        unlock(cond)
    end
end

# TODO: maybe can simplify with things I did above
function bg(f)
    t = wrapErr(f)
    cond = Threads.Condition()
    global running[] = @spawn notifyDone(t, cond)
    @spawn begin
        runSync(cond) do
            wait(cond)
        end
        running[] = nothing
        @info "bg task done"
        # whenDone()
    end
end

function loop(f, xs, args...)
    stop = false
    ThreadPools.twith(ThreadPools.QueuePool(2, 11)) do pool
        ThreadPools.@tthreads pool for x in xs
            !stop || return
            thid = Threads.threadid()
            try
                f(x, args...)
                yield()
            catch e
                stop = true
                println("Exception in thid:$(thid) thrown for x:$(x) and args:$(args)")
                rethrow(e)
            end
        end
    end
    return
end

#region Local
const running = Ref{Union{Nothing,Task}}()

function notifyDone(toRun, cond::Threads.Condition)
    toRun()
    runSync(cond) do
        notify(cond)
    end
end
#endregion

end