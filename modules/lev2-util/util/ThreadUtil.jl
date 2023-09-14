module ThreadUtil
import Base.Threads:@spawn
import ThreadPools

export resetAtomics, runSync, wrapErr, spawnWrapped, bg
export sleepWait, lockNotify

mutable struct Atomic2{T}
    @atomic value::T
end
resetAtomics(ats::Atomic2{Int}...) = for at in ats; @atomic at.value = 0 end

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
# sync_output(f) = runSync(f, lock_output)
sync_output(out) = runSync(lock_output) do
    println(out)
    flush(stdout)
end
sync_err(out) = runSync(lock_output) do
    println(stderr, out)
    flush(stderr)
end
function show_exc(e, msg="")
    sync_err(msg * "\n" * sprint(showerror, e, catch_backtrace()) * "\n")
end

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

# function loop(f, xs, args...)::Bool
#     stop = Atomic2(false)
#     # ThreadPools.twith(ThreadPools.QueuePool(2, threadcount)) do pool
#         # ThreadPools.@tthreads pool for x in xs
#         # ThreadPools.@bthreads for x in xs
#         for x in xs
#             !(@atomic stop.value) || return true
#             thid = Threads.threadid()
#             try
#                 f(x, args...)
#             catch e
#                 @atomic stop.value = true
#                 if e isa InterruptException
#                     sync_output("Interrupted thid:$(thid)")
#                 else
#                     show_exc(e, "Exception in thid:$(thid) thrown for x:$(first(string(x), 1000)) and args:$(args)")
#                 end
#                 break
#             end
#         end
#     # end
#     return @atomic stop.value
# end

function loop(f, xs)::Bool
    stop = Atomic2(false)
    # ThreadPools.twith(ThreadPools.QueuePool(2, threadcount)) do pool
        # ThreadPools.@tthreads pool for x in xs
        ThreadPools.@bthreads for x in xs
        # for x in xs
            !(@atomic stop.value) || return true
            thid = Threads.threadid()
            try
                f(x)
            catch e
                @atomic stop.value = true
                if e isa InterruptException
                    sync_output("Interrupted thid:$(thid)")
                else
                    show_exc(e, "Exception in thid:$(thid) thrown for x:$(first(string(x), 1000)) and args:$(args)")
                end
                break
            end
        end
    # end
    return @atomic stop.value
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