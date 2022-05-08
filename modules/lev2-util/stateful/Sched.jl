module Sched
using Dates
using Globals, ThreadUtil, CollUtil, LogUtil, OutputUtil
using Calendars
# TODO: should not depend directly on Calendars, more generic
# could have some generic thing that it called into for time important events: adding, each run, etc.

struct Job
    name::String
    mod::Module
    codeGen::Union{Expr,Symbol} # They might be just a function name, so symbol
    whenGen::Union{Expr,Symbol}
    runImmed::Bool
end
mutable struct JobRunning
    job::Job
    code::Function
    when::Function
    timeLast::DateTime
    timeNext::DateTime
end
getTimeNext(job::JobRunning) = job.timeNext
toTuple(job::JobRunning) = (;job.job.name, job.timeLast, job.timeNext, job.job.mod, job.job.codeGen, job.job.whenGen, job.job.runImmed)

replace(name::String, mod::Module, code::String, when::String, runImmed::Bool=false)::Nothing = ( remove(name) ; add(name, mod, code, when, runImmed) )
function add(name::String, mod::Module, code::String, when::String, runImmed::Bool=false; repok=false)::Nothing
    now(UTC) < nextMarketChange() || Calendars.updateCalendar()
    existing = find(j -> j.job.name == name, Jobs)
    if !isnothing(existing)
        repok || error("Tried to register job with same name ", name)
        if repok && runImmed
            existing.timeNext = now(UTC)
            wakeUp()
        end
        return
    end
    @log debug "Scheduling $(name)"
    exprCode = Meta.parse(code)
    exprWhen = Meta.parse(when)
    when = Base.eval(mod, exprWhen)
    job = JobRunning(Job(name, mod, exprCode, exprWhen, runImmed),
                     Base.eval(mod, exprCode), when, NEVER, runImmed ? now(UTC) : when(NEVER, isMarketOpen(), nextMarketChange()))

    runSync(Lock) do
        push!(Jobs, job)
        sort!(Jobs; by=getTimeNext)
        # @info "Checking new job times" job.timeNext getTimeNext(first(Jobs)) now(UTC)
        (now(UTC) - getTimeNext(first(Jobs)) <= maxSleep()) && ( @log debug "Waking up sched" ; wakeUp() )
    end
    return
end

function remove(name::String)::Bool
    runSync(Lock) do
        @log debug "Unscheduling $(name)"
        return del!(job -> job.job.name == name, Jobs)
    end
end

ison()::Bool = Running[]
stop()::Nothing = ( Stop[] = true ; wakeUp() ; while Running[]; sleep(0.01) end ; return )
function start()::Nothing
    !ison() || error("Timer already running")
    Stop[] = false
    spawnWrapped(run)
    return
end
restart()::Nothing = ( stop() ; start() ; return )
# list()::Vector{JobRunning} = Jobs
list() = pretyble(toTuple.(Jobs))
clearJobs()::Nothing = ( empty!(Jobs) ; return )

#region Local
const NEVER = DateTime(0)
const Jobs = Vector{JobRunning}()
const Lock = ReentrantLock()
const Running = Ref{Bool}(false)
const Stop = Ref{Bool}(false)
const waitCond = Threads.Condition()

function __init__()::Nothing
    Globals.has(:maxSchedPeriod) || Globals.set(:maxSchedPeriod, Second(2))
    return
end

maxSleep()::Period = Globals.get(:maxSchedPeriod)
wakeUp() = lockNotify(waitCond)

function run()::Nothing
    # TODO: can detect if woke up after sleep by storing a last ran time and compare to now
    try
        Running[] = true
        @log info "Sched thread has started"
        while true
            Stop[] && break
            next = NEVER
            while next < now(UTC)
                next = runSync(Lock) do
                    nowRun = now(UTC)
                    nowRun < nextMarketChange() || Calendars.updateCalendar()
                    # TODO: should we disable allow suspend during a run, only allow during sleep?
                    mktOpen = isMarketOpen()
                    mktChange = nextMarketChange()
                    @assert nowRun < mktChange
                    for job in Jobs
                        job.timeNext <= nowRun || break
                        spawnWrapped(runJob, job)
                        job.timeLast = nowRun
                        job.timeNext = job.when(nowRun, mktOpen, mktChange)
                        @assert job.timeNext > nowRun "timeNext > nowRun: $(job) > $(nowRun)"
                    end
                    sort!(Jobs; by=getTimeNext)
                    nextMax = round(nowRun + maxSleep(), maxSleep(), RoundDown)
                    return isempty(Jobs) ? nextMax : min(nextMax, first(Jobs).timeNext)
                end
            end
            nn = now(UTC)
            dur = next - nn
            dur > Millisecond(0) || @error "Sched has zero wait" next nn
            @log debug "Sched sleeping for $(dur)"
            sleepWait(waitCond, Millisecond(max(dur, Millisecond(0))).value/1000)
        end
        return
    finally
        @log info "Sched thread has stopped"
        Running[] = false
    end
end

runJob(job::JobRunning)::Nothing = ( job.code() ; return )
#endregion

end

#=================
using Dates

str = read("test.xml", String)
xml = parse_xml(str)
group = xml2["Triggers"]["CalendarTrigger"]
templTrig = group[1]
trigNew = deepcopy(templTrig)
trigNew["StartBoundary"]
2022-04-11T00:00:00
push!(group, templTrig)


2022-04-11T00:00:00

==============#