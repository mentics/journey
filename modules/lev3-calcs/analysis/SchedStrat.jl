module SchedStrat
using Dates
using Globals, LogUtil, OutputUtil
using Sched, Emails, DataHelper
using Calendars, Markets, Expirations
using StoreTrade
using CmdStrats
using SchedBg
using SystemUtil

# To be used with repl-sched.jl

function start()
    inds = updateInds()
    @log info "Scheduling analysis" inds
    Sched.add(JOB_NAME, @__MODULE__, "run", "whenUpdate", false)
    # TODO: Make sure the Calendars update is scheduled
    @assert SchedBg.Jobs[1][1] == "update-$(Calendars)"
    Sched.add(SchedBg.Jobs[1]...)
    Sched.ison() || Sched.start()
    setAllowSuspend(false)
end

function stop()
    @log info "Unscheduling analysis"
    Sched.remove(JOB_NAME)
    setAllowSuspend(true)
end

#region Local
const JOB_NAME = "run-$(SchedStrat)"
const PERIOD_UPDATE = Second(Minute(20))
const Index = Ref{Int}(0)
# TODO: consts?
DefaultExps = [
    (1,2,3),
    (2,3,4),
    (3,4,5),
    (4,5,6),
    (5,6,7),
    (6,7,8),
    (7,8,9),
    # (8,9,10)
]
UseExps = copy(DefaultExps)

function run()
    isMarketOpen() || ( (@log info "SchedStrat stopping because market closed") ; stop() ; return )
    urpon()
    inds = updateInds() # Globals.get(:anasExps)
    Globals.has(:anasExps) && (global UseExps = DefaultExps[inds])
    # TODO: it should call into a service level module and not a command, so extract it
    exps = nextExps()
    @log info "Running scheduled analysis" exps UseExps
    # TODO: scorer just for it
    ana(exps...; headless=true, nthreads=(Threads.nthreads()-2))
    res = CmdStrats.analysisResults()
    if !isempty(res)
        io = IOBuffer()
        pretyble(io, res; rowcol=true, widths=CmdStrats.tupleWidths())
        # TODO: include output of comp(1)
        sendEmail("***REMOVED***", "Found potential entry for $(exps)", String(take!(io)))
    end
end

nextExps() = ( Index[] = mod1(Index[] + 1, length(UseExps)); UseExps[Index[]] )

function updateInds()
    exAvoid = map(row -> searchsortedfirst(expirs(), row.targetdate), queryEntered(today()))
    inds = setdiff(eachindex(DefaultExps), exAvoid)
    return Globals.set(:anasExps, collect(inds))
end
#endregion

end