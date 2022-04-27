module SchedStrat
using Dates
using Globals, LogUtil, OutputUtil
using Sched, Emails, DataHelper
using Calendars, Markets
using CmdStrats
using SchedBg

# To be used with repl-sched.jl

function start()
    @log info "Scheduling analysis"
    Sched.add(JOB_NAME, @__MODULE__, "run", "whenUpdate", false)
    # TODO: Make sure the Calendars update is scheduled
    @assert SchedBg.Jobs[1][1] == "update-$(Calendars)"
    Sched.add(SchedBg.Jobs[1]...)
    Sched.ison() || Sched.start()
end

function stop()
    @log info "Unscheduling analysis"
    Sched.remove(JOB_NAME)
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
whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = whenMarket(from, isMktOpen, nextMktChange, PERIOD_UPDATE ÷ length(UseExps))

function run()
    isMarketOpen() || ( (@log info "SchedStrat stopping because market closed") ; stop() ; return )
    urpon()
    Globals.has(:anasExps) && (global UseExps = DefaultExps[Globals.get(:anasExps)])
    @log info "Running analysis for exps $(UseExps)"
    # TODO: it should call into a service level module and not a command, so extract it
    exps = nextExps()
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
#endregion

end