module SchedStrat
using Dates
using Globals, LogUtil, OutputUtil
using Emails
using Calendars, Markets, Expirations
using StoreTrade
using CmdStrats
using SystemUtil

#region Local
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
    (8,9,10)
]
UseExps = copy(DefaultExps)

function run()
    if isMarketOpen()
        setAllowSuspend(false)
    else
        setAllowSuspend(true)
        @logret "SchedStrat ran when market closed"
    end
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