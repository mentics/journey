module SchedStrat
using Dates
using StatusTypes
using SH, Globals, LogUtil, OutputUtil, CollUtil
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
SimpleExps = [1,2,3,4,5,6,7,8]
UseExps = copy(SimpleExps)

MinEvrs = Dict{Int,Float64}() # n => .01 for n in 1:8)

function run()
    if isMarketOpen()
        setAllowSuspend(false)
    else
        setAllowSuspend(true)
        @logret "SchedStrat ran when market closed"
    end
    urpon()
    useExps = Globals.has(:anasExpsOver) ? Globals.get(:anasExpsOver) : SimpleExps
    # global UseExps = (!Globals.has(:anasAvoid) || Globals.get(:anasAvoid)) ? useExps[updateInds(useExps)] : useExps
    global UseExps = useExps[updateInds(useExps)]
    # TODO: it should call into a service level module and not a command, so extract it
    exs = nextExps(UseExps)
    !isnothing(exs) || return
    @log info "Running scheduled analysis" exs UseExps
    # TODO: scorer just for it
    ana(exs...; headless=true, nthreads=(Threads.nthreads()-2)) # , filt=isCall
    res = CmdStrats.analysisResults()
    if !isempty(res)
        # if haskey(MinEvrs, exs[1])
        #     @info "Checking result:" exs[1] MinEvrs[exs[1]] (res[1].evr >= MinEvrs[exs[1]])
        # end
        !haskey(MinEvrs, exs[1]) || res[1].evr >= MinEvrs[exs[1]] || return
        io = IOBuffer()
        pretyble(io, res; rowcol=true, widths=CmdStrats.tupleWidths())
        # TODO: include output of comp(1)
        sendEmail("***REMOVED***", "Found potential entry for $(exs)", String(take!(io)))
    end
    return
end

function nextExps(useExps)
    !isempty(useExps) || return nothing
    global Index[] = mod1(Index[] + 1, length(useExps))
    return useExps[Index[]]
end

function updateInds(useExps)
    newActive = queryEntered(today(), Starting)
    exAvoid = map(row -> searchsortedfirst(expirs(), row.targetdate), newActive)
    inds = setdiff(eachindex(useExps), exAvoid)
    Hour(4) < nextMarketChange() - now(UTC) || del!(inds, 1) # Don't keep looking for ex1 after early morning
    return Globals.set(:anasExps, collect(inds))
end
#endregion

end