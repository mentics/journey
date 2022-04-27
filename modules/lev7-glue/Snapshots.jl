module Snapshots
using Dates
using Globals, DateUtil, FileUtil, TradierConfig, LogUtil
using Positions, Chains, Expirations, Markets, ProbHist
using Sched

export snave, snop

# snap() = isnothing(getSnap()) ? nothing : Dates.format(tims(market().ts), DFTEST)
Globals.snap(num::Int) = snap(findByIndex(num))
Globals.snap(num1::Int, num2::Int, num::Int...) = snap(findByParts(num1, num2, num...))
Globals.snap(nam::AbstractString) = useSnap(nam)
snop() = stopSnap()
snave() = saveSnap()

#region Local
const DFTEST = dateformat"yyyy-mm-dd.HH-MM"

__init__() = Globals.set(:snap, nothing)

const toRecord = [
    ()->positions(; age=Millisecond(0)),
    ()->expirs(; up=true),
    ()->chains(; up=true),
    ()->market(; up=true)
]

function saveSnap()
    if dev()
        error("Don't snave when in devMode")
    end
    if !isnothing(snap())
        error("Save Snap: Don't snave when snapped")
        return
    end
    name = newName()
    try
        setRecording(name)
        @log info "Snave: Saving test data" name
        updateAll()
        @info "Snaved" name
    catch e
        @warn "WARNING: Snave: Snap might be in an inconsistent state" name
        rethrow(e)
    finally
        setRecording(nothing)
    end
    return
end

function useSnap(nam::AbstractString)
    Sched.stop()
    devon()
    if snap() != nam
        try
            Globals.set(:snap, nam)
            @info "Use Snap: Loading saved data" nam
            updateAll()
            ProbHist.probHists(;up=true)
        catch e
            @warn "WARNING: Use Snap: Snap might be in an inconsistent state" nam
            rethrow(e)
        end
    else
        println("Use Snap: Already using that snap ", nam)
    end
    return snap()
end

function stopSnap()
    Globals.set(:snap, nothing)
    try
        @info "Stop Snap: Loading live data"
        updateAll()
        ProbHist.probHists(;up=true)
    catch e
        @warn "WARNING: Stop Snap: Snap might be in an inconsistent state"
        rethrow(e)
    end
    Sched.restart()
    return snap()
end

function updateAll()
    for f in toRecord
        f()
    end
    return
end

newName() = formatLocal(now(UTC), DFTEST)

# const REGEX_NAME = r"(\d\d\d\d-\d\d-\d\d\.\d\d-\d\d)"
findByIndex(num::Int)::String = sort(readdir(TradierConfig.SnavePath; join=false, sort=false); rev=true)[num]
function findByParts(nums::Int...)
    files = sort(readdir(TradierConfig.SnavePath; join=false, sort=false); rev=true)
    p = vcat(fill("", 5-length(nums)), map(n -> n == 0 ? "" : string(n), nums)...)
    for file in files
        rx = ".*$(p[1]).*-.*$(p[2]).*-.*$(p[3]).*\\..*$(p[4]).*-.*$(p[5]).*"
        println(p, rx)
        if !isnothing(match(Regex(rx), file))
            return file
        end
    end
end
#endregion

end
