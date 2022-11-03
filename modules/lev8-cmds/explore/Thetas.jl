module Thetas
using Dates
using SH, Globals, BaseTypes, LegMetaTypes, ChainTypes
using DateUtil, OptionUtil, OutputUtil, LegUtil
using Quoting, Between, SnapUtil
using Snapshots, Chains, Markets, Expirations
using Joe

export th
const th = @__MODULE__

function xpirBefore(day::Date)::Date
    # snap(bdaysBefore(day, 10), 0, 0)
    xpirs = SnapUtil.snapExpirs()
    i = findfirst(i -> xpirs[i+1] > day, eachindex(xpirs))
    return xpirs[i]
end

function setup(bdaysBack=20, xpir=xpirBefore(today()))
    global Xpir = xpir
    snap(StartDate, 1)
    global Xpr = xp.whichExpir(xpir)
    global BdaysBack = bdaysBack
    global StartDate = bdaysBefore(Xpir, bdaysBack)
    jorn(Xpr; all=true)
    global legs = getLeg.(Joe.ress[Xpr][1].lms)
end

function test(bdaysBack=20, xpir=xpirBefore(today()))
    (isdefined(@__MODULE__, :BdaysBack) && BdaysBack == bdaysBack && Xpir == xpir) || setup(bdaysBack, xpir)

    rows = NamedTuple[]
    understart = missing
    for ago in BdaysBack:-1:0
        println("snapping to ", bdaysBefore(Xpir, ago))
        date = bdaysBefore(Xpir, ago)
        SnapUtil.snapExists(date) || continue
        snap(date, 1)
        push!(rows, (;procDay(understart)...))
    end
    snap(SnapUtil.lastSnap(Xpir))
    push!(rows, (;procDay(understart)...))
    pretyble(rows)
end

function procDay(understart)
    row = Vector{Tuple{Symbol,Any}}()
    push!(row, (:time, toTimeLocal(market().tsMarket)))
    curp = market().curp
    push!(row, (:curp, curp))
    !ismissing(understart) || ( understart = curp )
    push!(row, (:dcurp, 100.0 * (curp / understart - 1.0)))

    lms = tos(LegMeta, legs, optQuoter)
    push!(row, (:bap, bap(lms)))

    thetaSum = 0.0
    xtrinSum = 0.0
    for (i, lm) in enumerate(lms)
        theta = getThetaDir(lm)
        thetaSum += theta
        push!(row, (Symbol(:theta, i), theta))
        xtrin = extrinsDir(lm, curp)
        xtrinSum += xtrin
        push!(row, (Symbol(:xtrin, i), xtrin))
    end
    push!(row, (:thetaSum, thetaSum))
    push!(row, (:xtrinSum, xtrinSum))

    # thetas = getTheta.(lms)
    # println(thetas, ' ', sum(thetas))
    # xtrs = extrins.(lms, curp)
    # println(xtrs, ' ', sum(xtrs))
    return row
end

function aprSched(targRate=0.5, risk=1.0)
    curVals = map(1:100) do i
        timult = bdaysPerYear / (1 + i)
        curVal = risk * targRate / timult
        return curVal
    end
end

end