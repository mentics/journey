module SpreadPricing
using Dates, ThreadPools, BlackBoxOptim, EnergyStatistics, GLMakie
using SH, BaseTypes, SmallTypes, ChainTypes, OptionTypes, QuoteTypes, OptionMetaTypes
using DateUtil, FileUtil, OptionUtil, PricingUtil, MathUtil
using Calendars, Snapshots

#region New Stuff

#==
filter(sp.itemsForCorrel()) do item
    (getMarketClose(getExpiration(item.oq2)) - item.ts) < Hour(2)
end
48
==#

# filter(xs) do x
#     # x[2] == 0.0 && x[1] == 0.0 && x[4] == 0.0 && x[5] == 0.0 && x[6] == 0.0 &&
#     x[3] < 7200.0
# end

# rand() had dcor of .0129149, so... even .1 is much higher than that.

# runCorrelBasic() = calcCorrel(makeDataPrepped(makeIndepBasic, itemsForCorrel())...)
# runCorrelChosenMultis() = calcCorrel(makeDataPrepped(multis ∘ makeIndepChosen, itemsForCorrel())...)

# function calcCorrel(xsmat, ys)
#     global DCORES = tmap(1:size(xsmat, 2)) do i
#         res = dcor(xsmat[:,i], ys)
#         println("completed ", i, " on ", Threads.threadid())
#         return res
#     end
#     return DCORES
# end

# # These are the highest variation of each var after running through calcCorrel
# function makeIndepChosen(xs)
#     tex = log0(calcTex(xs, fill(1.0, 5), 1))
#     curpRat = xs[8]^2
#     openCloseRat = xs[9]^2
#     vixOpen = log0(xs[10])
#     strikeRat = xs[7]
#     return [tex, curpRat, openCloseRat, vixOpen, strikeRat]
# end


#region Setup data
function itemsForCorrel(num=5000)
    filtered = filter(PricingUtil.AllSpreadCalls) do item
        instr = getStrike(item.oq2)
        return instr < item.curp && (instr / item.curp) > .9
        # &&
        #                 (getMarketClose(getExpiration(item.oq2)) - item.ts) < Hour(2)
    end
    return filtered[1:div(length(filtered),num):end]
end

function makeRows(items=itemsForCorrel())
    xs, ys = makeData(items)
    global XMAT = reduce(hcat, xs)'
    global YS = ys
    global YS_DM = EnergyStatistics.dcenter!(EnergyStatistics.DistanceMatrix(ys))
    return
end

function listVars(xmat=XMAT)
    strikeRat = xmat[:,7]
    tex = calcTex(xmat)
    vixOpen = xmat[:,10]
    curpRat = xmat[:,8]
    openCloseRat = xmat[:,9]
    # combined1 = varia(xs[9] + xs[9]) # no improvement
    # combined2 = varia(xs[9] + xs[9] + xs[10]) # no improvement

    ts = @. unix2datetime(xmat[:,12])
    sso = @. Dates.value(round(ts - getMarketOpen(toDateMarket(ts)), Millisecond)) / 1000.0 # log0 had dcor of almost 0.1
    dow = @. dayofweek(toDateMarket(ts)) # TODO: max dcor around 0.031 so ignore, but not sure if that was with using mid for net spread
    return hcat(strikeRat, tex, vixOpen, curpRat, openCloseRat, sso, dow)
end

makeIndepBasic(xmat=XMAT) = ( global INDEP_BASIC = listVars(xmat) ; global INDEP_VARIA = map(varia, eachcol(INDEP_BASIC)) )

setup() = ( readSpreads() ; makeRows() ; makeIndepBasic() )
#endregion

#region Figure out Tex params to hardcode
function optTex()
    isdefined(@__MODULE__, :FIT_TEX_CAND) || ( FIT_TEX_CAND = fill(1.0, 4) )
    mTex = XMAT[:,1:6]
    fit(vP) = 1.0 - dcorys(mTex, [vP[1], vP[2], 1.0, vP[3], vP[4], vP[4]])
    global FIT_TEX = bboptimize(fit, FIT_TEX_CAND; SearchRange=fill((0.0, 1.0), 4), MaxTime=2000, NThreads=(Threads.nthreads()-1))
    global FIT_TEX_CAND = best_candidate(FIT_TEX)
end
#endregion

# Optimize vars
function dcorys(mX, vP)
    dcorX = EnergyStatistics.dcenter!(EnergyStatistics.DistanceMatrix(mX * vP))
    return dcor(YS_DM, dcorX)
end
function dcorys(vX)
    dcorX = EnergyStatistics.dcenter!(EnergyStatistics.DistanceMatrix(vX))
    return dcor(YS_DM, dcorX)
end

fitVar(i::Int) = vP -> 1.0 - dcorys(INDEP_VARIA[i], vP)

function optVar(i::Int)
    # global OPT_VAR_CANDS = fill(fill(1.0, VARIA_LEN), size(INDEP_BASIC, 2))
    isdefined(@__MODULE__, :OPT_VAR_CANDS) || ( global OPT_VAR_CANDS = fill(fill(1.0, VARIA_LEN), size(INDEP_BASIC, 2)) )
    global OPT_VAR = bboptimize(fitVar(i), OPT_VAR_CANDS[i]; SearchRange=fill((0.0, 1.0), 4), MaxTime=100, NThreads=(Threads.nthreads()-1))
    global OPT_VAR_CANDS[i] = best_candidate(OPT_VAR)
end
#===
1: Best candidate found: [0.00648858, 0.197154, 0.0422695, 0.812298]  Fitness: 0.181933623
2: Best candidate found: [0.581062, 0.421032, 0.406703, 0.00130458]  Fitness: 0.659881137
3: Best candidate found: [0.7281, 0.358628, 0.486315, 0.0030583]  Fitness: 0.778708830
4: Best candidate found: [0.0373444, 0.0142841, 0.712274, 0.000146941]  Fitness: 0.934776386
5: Best candidate found: [0.0516066, 0.0558537, 0.923618, 0.0412013]  Fitness: 0.924944455
6: Best candidate found: [0.899857, 0.830114, 0.144859, 0.0118619]  Fitness: 0.942626928
7: Best candidate found: [0.0906754, 0.818899, 0.750524, 0.00114109]  Fitness: 0.968377166
===#

function optCombos()
    m = INDEP_BASIC[:,1:5]
    global COMBO_CORR = [(dcorys(m[:,i1] .* m[:,i2]), i1, i2) for (i1, i2) in indCombos(size(m, 2), 2)]
end
#===
10-element Vector{Tuple{Float64, Int64, Int64}}:
 (0.3795206211325236, 1, 2)
 (0.7458853693457844, 1, 3)
 (0.24746063195219997, 1, 4)
 (0.30124521644077723, 1, 5)
 (0.36616169368030244, 2, 3)
 (0.1599694290986328, 2, 4)
 (0.18949201529427062, 2, 5)
 (0.06460564486550824, 3, 4)
 (0.08183839492401228, 3, 5)
 (0.07360409888482199, 4, 5)
===#

# Now show a graph of each (and multiples?) against result to try to glean some shape to guess which curve to use to fit
drawVar(i::Int, makeVars=makeIndepChosen) = drawVar(makeDataPrepped(makeVars, itemsForCorrel())..., i)
function drawVar(xsmat, ys, i)
    scatter(xsmat[:,i], ys)
end
#endregion


#region Header
const PandaInit = Ref{Vector{Float64}}(Vector{Float64}())
#endregion

#region Results
xAxis(row) = row[7]
function drawStrice(filt=(x->true))
    xs, ys = filter(filt, runData())
    pts = collect(zip([xAxis(x) for x in xs], ys))
    display(scatter(pts))
    if !isempty(PandaInit[])
        modelPts = [(xAxis(x), model(x, PandaInit[])) for x in xs]
        display(scatter!(modelPts))
    end
end

function drawTexPrice(filt=(x->true))
    data, extrin = filter(filt, runData())
    texs::Vector{Float64} = calcTex.(data, PandaInit, 1)
    pts::Vector{NTuple{2,Float64}} = collect(zip(texs, extrin))
    display(scatter(pts))
    modelPts::Vector{NTuple{2,Float64}} = [(texs[i], model(data[i], PandaInit[])) for i in eachindex(data)]
    display(scatter!(modelPts))
end

baseStrike(strikeDist) = [414.995, strikeDist, 27.5, 633300.0, 43200.0, 117120.0, 70500.0, 156.0, 414.995/(414.995+strikeDist)]
function drawModelStrike(p=best_candidate(BbCallsItm))
    scatter([(x, model(baseStrike(x), p)) for x in 0.0:-1.0:-250.0])
end
#endregion

function run(;range=1:1000)
    readSpreads()
    PricingUtil.RunRange[] = range
    if isempty(PandaInit[]) || (length(searchRange()) != length(PandaInit[]))
        PandaInit[] = initDefault() # [0.0160376, 0.0686671, 0.124022, 0.627685, 0.0402868, 0.939992, 0.99831, 86.3807, -88.1671, 0.191875, -66.161, 91.5975, -10.0204, 46.8196, -10.0945, -10.281, 88.2485, -66.8913, 57.3583, -34.4107, 0.973417, 76.4187, -0.383973, 76.1867, -99.7596, 75.5632, -41.3579, 45.3754, 59.4767, -52.3964, 64.0692, 77.6899, -8.29805, -21.0307, 38.899, -0.00562926, 50.718, -64.0306, 0.428358, -24.6193, 95.8691, -7.86242, 0.886855, 72.2335, -99.0591, -31.9659]
    end
    xs, ys = setup(x -> getStrike(x.oq2) < x.curp, PricingUtil.AllSpreadCalls, makeData)
    # filtered = filter(x -> getStrike(x.oq) < x.curp, AllCalls)
    # xs, ys = runData(makeData(filtered))
    global BbCallsItm = bboptimize(modelOptim(xs, ys), PandaInit[]; SearchRange=searchRange(), MaxTime=1000) #, NThreads=(Threads.nthreads()-1))
    PandaInit[] = best_candidate(BbCallsItm)
    drawStrice()
    # drawTexPrice()
end

#region Calc
function modelOptim(data, prices)
    function(p)
        return sum((model(data[i], p) - prices[i])^2 for i in eachindex(data))
    end
end

searchRange() = vcat(
    fill((0.0, 1.0), 5), # tex
    fill((0.0, 1.0), 4), # tex varia linsum
    fill((0.0, 1.0), 3*4), # vty varia linsum
    fill((0.0, 1.0), 4), # strikeRat varia linsum
    fill((-10.0, 10.0), 2*4*4), # mult combos
    fill((-100.0, 100.0), 2*4) # funcs
)
initDefault() = [(x[1] + x[2])/2 for x in searchRange()]

function calcVty(x)
    curpRats = varia(x[8])
    openCloseRats = varia(x[9])
    vixOpens = varia(x[10])
    linsum((), p, off)
    return (curpRats..., openCloseRats..., vixOpens...)
end

# return [tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, tex.weekend.value, tex.holiday.value, strikeRat, curpRat, openCloseRat, row.vixOpen, spreadWidthRat]
function modelOld(x, p)::Float64
    texBase = calcTex(x, p, 1)
    off = 6
    texs = varia(texBase)
    tex = linsum(texs, p, off)
    off += length(texs)

    vtys = calcVty(x)
    vty = linsum(vtys, p, off)
    off += length(vtys)

    strikeRats = varia(x[7])
    strikeRat = linsum(strikeRats, p, off)
    off += length(strikeRats)

    texVtyComb1 = linsum(combsMult(texs, vtys), p, off)
    off += length(texs) * length(vtys)
    texVtyComb2 = linsum(combsMult(texs, vtys), p, off)
    off += length(texs) * length(vtys)

    thex = tex + vty + strikeRat + texVtyComb1
    # linsum((tex, vty, strikeRat), p, off)
    # off += 3

    res1 = otherFunc(thex, p[off], p[off+1], p[off+2])
    off += 4

    res2 = otherFunc(thex, p[off], p[off+1], p[off+2])
    off += 4

    res = texVtyComb2 + p[off] * res1 + p[off+1] * res2
    off += 2
    # @info "res" res off
    return res # res < 0.0 ? 100000*res : res
end
#endregion

#region MakeData
function makeData(items)
    return (makeX.(items), makeSpreadPrice.(items))
end

function makeX(item)::Vector{Float64}
    # (;ts, curp, spyOpen, spyClosePrev, vixOpen, oq1, oq2)
    @assert getStrike(item.oq1) < getStrike(item.oq2)
    strikeInner = getStrike(item.oq2)
    spreadWidth = getSpreadWidth(item.oq1, item.oq2)
    spreadWidthRat = spreadWidth / item.curp
    strikeRat = strikeInner / item.curp - 1.0
    curpRat = item.curp / item.spyOpen - 1.0
    openCloseRat = item.spyOpen / item.spyClosePrev - 1.0
    tex = Calendars.calcDurToExpr(item.ts, getExpiration(item.oq2))
    return [tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, tex.weekend.value, tex.holiday.value, strikeRat, curpRat, openCloseRat, item.vixOpen, spreadWidthRat, datetime2unix(item.ts)]
end
makeSpreadPrice(item)::Float64 = Float64(calcNetLongExtrin(item.oq1, item.oq2, C(item.curp)) / getSpreadWidth(item.oq1, item.oq2))
getSpreadWidth(oq1, oq2) = abs(getStrike(oq1) - getStrike(oq2))
#endregion

end



# function listVars(xs)
#     strikeRat = xs[7]
#     tex = calcTex(xs, fill(1.0, 5), 1)
#     vixOpen = xs[10]
#     curpRat = xs[8]
#     openCloseRat = xs[9]
#     # combined1 = varia(xs[9] + xs[9]) # no improvement
#     # combined2 = varia(xs[9] + xs[9] + xs[10]) # no improvement

#     ts = unix2datetime(xs[12])
#     sso = round(ts - getMarketOpen(toDateMarket(ts)), Millisecond).value / 1000.0 # log0 had dcor of almost 0.1
#     # dow = varia(dayofweek(toDateMarket(ts))) # max dcor around 0.031 so ignore
#     return [strikeRat, tex, vixOpen, curpRat, openCloseRat, sso]
# end
