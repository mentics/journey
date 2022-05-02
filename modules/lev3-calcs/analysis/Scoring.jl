module Scoring
using SH, BaseTypes, CalcUtil, Bins, ThreadUtil
using StratTypes, ProbTypes
using Kelly

export calcScore1, scoreRand, probMid, probFlat
export byEv, byEvr, byKelly, byProb, byMetrics
export resetCountsScore, showCountsScore

scoreRand(args...) = rand()

getCap(numPos::Real)::Float64 = 2.0 + 0.5 * (.25 * (numPos - 4))
getAdj(numPos::Real)::Float64 = 0.03 * (.25 * numPos)
calcMetricsBoth(pvals, bufBoth, numPos) = calcMetrics(pvals, bufBoth, getCap(4 + numPos), getAdj(4 + numPos))

filtkeys() = [:ev, :evr, :standsAlone, :cantAlone, :maxLoss, :noImpProb1, :noImpProb2, :lostProb100,
              :noImpEv1, :noImpEvr1, :noImpEv2, :noImpEvr2, :sides, :probStandAlone, :special]
function calcScore1(ctx, tctx, bufCombi::AVec{Float64}, bufPos::Union{Nothing,AVec{Float64}}, bufBoth::AVec{Float64})::Float64
    numPos = ctx.numPos
    adjCombi = getAdj(4) ; adjPos = getAdj(numPos) ; adjBoth = getAdj(4 + numPos) # TODO: should be length of combi, but it's always 4 right now
    capCombi = getCap(4) ; capPos = getCap(numPos) ; capBoth = getCap(4 + numPos)

    pvalsUse = getVals(ctx.probs[1])
    metb = calcMetrics(pvalsUse, bufBoth, capBoth, adjBoth)
    if isempty(bufCombi)
        # This means we're calcing base score from existing position, so only bufPos will be valid but it's also passed into bufBoth.
        return -10000 # metb.ev
    end
    isNew = isnothing(bufPos)
    pvalsUse2 = getVals(ctx.probs[2])
    metb2 = calcMetrics(pvalsUse2, bufBoth, capBoth, adjBoth)

    metc = isNew ? metb : calcMetrics(pvalsUse, bufCombi, capCombi, adjCombi)
    metc2 = isNew ? metb2 : calcMetrics(pvalsUse2, bufCombi, capCombi, adjCombi)

    canStandAlone = metc.ev > .05 && metc.evr > 1.0 && metc2.ev > .05 && metc2.evr > 1.0
    canStandAlone && countNo(:standsAlone)

    bmn, bmx = extrema(bufBoth)

    # TODO: if prob >= 1.0, require canStandAlone

    if isNew
        # canStandAlone || return countNo(:cantAlone)
        bmn > -0.7 - 0.25 * ctx.numDays || return countNo(:maxLoss)

        (bufCombi[1] > .12 && bufCombi[end] > .12) || return countNo(:special)

        # Directional Bias
        # metLong = calcMetrics(getVals(ctx.probs.plong), bufCombi)
        # metShort = calcMetrics(getVals(ctx.probs.pshort), bufCombi)
        # metShort.ev < metLong.ev || return countNo(filtMid)
    else
        pmn, pmx = extrema(bufPos)
        bmn >= 0.0 || bmn >= -0.7 - 0.25 * ctx.numDays || bmn >= 0.75 * pmn || return countNo(:maxLoss)
        metp = calcMetrics(pvalsUse, bufPos, capPos, adjPos)
        metp2 = calcMetrics(pvalsUse2, bufPos, capPos, adjPos)
        # (metp.prob ≈ 1.0 && metb.prob <= .999) && return countNo(:lostProb100)
        metb.prob ≈ 1.0 || metb.prob >= metp.prob || return countNo(:noImpProb1)
        metb2.prob ≈ 1.0 || metb2.prob >= metp2.prob || return countNo(:noImpProb2)

        # bufCombi[1] > 0.0 || return countNo(:sides)
        # bufBoth[1] > .2 || return countNo(filtSides)
        # bufBoth[1] > .2 || return countNo(filtSides)

        # binNearest(worst ret for best strat for target date / market().startPrice)
        # bufBoth[365] >= bufPos[365] + .1 || return countNo(filtEvrInv)
        # println(bufBoth[365] - bufPos[365])
        # return bufBoth[365] - bufPos[365]

        # if ctx.numDays >= 3
        #     metb.evr > 1.1 * metp.evr || return countNo(filtEvrB)
        #     metb.ev > 1.1 * metp.ev || return countNo(filtEvr)
        #     #     metb.loss > metp.loss || return countNo(filtEvrB)
        # else
            wasImproved(canStandAlone, metp.ev, metb.ev) || return countNo(:noImpEv1)
            wasImproved(canStandAlone, metp.evr, metb.evr) || return countNo(:noImpEvr1)
            wasImproved(canStandAlone, metp2.ev, metb2.ev) || return countNo(:noImpEv2)
            wasImproved(canStandAlone, metp2.evr, metb2.evr) || return countNo(:noImpEvr2)
            # metb.evr > reqimp * metp.evr || return countNo(filtEvr)
            # metb.ev > reqimp * metp.ev || return countNo(filtEv)
            # metb2.evr > reqimp * metp2.evr || return countNo(filtEvr)
            # metb2.ev > reqimp * metp2.ev || return countNo(filtEv)
            #     metb.loss > metp.loss || return countNo(filtEvrB)
        # end

        # Directional Bias
        # metLong = calcMetrics(getVals(ctx.probs.plong), bufCombi)
        # metShort = calcMetrics(getVals(ctx.probs.pshort), bufCombi)
        # metLongP = calcMetrics(getVals(ctx.probs.plong), bufPos)
        # metShortP = calcMetrics(getVals(ctx.probs.pshort), bufPos)
        # impShort = metShort.ev - metShortP.ev
        # impLong = metLong.ev - metLongP.ev
        # impLong > impShort || return countNo(filtMid)

        metb.prob < 0.999 || metb2.prob < 0.999 || canStandAlone || return countNo(:probStandAlone)
        # @info "kept" metb.prob metb2.prob canStandAlone
    end

    @atomic passed.count += 1
    return metb.evr
end

wasImproved(simple::Bool, prev, cur) = simple ? cur > prev : cur > prev #^1.01

countNo(sym::Symbol) = ( @atomic filt[sym].count += 1 ; NaN )

# TODO: assert center same
# @assert prob.center == first(s)[2].center
byEv(prob::Prob, numPos::Real) = s::Coll{LegRet,4} -> calcMetrics(getVals(prob), combineTo(Vals, s), getCap(numPos), getAdj(numPos)).ev
byEvr(prob::Prob, numPos::Real) = s::Coll{LegRet,4} -> calcMetrics(getVals(prob), combineTo(Vals, s), getCap(numPos), getAdj(numPos)).evr
byKelly(prob::Prob, numPos::Real) = s::Coll{LegRet,4} -> calcKelly(getVals(prob), combineTo(Vals, s), getCap(numPos), getAdj(numPos))
function byMetrics(prob::Prob)
    function(s::Coll{LegRet,4})
        k = calcKelly(getVals(prob), combineTo(Vals, s))
        m = calcMetrics(getVals(prob), combineTo(Vals, s))
        return k * m.evr * m.prob
    end
end
function byProb(prob::Prob, numPos::Real)
    function(s::Coll{LegRet,4})
        vals = combineTo(Vals, s)
        p = calcMetrics(getVals(prob), vals, getCap(numPos), getAdj(numPos)).prob
        return scoreProb(p, extrema(vals)[1])
    end
end
scoreProb(p, mn) = p ≈ 1.0 ? p + mn : p
# byMin(b) = extrema(combineTo(Vals, b))[1]
byEvC(prob::Prob) = (c, b) -> calcMetrics(getVals(prob), combineTo(Vals, c)).ev

function probMid(p::Prob, from::Float64=.97, to::Float64=1.03)
    left = binNearest(from)
    right = binNearest(to)
    vals = Bins.with(0.0)
    pv1 = getVals(p)
    sum = 0.0
    for i in left:right
        vals[i] = pv1[i]
        sum += vals[i]
    end
    vals ./= sum
    return Prob(getCenter(p), vals)
end

# function probFlat(p::Prob)
#     vals = with(1.0 / Bins.VNUM) # this is implemented hacky anyway and probably get removed, so... ok to call const here
#     pvals = getVals(p)
#     vals[1] = 1.1 * pvals[1]
#     vals[end] = 1.1 * pvals[end]
#     normalize!(vals)
#     return Prob(getCenter(p), vals)
# end

# function prob2(p::Prob, from::Float64=.97, to::Float64=1.03, mult::Float64=2.0)
#     left = binNearest(from)
#     right = binNearest(to)
#     vals = copy(getVals(p))
#     for i in left:right
#         vals[i] *= mult
#     end
#     s = sum(vals)
#     vals ./= s
#     return Prob(getCenter(p), vals)
# end

export compareVals
function compareVals(pv::Vector{Float64}, ideal::Vector{Float64}, vals::Vector{Float64})::Float64
    sum = 0.0
    for i in eachindex(vals)
        id = ideal[i]
        v = vals[i]
        if v < id
            diff = id - v
            sum += pv[i] * diff^2
        end
    end
    return sum
end

resetCountsScore() = ( empty!(filt) ; push!(filt, [k => Atomic{Int}(0) for k in filtkeys()]...) ) # = resetAtomics(filtProb, filtEv, filtEvr, filtEvrB, filtMid, filtEvrInv, filtSides, filtExtrema) # filtLong, filtShort, filtMid, filtPos, filtEvrB, passed)
showCountsScore() = ( res = filter(kv -> kv.second.count != 0, filt) ; @info "Score counts" passed.count res ) # @info "Score counts" filtProb.count filtEv.count filtEvr.count filtEvrB.count filtMid.count filtEvrInv.count filtSides.count filtExtrema.count # filtLong.count filtShort.count filtMid.count filtPos.count passed.count

#region Local
# const filtProb = Atomic{Int}(0)
# const filtEv = Atomic{Int}(0)
# const filtEvr = Atomic{Int}(0)
# const filtEvrB = Atomic{Int}(0)
# # const filtLong = Atomic{Int}(0)
# # const filtShort = Atomic{Int}(0)
# const filtMid = Atomic{Int}(0)
# # const filtPos = Atomic{Int}(0)
# const filtEvrInv = Atomic{Int}(0)
# const filtSides = Atomic{Int}(0)
# const filtExtrema = Atomic{Int}(0)
const passed = Atomic{Int}(0)
const filt = Dict{Symbol,Atomic{Int}}()
#endregion

end




# function calcEvPnl(ps, vs)
#     profit = 0.0 ; loss = 0.0
#     for i in 1:length(ps)
#         v = vs[i]
#         v > 0.0 && (profit += ps[i] * v)
#         v < 0.0 && (loss += ps[i] * v)
#     end
#     return (profit, loss)
# end
