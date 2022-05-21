 module Scoring
using SH, BaseTypes, SmallTypes, StratTypes, ProbTypes, RetTypes
using CalcUtil, Bins, ThreadUtil, LogUtil
# using Kelly

export calcScore1, scoreRand, probMid, probFlat
export bySym, bySym2, byScore, byProb
export resetCountsScore, showCountsScore

scoreRand(args...) = rand()

# TODO: move
# sigbal(x::Float64)::Float64 = ( y = (-.5) + (1 / (1 + ℯ^-x)) ; x < 0.0 ? 2 * y : y )
sigbal(x::Float64)::Float64 = y = -1 + (2 / (1 + ℯ^-x))

filtkeys() = [:ev, :ev1, :ev2, :noImpEv, :noImpEvOr, :standsAlone, :cantAlone, :maxLoss, :maxLossAbs, :prob, :noImpProb, :noImpProb2, :noImpLoss,
              :probStandAlone, :sides, :sidesMaxLoss, :special, :special2, :noImpSum, :biasWrong]
function calcScore1(ctx, tctx, bufCombi::AVec{Float64}, bufBoth::AVec{Float64}, posRet::Union{Nothing,Ret}, show=false)::Float64
    MAX_LOSS = -3
    factor = 1.0
    numLegs = isnothing(posRet) ? 4 : 4 + posRet.numLegs
    metb = calcMetrics(ctx.probs[1], bufBoth, numLegs)
    metb2 = calcMetrics(ctx.probs[2], bufBoth, numLegs)

    if isempty(bufCombi)
        # This means we're calcing base score from existing position, so only bufPos will be valid but it's also passed into bufBoth.
        # return -10000
        return score(metb, metb2, factor)
    end

    # bufCombi[1] > 0.04 || return countNo(:sides)
    # bufCombi[end] > 0.04 || return countNo(:sides)
    # bufBoth[1] > 0.14 || return countNo(:sides)
    # bufBoth[end] > 0.14 || return countNo(:sides)
    metb.mn > MAX_LOSS || return countNo(:maxLossAbs)
    # return score(metb, metb2, factor)

    # req(ctx.sp, bufCombi, 410, 420, 0.0) || return countNo(:sides)

    isNew = isnothing(posRet)

    if isNew
        (bufBoth[1] >= .34 && bufBoth[end] >= .34) || return countNo(:sides)
        bufBoth[end] >= bufBoth[1] || return countNo(:sides)
        # metb.ev > 0.0 || metb2.ev > 0.0 || return countNo(:ev)
        # (metb.ev + metb2.ev) > 0.04 || return countNo(:ev) # || (factor = upFactor(factor, .8, "sumev", show))
        # metb.mn > 2.0 || return countNo(:maxLossAbs)
        # bufBoth[end] > 0.0 || return countNo(:sides)
        # return metb.profit - 2 * metb.loss
    else
        metp = ctx.metp ; metp2 = ctx.metp2
        # metb.mn > MAX_LOSS / (1.0 + numLegs/8) || return countNo(:maxLossAbs)

        probLeniency = .85 - .15 * sigbal(.43 * ctx.numDays - 6.0)
        (2*metb.prob + metb2.prob) >= probLeniency * (2*metp.prob + metp2.prob) || return countNo(:noImpProb)

        isnothing(ctx.biasUse) || bias(ctx.biasUse, ctx.probs[1], bufBoth, posRet, numLegs) || return countNo(:biasWrong)

        # improvProb = (5*metb.prob + metb2.prob) - (5*metp.prob + metp2.prob)
        # improvEv = (5*metb.ev + metb2.ev) - (5*metp.ev + metp2.ev)
        # improvEvr = (5*metb.evr + metb2.evr) - (5*metp.evr + metp2.evr)
        # improvLoss = ((5*metb.loss + metb2.loss) - (5*metp.loss + metp2.loss))
        # improvProb + improvEv + improvEvr + improvLoss > 0.01 || return countNo(:noImpSum)

        # (2*metb.prob + metb2.prob) >= (2*metp.prob + metp2.prob) || return countNo(:noImpProb)
        # # (metb.ev > metp.ev) || (metb2.ev > metp2.ev) || return countNo(:noImpEvOr)
        # (2*metb.ev + metb2.ev) >= (2*metp.ev + metp2.ev) || return countNo(:noImpEv)
        # # ((metb.ev >= -.2) && (metb2.ev >= -.2)) || return countNo(:ev) TODO: this is bad because out of our control when price moves.
        # (2*metb.loss + metb2.loss) >= (2*metp.loss + metp2.loss) || return countNo(:noImpLoss)

        # return improvProb + improvLoss #+ improvEvr

        # metc = isNew ? metb : calcMetrics(ctx.probs[1], bufCombi, 4)
        # metc2 = isNew ? metb2 : calcMetrics(ctx.probs[2], bufCombi, 4)
        # metc.mn > MAX_LOSS || return countNo(:maxLossAbs)
        # canStandAlone = (metc.ev + metc2.ev > 0.04) # && (metc.ev + metc2.ev > 0.04)
        # canStandAlone || return countNo(:cantAlone)
        # return metb.evr
    end
    @atomic passed.count += 1
    return score(metb, metb2, factor)
end

score(met) = score(met, met, 1.0)
# score(metb, factor::Float64)::Float64 = factor * (metb.ev + metb.evr) # scoreProb(metb.prob, metb.mn)
function score(metb, metb2, factor::Float64)::Float64
    # return factor * (metb.evr + metb2.evr) / 2
    res = factor * (2*metb.evr + metb2.evr)
    isfinite(res) || error("found prob ", res)
    return res
    # mn = metb.mn + metb2.mn
    # numer = factor * (metb.ev + metb2.ev)
    # return mn < 0.0 ? numer / (1 - mn) : numer * (1 + mn)
end

# wasImproved(simple::Bool, prev, cur) = simple ? cur > prev : cur > prev #^1.01
# wasImproved(simple::Bool, prev, cur, factor=.1) = simple ? cur > prev : cur > prev + log(1.0 + factor * abs(prev))
# improvement(prev::Float64, cur::Float64) = (cur - prev) / prev
improveFactor(from::Float64, to::Float64)::Float64 = from >= to ? 0.95 / (1.0 + from - to) : 1.01 + log(1.0 + 4*(to - from))
# upFactor(factor, k, str, show=false, k2=1.0) = ( res = k * (1 + k2 * (factor - 1) ) ; show && println("$(nstr(k)) by $(k2) to $(nstr(res)) for $(str)") ; res )
upFactor(factor, k, str, show=false) = ( res = k * factor ; show && println("$(nstr(k)) to $(nstr(res)) for $(str)") ; res )

countNo(sym::Symbol) = ( @atomic filt[sym].count += 1 ; NaN )

# function calcScore1(ctx, tctx, bufCombi::AVec{Float64}, bufPos::Union{Nothing,AVec{Float64}}, bufBoth::AVec{Float64})::Float64
byScore(ctx, c, b, pr) = calcScore1(ctx, nothing, c, b, pr)
byProb(ctx, c, b, pr) = ( metb = calcMetrics(ctx.probs[1], b) ; scoreProb(metb.prob, metb.mn) )
bySym(sym::Symbol) = (ctx, c, b, pr) -> calcMetrics(ctx.probs[1], b, numLegsBoth(pr))[sym]
bySym2(sym::Symbol) = (ctx, c, b, pr) -> calcMetrics(ctx.probs[2], b, numLegsBoth(pr))[sym]
numLegsBoth(r::Nothing) = 4
numLegsBoth(r::Ret) = 4 + r.numLegs

# TODO: assert center same
# @assert prob.center == first(s)[2].center
# byEv(prob::Prob, numLegs::Real) = s::Coll{LegRet,4} -> calcMetrics(getVals(prob), combineTo(Vals, s), getCap(numLegs), getAdj(numLegs)).ev
# byEvr(prob::Prob, numLegs::Real) = s::Coll{LegRet,4} -> calcMetrics(getVals(prob), combineTo(Vals, s), getCap(numLegs), getAdj(numLegs)).evr
# byKelly(prob::Prob, numLegs::Real) = s::Coll{LegRet,4} -> calcKelly(getVals(prob), combineTo(Vals, s), getCap(numLegs), getAdj(numLegs))
# function byMetrics(prob::Prob)
#     function(s::Coll{LegRet,4})
#         k = calcKelly(getVals(prob), combineTo(Vals, s))
#         m = calcMetrics(getVals(prob), combineTo(Vals, s))
#         return k * m.evr * m.prob
#     end
# end
# function byProb(prob::Prob, numLegs::Real)
#     function(s::Coll{LegRet,4})
#         vals = combineTo(Vals, s)
#         met = calcMetrics(getVals(prob), vals, getCap(numLegs), getAdj(numLegs))
#         return scoreProb(met.prob, met.mn)
#     end
# end
# scoreProb(p, mn) = p ≈ 1.0 ? p + mn : p
# byMin(b) = extrema(combineTo(Vals, b))[1]
# byEvC(prob::Prob, numLegs::Int) = (c, b) -> calcMetrics(getVals(prob), combineTo(Vals, c), getCap(numLegs), getAdj(numLegs)).ev
# byMaxLoss(prob::Prob) =

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

probFlat(p::Prob)::Prob = ( vals = getVals(p) ; probFlat(getCenter(p), (vals[1] + vals[end])/2) )
function probFlat(center::Float64, ends::Float64)::Prob
    vals = Bins.with((1.0 - 2*ends) / Bins.NUM) # this is implemented hacky anyway and probably get removed, so... ok to call const here
    vals[1] = ends
    vals[end] = ends
    # normalize!(vals)
    return Prob(center, vals)
end

# function probRoof(center::Float64, ends::Float64)::Prob
#     vals = Bins.with((1.0 - 2*ends) / Bins.NUM) # this is implemented hacky anyway and probably get removed, so... ok to call const here
#     vals[1] = ends
#     vals[end] = ends
#     # normalize!(vals)
#     return Prob(center, vals)
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

function resetCountsScore()
    @atomic passed.count = 0
    empty!(filt) ; push!(filt, [k => Atomic{Int}(0) for k in filtkeys()]...)
end
showCountsScore() = ( res = filter(kv -> kv.second.count != 0, filt) ; @info "Score counts" passed.count res ) # @info "Score counts" filtProb.count filtEv.count filtEvr.count filtEvrB.count filtMid.count filtEvrInv.count filtSides.count filtExtrema.count # filtLong.count filtShort.count filtMid.count filtPos.count passed.count

#region Local
const passed = Atomic{Int}(0)
const filt = Dict{Symbol,Atomic{Int}}()

function req(sp, buf, prLeft, prRight, greaterThan)
    left = Bins.nearest(Float64(prLeft/sp))
    right = Bins.nearest(Float64(prRight/sp))

    for i in range(left, right; step=(right - left) ÷ 10)
        buf[i] > greaterThan || return false
    end
    return true
end

function bias(side::Side.T, prob, bufBoth, posRet, numLegs)::Bool
    # Directional Bias
    metpLong = score(calcMetrics(prob, posRet, Bins.halfRight()))
    metpShort = score(calcMetrics(prob, posRet, Bins.halfLeft()))
    metbLong = score(calcMetrics(prob, bufBoth, numLegs, Bins.halfRight()))
    metbShort = score(calcMetrics(prob, bufBoth, numLegs, Bins.halfLeft()))
    (side === Side.long ? metbLong >= metbShort : metbShort >= metbLong) || return false
    return side === Side.long ? (metbLong - metpLong >= metbShort - metpShort) : (metbShort - metpShort >= metbLong - metpLong)
end
#endregion

end
