 module Scoring
using SH, BaseTypes, StratTypes, ProbTypes, RetTypes
using CalcUtil, Bins, ThreadUtil, LogUtil
# using Kelly

export calcScore1, scoreRand, probMid, probFlat
export bySym, bySym2, byScore, byProb
export resetCountsScore, showCountsScore

scoreRand(args...) = rand()

# getCap(numLegs::Real)::Float64 = 10000.0 # 1.0 + log(numLegs/4) # 0.5 * (.25 * (numLegs - 4))
# getAdj(numLegs::Real)::Float64 = 0.0 # 0.03 * (.25 * numLegs)
# getCap(numLegs::Real)::Float64 = 1.0 + log(numLegs/4) # 0.5 * (.25 * (numLegs - 4))
# getAdj(numLegs::Real)::Float64 = 0.02 * (numLegs/4)

# TODO: move
# sigbal(x::Float64)::Float64 = ( y = (-.5) + (1 / (1 + ℯ^-x)) ; x < 0.0 ? 2 * y : y )

filtkeys() = [:ev, :ev1, :ev2, :noImpEv, :noImpEvOr, :standsAlone, :cantAlone, :maxLoss, :maxLossAbs, :prob, :noImpProb, :noImpProb2, :noImpLoss,
              :probStandAlone, :sides, :sidesMaxLoss, :special, :special2]
function calcScore1(ctx, tctx, bufCombi::AVec{Float64}, bufBoth::AVec{Float64}, posRet::Union{Nothing,Ret}, show=false)::Float64
    MAX_LOSS = -3
    factor = 1.0
    metb = calcMetrics(ctx.probs[1], bufBoth, 4 + posRet.numLegs)

    if isempty(bufCombi)
        # This means we're calcing base score from existing position, so only bufPos will be valid but it's also passed into bufBoth.
        return -10000
        # return score(metb, metb, factor)
    end
    # metb.ev > -0.21 || return countNo(:ev1)
    # metb.mn > 0.32 || return countNo(:maxLoss)
    # bufCombi[end] > 0.14 || return countNo(:sides)
    # bufBoth[1] > 0.14 || return countNo(:sides)
    # bufBoth[end] > 0.14 || return countNo(:sides)
    metb.mn > MAX_LOSS || return countNo(:maxLossAbs)
    # metb.prob >= .25 || return countNo(:prob)
    # return scoreProb(metb.prob, metb.mn)

    # req(ctx.sp, bufCombi, 410, 420, 0.0) || return countNo(:sides)

    isNew = isnothing(posRet)
    metb2 = calcMetrics(ctx.probs[2], bufBoth, 4 + posRet.numLegs)

    # metb2.ev > -0.21 || return countNo(:ev2)
    # println("ev1 ", metb.ev, " ev2 ", metb2.ev)
    # println(metb.ev, " ", metb2.ev)
    # return score(metb, metb2, factor)

    if isNew
        (bufBoth[1] > .14 && bufBoth[end] > .14) || return countNo(:sides)
        # metb.ev > 0.0 || metb2.ev > 0.0 || return countNo(:ev)
        (metb.ev + metb2.ev) > 0.04 || return countNo(:ev) # || (factor = upFactor(factor, .8, "sumev", show))
    else
        metp = ctx.metp ; metp2 = ctx.metp2

        # improvProb = (5*metb.prob + metb2.prob) - (5*metp.prob + metp2.prob)
        # improvEv = (metb.ev + metb2.ev) - (metp.ev + metp2.ev)
        # improvEvr = (5*metb.evr + metb2.evr) - (5*metp.evr + metp2.evr)
        # improvLoss = 2*((5*metb.loss + metb2.loss) - (5*metp.loss + metp2.loss))

        (metb.prob + metb2.prob) >= (metp.prob + metp2.prob) || return countNo(:noImpProb)
        (metb.ev > metp.ev) || (metb2.ev > metp2.ev) || return countNo(:noImpEvOr)
        # ((metb.ev >= -.2) && (metb2.ev >= -.2)) || return countNo(:ev) TODO: this is bad because out of our control when price moves.
        (metb.loss + metb2.loss) >= (metp.loss + metp2.loss) || return countNo(:noImpLoss)

        # bufBoth[1] > .6 * bufPos[1] || return countNo(:sides)
        # bufBoth[end] > .6 * bufPos[end] || return countNo(:sides)

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
bySym(sym::Symbol) = (ctx, c, b, pr) -> calcMetrics(ctx.probs[1], b)[sym]
bySym2(sym::Symbol) = (ctx, c, b, pr) -> calcMetrics(ctx.probs[2], b)[sym]

# TODO: assert center same
# @assert prob.center == first(s)[2].center
# byEv(prob::Prob, numLegs::Real) = s::Coll{LegRet,4} -> calcMetrics(getVals(prob), combineTo(Vals, s), getCap(numLegs), getAdj(numLegs)).ev
byEvr(prob::Prob, numLegs::Real) = s::Coll{LegRet,4} -> calcMetrics(getVals(prob), combineTo(Vals, s), getCap(numLegs), getAdj(numLegs)).evr
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
scoreProb(p, mn) = p ≈ 1.0 ? p + mn : p
# byMin(b) = extrema(combineTo(Vals, b))[1]
byEvC(prob::Prob, numLegs::Int) = (c, b) -> calcMetrics(getVals(prob), combineTo(Vals, c), getCap(numLegs), getAdj(numLegs)).ev
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



# function calcScore1(ctx, tctx, bufCombi::AVec{Float64}, bufPos::Union{Nothing,AVec{Float64}}, bufBoth::AVec{Float64})::Float64
#     numLegs = ctx.numLegs
#     adjCombi = getAdj(4) ; adjPos = getAdj(numLegs) ; adjBoth = getAdj(4 + numLegs) # TODO: should be length of combi, but it's always 4 right now
#     capCombi = getCap(4) ; capPos = getCap(numLegs) ; capBoth = getCap(4 + numLegs)

#     pvalsUse = getVals(ctx.probs[1])
#     metb = calcMetrics(pvalsUse, bufBoth, capBoth, adjBoth)
#     if isempty(bufCombi)
#         # This means we're calcing base score from existing position, so only bufPos will be valid but it's also passed into bufBoth.
#         return score(metb)
#     end
#     # metb.mn > -0.7 - 0.1 * ctx.numDays || return countNo(:maxLoss)
#     # bufCombi[end] > 1.05 * bufCombi[1] || return countNo(:special)
#     # (bufCombi[1] > .12 && bufCombi[end] > .12) || return countNo(:special)
#     # bufCombi[Bins.nearest(430.0/ctx.sp)] > 0.0 || return countNo(:special2)
#     # bufCombi[Bins.nearest(410.0/ctx.sp)] > 0.0 || return countNo(:special2)
#     # bufBoth[end] >= 0.32 || return countNo(:special)
#     # bufBoth[1] >= 0.0 || return countNo(:special2)
#     # isnothing(bufPos) || (bufBoth[1] + bufBoth[end]) >= 0.5 * (bufPos[1] + bufPos[end]) || return countNo(:special2)
#     # bufBoth[div(length(bufBoth), 2)] > -10.0 || return countNo(:special2)
#     return score(metb)

#     metb.mn > -5.0 || return countNo(:maxLossAbs)
#     # (bufBoth[1] > -1.7 && bufBoth[end] > -1.7) ||  return countNo(:sidesMaxLoss)

#     isNew = isnothing(bufPos)
#     pvalsUse2 = getVals(ctx.probs[2])
#     metb2 = calcMetrics(pvalsUse2, bufBoth, capBoth, adjBoth)

#     metc = isNew ? metb : calcMetrics(pvalsUse, bufCombi, capCombi, adjCombi)
#     metc2 = isNew ? metb2 : calcMetrics(pvalsUse2, bufCombi, capCombi, adjCombi)

#     canStandAlone = metc.ev > .05 && metc.evr > 1.0 && metc2.ev > .05 && metc2.evr > 1.0
#     canStandAlone && countNo(:standsAlone)

#     # if ctx.numDays >= 10
#     #     # (bufCombi[1] > .12 || bufCombi[end] > .12) || return countNo(:special)
#     # end

#     # TEMP: not sure about this numdays
#     if isNew || ctx.numDays >= 10
#         # TODO: check numDays > min to not accidentally
#         # canStandAlone || return countNo(:cantAlone)
#         metb.mn > -0.7 - log(0.5 * ctx.numDays) || return countNo(:maxLoss)

#         (bufCombi[1] > .12 && bufCombi[end] > .12) || return countNo(:special)

#         # Directional Bias
#         # metLong = calcMetrics(getVals(ctx.probs.plong), bufCombi)
#         # metShort = calcMetrics(getVals(ctx.probs.pshort), bufCombi)
#         # metShort.ev < metLong.ev || return countNo(filtMid)
#     else
#         metp = calcMetrics(pvalsUse, bufPos, capPos, adjPos)
#         metp2 = calcMetrics(pvalsUse2, bufPos, capPos, adjPos)
#         # If we just moved it to 100% prob, then keep it
#         metb.prob ≈ 1.0 && metp.prob < 1.0 && ( @atomic passed.count += 1 ; return score(metb) )

#         metb.mn >= 0.0 || metb.mn >= -0.7 - log(0.4 * ctx.numDays) || metb.mn >= 0.75 * metp.mn ||
#                 (wasImproved(canStandAlone, metp.loss, metb.loss, .2) && wasImproved(canStandAlone, metp2.loss, metb2.loss, .2)) ||
#                 return countNo(:maxLoss)
#         # (metp.prob ≈ 1.0 && metb.prob <= .999) && return countNo(:lostProb100)
#         metb.prob ≈ 1.0 || metb.prob >= metp.prob || return countNo(:noImpProb1)
#         metb2.prob ≈ 1.0 || metb2.prob >= metp2.prob || return countNo(:noImpProb2)

#         # bufCombi[1] > 0.0 || return countNo(:sides)
#         # bufBoth[1] > .2 || return countNo(filtSides)
#         # bufBoth[1] > .2 || return countNo(filtSides)

#         # binNearest(worst ret for best strat for target date / market().startPrice)
#         # bufBoth[365] >= bufPos[365] + .1 || return countNo(filtEvrInv)
#         # println(bufBoth[365] - bufPos[365])
#         # return bufBoth[365] - bufPos[365]

#         # if ctx.numDays >= 3
#         #     metb.evr > 1.1 * metp.evr || return countNo(filtEvrB)
#         #     metb.ev > 1.1 * metp.ev || return countNo(filtEvr)
#         #     #     metb.loss > metp.loss || return countNo(filtEvrB)
#         # else
#             wasImproved(canStandAlone, metp.ev, metb.ev) || return countNo(:noImpEv1)
#             wasImproved(canStandAlone, metp.evr, metb.evr) || return countNo(:noImpEvr1)
#             wasImproved(canStandAlone, metp2.ev, metb2.ev) || return countNo(:noImpEv2)
#             wasImproved(canStandAlone, metp2.evr, metb2.evr) || return countNo(:noImpEvr2)
#             # metb.evr > reqimp * metp.evr || return countNo(filtEvr)
#             # metb.ev > reqimp * metp.ev || return countNo(filtEv)
#             # metb2.evr > reqimp * metp2.evr || return countNo(filtEvr)
#             # metb2.ev > reqimp * metp2.ev || return countNo(filtEv)
#             #     metb.loss > metp.loss || return countNo(filtEvrB)
#         # end

#         # Directional Bias
#         # metLong = calcMetrics(getVals(ctx.probs.plong), bufCombi)
#         # metShort = calcMetrics(getVals(ctx.probs.pshort), bufCombi)
#         # metLongP = calcMetrics(getVals(ctx.probs.plong), bufPos)
#         # metShortP = calcMetrics(getVals(ctx.probs.pshort), bufPos)
#         # impShort = metShort.ev - metShortP.ev
#         # impLong = metLong.ev - metLongP.ev
#         # impLong > impShort || return countNo(filtMid)

#         metb.prob < 0.999 || metb2.prob < 0.999 || canStandAlone || return countNo(:probStandAlone)
#         # @info "kept" metb.prob metb2.prob canStandAlone
#     end

#     @atomic passed.count += 1
#     return score(metb)
# end
