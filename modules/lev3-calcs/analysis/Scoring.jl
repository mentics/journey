 module Scoring
using SH, BaseTypes, SmallTypes, StratTypes, ProbTypes, RetTypes, VectorCalcUtil
using CalcUtil, Bins, ThreadUtil, LogUtil
# using Kelly

export calcScore1, scoreRand, probMid, probFlat
export bySym, bySym2, byScore, byProb
export resetCountsScore, showCountsScore

# TODO: remove after fixing calcs to not need it
TexPerDay = 6.5 + .3 * (24 - 6.2)

scoreRand(args...) = rand()

# TODO: move
# sigbal(x::Float64)::Float64 = ( y = (-.5) + (1 / (1 + ℯ^-x)) ; x < 0.0 ? 2 * y : y )
sigbal(x::Float64)::Float64 = -1 + (2 / (1 + ℯ^-x))
# sigmoid(x::Float64, half::Float64=0.0)::Float64 = 1 / (1 + ℯ^(-x+half))

# TODO: would probably speed things up a little if we typed ctx and tctx
const MetricBuf = Ref{Vector{NamedTuple}}()
filtkeys() = [:ev, :ev1, :ev2, :noImpEvr, :noImpEvOr, :standsAlone, :cantAlone, :maxLoss, :maxLossAbs, :prob, :noImpProb, :noImpProb2, :noImpLoss,
              :probStandAlone, :sides, :sidesMaxLoss, :special, :special2, :noImpSum, :biasWrong]
calcScore1(ctx, bufCombi, bufBoth, posRet, show=false) = calcScore1(ctx, (; metsBoth=MetricBuf[]), bufCombi, bufBoth, posRet, show)
function calcScore1(ctx, tctx, bufCombi::AVec{Float64}, bufBoth::AVec{Float64}, posRet::Union{Nothing,Ret}, show=false)::Float64
    texDays = ctx.tex / TexPerDay
    MAX_LOSS = -3 + min(2., 2*texDays/20)
    numLegs = isnothing(posRet) ? 4 : 4 + posRet.numLegs
    # minForLegs = (.02 / 4.0) * numLegs
    metsPos = ctx.metsPos

    if isempty(bufCombi)
        # This means we're calcing base score from existing position, so only bufPos will be valid but it's also passed into bufBoth.
        # return -10000
        show && ( @info "pos scoring" metsBoth )
        return score(metsPos)
    end

#======== construct test
    metb = calcMetrics(ctx.probs[1], bufBoth, numLegs)

    buf = bufCombi
    met = calcMetrics(ctx.probs[1], bufCombi, 4)

    # met.mn > -3. || return countNo(:maxLossAbs)

    dir = Main.save[:dir]
    # texDays > 7 || (dir = 1)
    2.0 < met.mx < 2.5 && -2.0 < met.mn < -1.5 || return countNo(:maxLossAbs)
    (2.0 < buf[1] < 2.5) || (-2.0 < buf[1] < -1.5) || return countNo(:maxLossAbs)
    (2.0 < buf[end] < 2.5) || (-2.0 < buf[end] < -1.5) || return countNo(:maxLossAbs)

    xnLongL = met.mx + buf[1]
    xnLongR = met.mx + buf[end]
    xnShortL = buf[1] + met.mn
    xnShortR = buf[end] + met.mn
    xn = dir == 1 ? (xnLongL + xnLongR)/2 : (xnShortL + xnShortR)/2

    minProf = .11
    if dir == 1
        buf[1] < 0.0 && buf[end] < 0.0 || return countNo(:sides)
        (xnLongL > 2 * minProf && xnLongR > 2 * minProf) || return countNo(:sides)
    else
        buf[1] > 0.0 && buf[end] > 0.0 || return countNo(:sides)
        (xnShortL > 2 * minProf && xnShortR > 2 * minProf) || return countNo(:sides)
    end

    # spread = .008
    # req(bufBoth, Bins.nearest(1.0 - spread), Bins.nearest(1.0 + spread), 0.5) || return countNo(:sides)
    # # req(bufCombi, Bins.nearest(1.0 - spread), Bins.nearest(1.0 + spread), 0.07) || return countNo(:special)

    # return xn
    return score([metb])
    # return metb.prob
========#

    metsBoth = tctx.metsBoth
    for i in eachindex(ctx.probs)
        metsBoth[i] = calcMetrics(ctx.probs[i], bufBoth, numLegs)
    end
    metsBoth[1].mn > MAX_LOSS || return countNo(:maxLossAbs)
    # MaxLoss = -8 + min(4., 4*texDays/20)
    # metsBoth[1].mn > MaxLoss || return countNo(:maxLossAbs)

    isNew = isnothing(posRet)

    # (bufBoth[Bins.center() ÷ 2] > 0.0 && bufBoth[Bins.center()] > 0) || return countNo(:special)

    # metsBoth[1].evr > 0 || return countNo(:ev1)
    # metc = isNew ? metsBoth[1] : calcMetrics(ctx.probs[1], bufCombi, 4)
    # metc.mn > -0.8 || return countNo(:maxLossAbs)


    # if ctx.tex > 10
        # bufBoth[1] > minForLegs || return countNo(:sides)
        # bufBoth[end] > minForLegs || return countNo(:sides)
        # bufCombi[1] > .02 || return countNo(:sides)
        # bufCombi[end] > .02 || return countNo(:sides)
    # end

    # bufCombi[1] > 0.07 || return countNo(:sides)
    # bufCombi[end] > 0.07 || return countNo(:sides)

    # bufBoth[1] > 0.0 || return countNo(:sides)
    if isNew
        if texDays > 4
            bufBoth[1] > 0.0 && bufBoth[end] > 0.0 || return countNo(:sides)
        else
            (bufBoth[1] > 0.0 && bufBoth[end] > 0.0) || (bufBoth[1] >= bufBoth[end]) || return countNo(:sides)
            # req(bufBoth, Bins.nearest(.92), Bins.nearest(1.02), 0.0) || return countNo(:sides)
        end
        metsBoth[1].prob >= .7 || return countNo(:prob)
        # req(bufBoth, Bins.nearest(.96), Bins.nearest(1.04), .1) || return countNo(:sides)
        # (bufBoth[1] >= .14 && bufBoth[end] >= .14) || return countNo(:sides)
        # bufBoth[end] >= bufBoth[1] || return countNo(:sides)
        # metb.ev > 0.0 || metb2.ev > 0.0 || return countNo(:ev)
        # (metb.ev + metb2.ev) > 0.04 || return countNo(:ev) # || (factor = upFactor(factor, .8, "sumev", show))
        # bufBoth[end] > 0.0 || return countNo(:sides)
        # return metb.profit - 2 * metb.loss
    else
        if texDays > 3
            (bufBoth[1] > 0.0 && bufBoth[end] > 0.0) || (bufBoth[1] >= (bufBoth[end]) - .2) || return countNo(:sides)
        end
        # metp = ctx.metsPos[1]
        # metsBoth[1].loss > 1.5 * metp.loss || return countNo(:special2)
        # metsBoth[1].evr >= metp.evr || return countNo(:noImpEvr)
        # probLency = .85 - .15 * sigbal(.43 * texDays - 6.0)
        # metb.prob >= probLency * metp.prob || return countNo(:noImpProb)

        # metsBoth[1].mn > -1.0 || metp.mn > 0.0 || metsBoth[1].mn > 1.5 * metp.mn || return countNo(:special2)
        # metsBoth[1].ev >= metp.ev || return countNo(:noImpEvr)
        # isok = false
        # for i in eachindex(ctx.probs)
        #     metp = ctx.metsPos[i]
        #     metb = metsBoth[i]

        #     # (2*metb.prob + metb2.prob) >= probLency * (2*metp.prob + metp2.prob) || return countNo(:noImpProb)
        #     metb.prob >= probLency * metp.prob || continue

        #     # if (metb.mn < 0.0)
        #     #     mnLency = probLency # .85 - .15 * sigbal(.43 * texDays - 6.0)
        #     #     metb.mn >= mnLency * metp.mn || return countNo(:noImpLoss)
        #     # end

        #     # metb.mn >= probLency * metp.mn || continue
        #     metb.evr >= probLency * metp.evr || continue
        #     # metb.loss >= probLency * metp.loss || continue

        #     # isnothing(ctx.biasUse) || bias(ctx.biasUse, ctx.probs[1], bufBoth, posRet, numLegs) || continue
        #     isok = true
        #     break
        # end
        # isok || return countNo(:special)

        # metp = metsPos[1]
        # # metb.mn > MAX_LOSS / (1.0 + numLegs/8) || return countNo(:maxLossAbs)

        # probLency = .85 - .15 * sigbal(.43 * texDays - 6.0)
        # # (2*metb.prob + metb2.prob) >= probLency * (2*metp.prob + metp2.prob) || return countNo(:noImpProb)
        # (2*metb.prob) >= probLency * (2*metp.prob) || return countNo(:noImpProb)

        # # if (metb.mn < 0.0)
        # #     mnLency = probLency # .85 - .15 * sigbal(.43 * texDays - 6.0)
        # #     metb.mn >= mnLency * metp.mn || return countNo(:noImpLoss)
        # # end

        # metb.mn >= probLency * metp.mn || return countNo(:noImpLoss)
        # metb.evr >= probLency * metp.evr || return countNo(:noImpEvr)

        # isnothing(ctx.biasUse) || bias(ctx.biasUse, ctx.probs[1], bufBoth, posRet, numLegs) || return countNo(:biasWrong)

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
    show && ( @info "scoring" metsBoth )
    return score(metsBoth)
end

score(mets) = sum(m.evr for m in mets) / length(mets)
# score(met::NamedTuple) = met.evr
# function score(factor::Float64, mets::Vector)::Float64
#     res = factor * (score(mets[1]) + sum(score, mets)) # this doubles weight of mets[1]
#     isfinite(res) || error("something wrong with scoring ", res)
#     return res
# end

# wasImproved(simple::Bool, prev, cur) = simple ? cur > prev : cur > prev #^1.01
# wasImproved(simple::Bool, prev, cur, factor=.1) = simple ? cur > prev : cur > prev + log(1.0 + factor * abs(prev))
# improvement(prev::Float64, cur::Float64) = (cur - prev) / prev
improveFactor(from::Float64, to::Float64)::Float64 = from >= to ? 0.95 / (1.0 + from - to) : 1.01 + log(1.0 + 4*(to - from))
# upFactor(factor, k, str, show=false, k2=1.0) = ( res = k * (1 + k2 * (factor - 1) ) ; show && println("$(nstr(k)) by $(k2) to $(nstr(res)) for $(str)") ; res )
upFactor(factor, k, str, show=false) = ( res = k * factor ; show && println("$(nstr(k)) to $(nstr(res)) for $(str)") ; res )

countNo(sym::Symbol) = ( @atomic filt[sym].count += 1 ; NaN )

# function calcScore1(ctx, tctx, bufCombi::AVec{Float64}, bufPos::Union{Nothing,AVec{Float64}}, bufBoth::AVec{Float64})::Float64
byScore(ctx, c, b, pr) = calcScore1(ctx, c, b, pr)
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
scoreProb(p, mn) = p ≈ 1.0 ? p + mn : p
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

# PidealThreshUp = 0.01
# PidealThreshDown = 0.01
# PidealMidMult = 2.0
function probIdeal(p::Prob)
    pvals = getVals(p)
    pvMax = maximum(pvals)
    PidealMidMult = 4.0
    PidealThreshUp = pvMax / PidealMidMult
    PidealThreshDown = pvMax / PidealMidMult / PidealMidMult
    cur = 1.0
    vals = Bins.empty()
    vals[1] = pvals[1]
    for i in Bins.binds()
        if cur === 1.0 && pvals[i] > PidealThreshUp
            cur = PidealMidMult
        elseif cur === PidealMidMult && pvals[i] < PidealThreshDown
            cur = 0.0001
        end
        vals[i] = cur * pvals[i]
    end
    vals[end] = 0.0
    normalize!(vals)
    return Prob(getCenter(p), vals)
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
showCountsScore() = ( res = filter(kv -> kv.second.count != 0, filt) ; @log info "Score counts" passed.count res ) # @info "Score counts" filtProb.count filtEv.count filtEvr.count filtEvrB.count filtMid.count filtEvrInv.count filtSides.count filtExtrema.count # filtLong.count filtShort.count filtMid.count filtPos.count passed.count

#region Local
const passed = Atomic{Int}(0)
const filt = Dict{Symbol,Atomic{Int}}()

req(sp::Real, buf, prLeft::Float64, prRight::Float64, greaterThan::Float64, numChecks::Int=10) = req(buf, Bins.nearest(Float64(prLeft/sp)), Bins.nearest(Float64(prRight/sp)), greaterThan, numChecks)
function req(buf, binLeft::Int, binRight::Int, greaterThan::Float64, numChecks::Int=10)
    for i in range(binLeft, binRight; step=(binRight - binLeft) ÷ numChecks)
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
