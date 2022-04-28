module Scoring
using SH, BaseTypes, CalcUtil, Bins, ThreadUtil
using StratTypes, ProbTypes
using Kelly

export calcScore1, scoreRand, probMid, probFlat
export byEv, byEvr, byKelly, byProb, byMetrics
export resetCountsScore, showCountsScore

const FLAT = fill(1.0/numVals(), numVals())
const MID = fill(1.0/numVals(), numVals())

scoreRand(args...) = rand()

filtkeys() = [:ev, :evr, :standsAlone, :cantAlone, :maxLoss, :noImpProb1, :noImpProb2, :lostProb100,
              :noImpEv1, :noImpEvr1, :noImpEv2, :noImpEvr2]
function calcScore1(ctx, tctx, bufCombi::AVec{Float64}, bufPos::Union{Nothing,AVec{Float64}}, bufBoth::AVec{Float64})::Float64
    pvalsUse = getVals(ctx.probs[1])
    metb = CalcUtil.calcMetrics(pvalsUse, bufBoth)
    pvalsUse2 = getVals(ctx.probs[2])
    metb2 = CalcUtil.calcMetrics(pvalsUse2, bufBoth)
    if isempty(bufCombi)
        # This means we're calcing base score from existing position, so only bufPos will be valid but it's also passed into bufBoth.
        return metb.ev
    end
    canStandAlone = metb.ev > .1 && metb.evr > 1.0 && metb2.ev > .1 && metb2.evr > 1.0
    canStandAlone && countNo(:standsAlone)
    (!isnothing(bufPos) || canStandAlone) || return countNo(:cantAlone)
    # bufCombi[365] >= 0 || return countNo(filtEvrInv)
    # TODO: created dict for filter reason so we can just create new symbols on the fly

    bmn, bmx = extrema(bufBoth)
    bmn > -1.7 || return countNo(:maxLoss)
    # metb.prob < 1.0 || (@atomic passed.count += 1 ; return true)

    # return metb.evr

    # return scoreProb(metb.prob, bmn)
    # TODO: try return metb.prob + bmn

    # metb.ev > .2 || return countNo(filtEv)
    # metb.evr > 1.2 || return countNo(filtEvr)

    # metb.prob ≈ 1.0 || return countNo(filtProb)

    # (bmn < -2.3 || bmx <= .49) && return countNo(filtExtrema)


    # if ctx.numDays >= 7
    #     (bufBoth[2] > .14 || bufBoth[numBins()+1] > .14) || return countNo(filtSides)
    #     (bmn < -(1 + 7/5) || bmx <= .49) && return countNo(filtExtrema)
    # elseif ctx.numDays >= 3
    #     (bufBoth[2] > .14 && bufBoth[numBins()+1] > .14) || return countNo(filtSides)
    #     (bmn > -(1.0 + ctx.numDays/5.0) && bmx > .49) || return countNo(filtExtrema)
    # else
    #     (bmn > -1.0 && bmx > .49) || return countNo(filtExtrema)
    #     metb.prob > 0.6 || return countNo(filtProb)
    # end

    if !isnothing(bufPos)
        metp = calcMetrics(pvalsUse, bufPos)
        metp2 = calcMetrics(pvalsUse2, bufPos)
        # (metp.prob ≈ 1.0 && metb.prob <= .999) && return countNo(:lostProb100)
        metb.prob ≈ 1.0 || metb.prob >= metp.prob || return countNo(:noImpProb1)
        metb2.prob ≈ 1.0 || metb2.prob >= metp2.prob || return countNo(:noImpProb2)

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
    else
        # Directional Bias
        # metLong = calcMetrics(getVals(ctx.probs.plong), bufCombi)
        # metShort = calcMetrics(getVals(ctx.probs.pshort), bufCombi)
        # metShort.ev < metLong.ev || return countNo(filtMid)
    end

    # (bufCombi[2] >= .21 && bufCombi[numBins()+1] >= .21) || return countNo(filtSides)
    # bufBoth[binsMid()] > .1 || return countNo(filtMid)

    # metinvb = calcMetrics(getVals(ctx.probs.pposInv), bufBoth)
    # metinvp = calcMetrics(getVals(ctx.probs.pposInv), bufPos)
    # metinvb.evr > metinvp.evr || return countNo(filtEvr)

    # kel = calcKelly!(pvalsUse, bufBoth)
    # isfinite(kel) || return countNo(filtEvr) # TODO: changge filt var if keep this

    # metc = calcMetrics(getVals(probUse), bufCombi)
    # metc.ev > 0 || return countNo(filtEv) # We don't want to do this because it won't let us fill in holes

    @atomic passed.count += 1
    return metb.evr
end

wasImproved(simple::Bool, prev, cur) = simple ? cur > prev : cur > prev #^1.01

countNo(sym::Symbol) = ( @atomic filt[sym].count += 1 ; NaN )

# TODO: assert center same
# @assert prob.center == first(s)[2].center
byEv(prob::Prob) = s::Coll{LegRet,4} -> calcMetrics(getVals(prob), combineTo(Vals, s)).ev
byEvr(prob::Prob) = s::Coll{LegRet,4} -> calcMetrics(getVals(prob), combineTo(Vals, s)).evr
byKelly(prob::Prob) = s::Coll{LegRet,4} -> calcKelly(getVals(prob), combineTo(Vals, s))
function byMetrics(prob::Prob)
    function(s::Coll{LegRet,4})
        k = calcKelly(getVals(prob), combineTo(Vals, s))
        m = calcMetrics(getVals(prob), combineTo(Vals, s))
        return k * m.evr * m.prob
    end
end
function byProb(prob::Prob)
    function(s::Coll{LegRet,4})
        vals = combineTo(Vals, s)
        p = calcMetrics(getVals(prob), vals).prob
        return scoreProb(p, extrema(vals)[1])
    end
end
scoreProb(p, mn) = p ≈ 1.0 ? p + mn : p
# byMin(b) = extrema(combineTo(Vals, b))[1]
byEvC(prob::Prob) = (c, b) -> calcMetrics(getVals(prob), combineTo(Vals, c)).ev

function probMid(p::Prob, from::Float64=.97, to::Float64=1.03)
    left = binNearest(from)
    right = binNearest(to)
    vals = fill(0.0, numVals())
    pv1 = getVals(p)
    sum = 0.0
    for i in left:right
        vals[i] = pv1[i]
        sum += vals[i]
    end
    vals ./= sum
    return Prob(getCenter(p), vals)
end

function probFlat(p::Prob)
    vals = fill(1.0 / numVals(), numVals())
    pvals = getVals(p)
    vals[1] = 1.1 * pvals[1]
    vals[end] = 1.1 * pvals[end]
    normalize!(vals)
    return Prob(getCenter(p), vals)
end

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
