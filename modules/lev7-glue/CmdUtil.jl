module CmdUtil
using Dates
using StatusTypes, ChainTypes
using LogUtil
using StoreTrade
using Expirations

export tradesToClose, xlms, xlmsv

# TODO: remove after fixing calcs to not need it
TexPerDay = 6.5 + .3 * (24 - 6.2)

tradesToClose(ex::Int=0) = tradesToClose(expir(ex)) # findTrades(expir(ex; td=true), Filled,Closing,PartialClosed)
tradesToClose(exp::Date) = findTrades(exp, Filled, Closing, PartialClosed)

# TODO: must be moved, wrong level
export calcPosStrat
using SH, BaseTypes, RetTypes, LegMetaTypes, StratTypes
using StoreTrade
function calcPosStrat(forDate::Date, sp::Currency, vtyRatio::Float64, extra::Union{Nothing,Vector{LegMeta}}=nothing)::Vector{LegRet}
    lms = xlms(forDate)
    if !isempty(lms)
        isnothing(extra) || append!(lms, extra)
        sort!(lms; by=getStrike)
        # return [(lm, to(Ret, lm, forDate, sp, vtyRatio)) for lm in lms]
        return tos(LegRet, lms, forDate, sp, vtyRatio)
    else
        return Vector{LegRet}()
    end
end

xlmsv(ex::Int=0)::Vector{LegMeta} = tos(Vector{LegMeta}, tradesToClose(ex))
xlms(ex::Int=0)::Vector{LegMeta} = xlms(expir(ex))
xlms(expr::Date)::Vector{LegMeta} = combineTo(Vector{LegMeta}, tradesToClose(expr))
# function xlms(when::Int=0)::Vector{LegMeta}
#     combineTo(Vector{LegMeta}, tradesToClose(when))
#     # trades = findTrades(forDate, Filled)
#     # if !isempty(trades)
#     #     lms = tos(Vector{LegMeta}, trades)
#     #     sort!(lms; by=getStrike)
#     #     return lms
#     # else
#     #     return Vector{LegMeta}()
#     # end
# end

export probsFor
using ProbTypes
using OptionUtil, ProbUtil, VectorCalcUtil
using Markets, Calendars, ProbHist
probsFor(i::Int; kws...) = probsFor(expir(i); kws...)
# probsFor(exp::Date; kws...) = makeProbs(calcTex(market().tsMarket, exp), exp; kws...)
using ProbKde
probsFor(exp::Date; kws...) = [probKde(Float64(market().curp), calcTex(market().tsMarket, exp), Float64(market().vix))]

using DictUtil
import Snapshots
const PROBS_SNAP2 = Dict{Tuple{String, Date, Currency},Tuple{Prob,Prob}}()
probsFor(snapName::String, to::Date, curp::Real) = useKey(PROBS_SNAP2, (snapName, to, curp)) do
    chains = Chains.chainSnap(snapName)
    tex = calcTex(Snapshots.snapToTs(snapName), to)
    nearIv = calcNearIv(to, chains)
    (!isnothing(nearIv) && isfinite(nearIv)) || @logret "makeProbs: Invalid ivsd" ivsd nearIv tex targetDate curp
    ivsd = ivTexToStdDev(nearIv, tex)
    pnd = probsNormDist(curp, ivsd)
    # TODO: this numdays proxy calc is wrong. Completely change how we calc probHist, do it based on tex
    phOrig = probHist(curp, round(Int, tex / TexPerDay))
    ph = Prob(getCenter(phOrig), smooth(getVals(phOrig)))
    return (ph, pnd)
end

function makeProbs(tex::Float64, targetDate::Date; curp::Currency=market().curp)::Union{Nothing,Tuple}
    # nearIv = calcNearIv(targetDate, curp)
    # (!isnothing(nearIv) && isfinite(nearIv)) || @logret "makeProbs: Invalid ivsd" nearIv targetDate curp
    # ivsd = ivTexToStdDev(nearIv, tex)
    # pnd = probsNormDist(curp, ivsd)
    # TODO: this numdays proxy calc is wrong. Completely change how we calc probHist, do it based on tex
    phOrig = probHist(curp, round(Int, tex / TexPerDay))
    ph = Prob(getCenter(phOrig), smooth(getVals(phOrig)))
    probs = (ph,)

    for probInd in eachindex(probs)
        prob = probs[probInd]
        pvals = getVals(prob)
        for i in eachindex(pvals)
            @assert isfinite(pvals[i]) "Prob $(probInd) had a bad data $(i)"
        end
    end
    return probs
end

using Bins
probFlat(p::Prob)::Prob = ( vals = getVals(p) ; probFlat(getCenter(p), (vals[1] + vals[end])/2) )
function probFlat(center::Real, ends::Float64)::Prob
    vals = Bins.with((1.0 - 2*ends) / Bins.NUM) # this is implemented hacky anyway and probably get removed, so... ok to call const here
    vals[1] = ends
    vals[end] = ends
    # normalize!(vals)
    return Prob(center, vals)
end

using Chains, SmallTypes, LegMetaTypes
# function findCondor(oqss::Oqss, curp::Currency, side::Side.T, mid, w; maxDiff=1.0)
#     if side == Side.long
#         distsLong = [-mid-w, mid+w] ./ 2
#         distsShort = [-mid, mid] ./ 2
#     else
#         distsShort = [-mid-w, mid+w] ./ 2
#         distsLong = [-mid, mid] ./ 2
#     end

#     oqssLong = Chains.findOqs(oqss.call.long, curp::Currency, distsLong; maxDiff)
#     !isnothing(oqssLong) || return nothing
#     longs = map(oq -> LegMeta(oq, 1.0, Side.long), oqssLong)

#     oqssShort = Chains.findOqs(oqss.call.short, curp::Currency, distsShort; maxDiff)
#     !isnothing(oqssShort) || return nothing
#     shorts = map(oq -> LegMeta(oq, 1.0, Side.short), oqssShort)

#     return sort!(vcat(shorts, longs); by=getStrike)
# end

makeLeg(oqs, ind, side) = 0 < ind < length(oqs) ? LegMeta(oqs[ind], 1.0, side) : nothing

function makeCondorCall(oqss::Oqss, mids, toInner::Int, toOuter::Int)::Union{Nothing,Coll{LegMeta}}
    innerUp = toInner ÷ 2
    innerDown = toInner - innerUp
    res = (makeLeg(oqss.call.short, mids.callShort - innerDown - toOuter, Side.short),
        makeLeg(oqss.call.long, mids.callLong - innerDown, Side.long),
        makeLeg(oqss.call.long, mids.callLong + innerUp, Side.long),
        makeLeg(oqss.call.short, mids.callShort + innerUp + toOuter, Side.short))
    return isnothing(findfirst(isnothing, res)) ? res : nothing
end

function makeCondorPut(oqss::Oqss, mids, toInner::Int, toOuter::Int)::Union{Nothing,Coll{LegMeta}}
    innerUp = toInner ÷ 2
    innerDown = toInner - innerUp
    res = (makeLeg(oqss.put.short, mids.callShort - innerDown - toOuter, Side.short),
        makeLeg(oqss.put.long, mids.callLong - innerDown, Side.long),
        makeLeg(oqss.put.long, mids.callLong + innerUp, Side.long),
        makeLeg(oqss.put.short, mids.callShort + innerUp + toOuter, Side.short))
    return isnothing(findfirst(isnothing, res)) ? res : nothing
end

function makeCondorIron(oqss::Oqss, mids, toInner::Int, toOuter::Int)::Union{Nothing,Coll{LegMeta}}
    innerUp = toInner ÷ 2
    innerDown = toInner - innerUp
    res = (makeLeg(oqss.put.short, mids.callShort - innerDown - toOuter, Side.short),
        makeLeg(oqss.put.long, mids.callLong - innerDown, Side.long),
        makeLeg(oqss.call.long, mids.callLong + innerUp, Side.long),
        makeLeg(oqss.call.short, mids.callShort + innerUp + toOuter, Side.short))
    return isnothing(findfirst(isnothing, res)) ? res : nothing
end

# makeCondorIron(oqss::Oqss, mids, toInner::Int, toOuter::Int)::Coll{LegMeta} = (
#     LegMeta(oqss.put.short[mids.callShort - toInner - toOuter], 1.0, Side.short),
#     LegMeta(oqss.put.long[mids.callLong - toInner], 1.0, Side.long),
#     LegMeta(oqss.call.long[mids.callLong + toInner], 1.0, Side.long),
#     LegMeta(oqss.call.short[mids.callShort + toInner + toOuter], 1.0, Side.short))

end