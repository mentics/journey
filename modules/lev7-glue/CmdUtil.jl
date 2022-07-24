module CmdUtil
using Dates
using StatusTypes
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

xlmsv(ex=0) = tos(Vector{LegMeta}, tradesToClose(ex))
function xlms(when=0)::Vector{LegMeta}
    combineTo(Vector{LegMeta}, tradesToClose(when))
    # trades = findTrades(forDate, Filled)
    # if !isempty(trades)
    #     lms = tos(Vector{LegMeta}, trades)
    #     sort!(lms; by=getStrike)
    #     return lms
    # else
    #     return Vector{LegMeta}()
    # end
end

export probsFor
using ProbTypes
using OptionUtil, ProbUtil, VectorCalcUtil
using Markets, Calendars, ProbHist
probFor(i::Int; kws...) = probsFor(i; kws...)[1]
probsFor(i::Int; kws...) = probsFor(expir(i); kws...)
probsFor(exp::Date; kws...) = makeProbs(calcTex(market().tsMarket, exp), exp; kws...)

using DictUtil
import Snapshots
const PROBS_SNAP2 = Dict{Tuple{String, Date, Currency},Tuple{Prob,Prob}}()
probsFor(snapName::String, to::Date, curp::Real) = useKey(PROBS_SNAP2, (snapName, to, curp)) do
    chains = Chains.chainSnap(snapName)
    tex = calcTex(Snapshots.snapToTs(snapName), to)
    ivsd = ivTexToStdDev(calcNearIv(to, chains), tex)
    pnd = probsNormDist(curp, ivsd)
    # TODO: this numdays proxy calc is wrong. Completely change how we calc probHist, do it based on tex
    phOrig = probHist(curp, round(Int, tex / TexPerDay))
    ph = Prob(getCenter(phOrig), smooth(getVals(phOrig)))
    return (ph, pnd)
end

function makeProbs(tex::Float64, targetDate::Date; curp::Currency=market().curp)::Tuple
    ivsd = ivTexToStdDev(calcNearIv(targetDate), tex)
    # shift = ivsd/2
    pnd = probsNormDist(curp, ivsd)
    # pndL = probsNormDist(sp, ivsd, -shift)# + .25 * numDays * .05))
    # pndR = probsNormDist(sp, ivsd, shift)# + .25 * numDays * .05))
    # probs = (pnd, pndL, pndR)
    # TODO: this numdays proxy calc is wrong. Completely change how we calc probHist, do it based on tex
    phOrig = probHist(curp, round(Int, tex / TexPerDay))
    # pvals = getVals(phOrig)
    ph = Prob(getCenter(phOrig), smooth(getVals(phOrig)))
    return (ph, pnd)
    # pideal = Scoring.probIdeal(ph)

    # s = 0.0
    # i = 0
    # while (s < .5)
    #     i += 1
    #     s += pvals[i]
    # end
    # # mu = sp * Bins.x(i)

    # pndsh = probsNormDist(sp, ivsd, Bins.x(i) - 1.0)
    # probs = (pideal, ph, pndsh)
    # probs = (pndsh + ph,)
    # probs = (pndsh, pnd, ph)
    # pflat = probFlat(Float64(curp), 0.0) # pnd[1]/2)
    # probs = (pnd + pflat,)
    # probs = (pnd,)
    # probs = (pflat,)
    # pflat = probRoof(Float64(sp), pnd[1]/2)
    # pflat = probFlat(getCenter(pnd), pnd.vals[1])
    # pshort = probMid(ph, binMin(), 1.0)
    # plong = probMid(ph, 1., binMax())
    # pmid = probMid(ph, .5*(1.0+binMin()), .5*(1.0+binMax()))
    # ppos = isnothing(lastPosRet[]) ? nothing : retToProb(lastPosRet[])
    # pposInv = isnothing(lastPosRet[]) ? nothing : invert(ppos)
    # pposHyb = Prob(getCenter(ph), normalize!(getVals(ph) .+ (getVals(pposInv) .* 2)))
    # probs = (ph,pnd)
    return probs
end

using Chains, SmallTypes, LegMetaTypes
# oqss = getOqs(exp, [], curp)
function findCondor(oqss::Oqss, curp::Currency, side::Side.T, mid, w)
    if side == Side.long
        distsLong = [-mid-w, mid+w]
        distsShort = [-mid, mid]
    else
        distsShort = [-mid-w, mid+w]
        distsLong = [-mid, mid]
    end
    longs = map(oq -> LegMeta(oq, 1.0, Side.long), Chains.findOqs(oqss.call.long, curp::Currency, distsLong))
    shorts = map(oq -> LegMeta(oq, 1.0, Side.short), Chains.findOqs(oqss.call.short, curp::Currency, distsShort))
    return sort!(vcat(shorts, longs); by=getStrike)
end

end