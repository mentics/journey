module CmdUtil
using Dates
using SH, BaseTypes, StatusTypes, ChainTypes, OptionMetaTypes
using LogUtil, CollUtil
using StoreTrade
using Chains, Expirations

ERROR: not in use

export xlms, xlmsv

# TODO: remove after fixing calcs to not need it
TexPerDay = 6.5 + .3 * (24 - 6.2)

# xlmsv(ex::Int=0)::Vector{LegMeta} = tos(Vector{LegMeta}, tradesToClose(ex))
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

import Positions, StoreOrder, LegTypes
using TradierAccount, OrderTypes, OptionTypes, TradierOrderConvert
export entryFilterOption, entryFilterLeg
function entryFilterOption()
    d = entryFilterLookup()
    function c1(opt, side::Side.T)
        status = get(d, getOption(opt), 0)
        return !(status != 0 && (status == 2 || status != Int(side)))
    end
end
function entryFilterLeg()
    d = entryFilterLookup()
    function c1(leg)
        status = get(d, getOption(leg), 0)
        side = getSide(leg)
        return !(status != 0 && (status == 2 || status != Int(side)))
    end
end

function entryFilterLookup()
    ords = filter!(x->!SH.isStatus(x, Deleted), tos(Order, ta.tradierOrders()))
    legsPos = Iterators.map(getLeg, Positions.positions(; age=Minute(1)))
    legsOrds = Iterators.map(getLeg, flatmap(getLegs, ords))
    d = Dict{Option,Int}()
    for leg in flat(legsPos, legsOrds)
        opt = getOption(leg)
        side = Int(getSide(leg))
        status = get(d, opt, 0)
        d[opt] = status == 0 ? side : (status == side ? status : 2)
    end
    return d
end

using TradeTypes
function greeksPos(xprs=1:21)
    trades = filter(t -> xp.whichExpir(getTargetDate(t)) in xprs, StoreTrade.tradesOpen())
    return getGreeks(trades)
    # return isempty(trades) ? zeros(4) : mapreduce(greeks, .+, trades)
end

# apply(x, fs...) = map(f -> f(x), fs)
# apply(x, fs...) = map(f -> f(x), fs)

# SH.getDelta(trade::Trade) = SH.getDelta(Quoting.requote(optQuoter, getLegs(trade), Action.close))
# SH.getGamma(trade::Trade) = SH.getGamma(Quoting.requote(optQuoter, getLegs(trade), Action.close))
# SH.getTheta(trade::Trade) = SH.getTheta(Quoting.requote(optQuoter, getLegs(trade), Action.close))
# SH.getVega(trade::Trade) = SH.getVega(Quoting.requote(optQuoter, getLegs(trade), Action.close))
SH.getGreeks(trade::Trade)::Greeks = getGreeks(Quoting.requote(optQuoter, getLegs(trade), Action.close))

# export deltaPos
# function deltaPos(xprs=1:21)
#     trades = filter(t -> xp.whichExpir(getTargetDate(t)) in xprs, StoreTrade.tradesOpen())
#     isempty(trades) ? "No trades" : sum(SH.getDelta, trades)
# end

export texPY
texPY(from, leg) = cal.texToYear(calcTex(from, getExpir(leg)))

end