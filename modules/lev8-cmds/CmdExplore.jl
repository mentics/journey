module CmdExplore
using Dates
using SH, Globals, BaseTypes, SmallTypes, RetTypes, StratTypes, LegMetaTypes, ConstructUtil
using Shorthand, Between
using Expirations, Markets, Chains
using DrawStrat

export sh, shc, shRet, shVals, drsh, drsh!, shLegs # shLegs is reexported form Shorthand
export drlms, drlms!

drlms(lms; kws...) = drawRet(combineTo(Ret, lms, minimum(getExpiration.(lms)), market().startPrice, getvr()); curp=market().curp, kws...)
drlms!(lms; kws...) = drawRet!(combineTo(Ret, lms, minimum(getExpiration.(lms)), market().startPrice, getvr()); kws...)

export findShortsToClose
# TODO: move this to scheduled
function findShortsToClose()
    filter(positions(;age=Second(0))) do p
        oq = optQuoter(getLeg(p), Action.close)
        return getSide(p) == Side.short && abs(getBid(oq)) < .02
    end
end

shc(args...) = Tuple(lr for lr in sh(args...))
sh(str::AStr, exs::Int...) = sh(str, expir.(exs))
sh(str::AStr, exps::Coll{Date}=expirs())::Vector{LegMeta} = tos(LegMeta, shLegs(str, collect(exps)), optQuoter)
function shlr(str::AStr, exps=expirs(), sp=market().startPrice)
    lms = sh(str, exps)
    exp = minimum(getExpiration, lms)
    vr = Globals.get(:vtyRatio)
    [(lm, to(Ret, lm, exp, sp, vr)) for lm in lms]
end
shRet(str::AStr, exps, sp=market().startPrice) = combineTo(Ret, shlr(str, exps, sp))
shVals(str::AStr, exps, sp=market().startPrice) = getVals(shRet(str, exps, sp))
drsh(str::AStr, ex::Int=1; kws...) = drsh(str, expirs()[ex:ex+2]; kws...) # (sp = market().startPrice ; drawRet(shRet(str, expirs()[ex:ex+2], sp), nothing, sp, "sh") )
drsh(str::AStr, exps; kws...) = (sp = market().startPrice ; drawRet(shRet(str, exps, sp); kws..., curp=sp, label="sh") )
drsh!(str::AStr, ex::Int=1; kws...) = drsh!(str, expirs()[ex:ex+2])
drsh!(str::AStr, exps; kws...) = (sp = market().startPrice ; drawRet!(shRet(str, exps, sp); label="sh+") )

# TODO: need to move all probs calcs to util
# using ProbHist
# function locprobs(sp, exps)
#     numDays = mktNumDays(minimum(exps))
#     phOrig = probHist(sp, numDays)
#     ph = Prob(getCenter(phOrig), smooth(getVals(phOrig)))
# end

using StratTypes
export findSpreads, findSpread
findSpreads(spreads, combi::Combi) = ( findSpread(spreads, (combi[1], combi[2])), findSpread(spreads, (combi[3], combi[4])) )
findSpread(spreads, lr::NTuple{2,LegRet}) = findfirst(s -> s == lr, spreads)

# using CmdStrats, StratGen, Positions
# function whyNotCombi(combi::Combi)
#     inspreads = findSpreads(CmdStrats.lastSpreads2[], combi)
#     # @info "Was in spreads?" inspreads
#     oqs = optQuoter.(tos(LegMeta, combi))
#     vo = StratGen.validOption(Globals.get(:Strats)[:maxStrikeDist], market().curp)
#     optInvalid = findfirst(map(!, vo.(oqs)))
#     @info "Options valid?" optInvalid
#     legsPos = getLeg.(positions())
#     conflict = map(lr -> isConflict.(getLeg(lr), legsPos), combi)
#     @info "Is position conflict?" conflict # Tuple(c for c in conflict)

#     if (inspreads == (nothing,nothing))
#         answer = "because: \n"
#         if !isnothing(optInvalid) # optsValid != (false, false, false, false)
#             answer *= "  first invalid option found "
#             answer *= string(oqs[optInvalid])
#             answer *= '\n'
#         end
#         if conflict != (false, false, false, false)
#             ind = findfirst(conflict)
#             answer *= "  conflicted with position "
#             answer *= string(find(p -> isConflict(combi[ind], getLeg(p)), legsPos))
#             answer *= '\n'
#         end
#         println("Was not in spreads $(answer)")
#     else
#         println("TODO: was in spreads")
#     end
# end

using ThreadPools
using ProbTypes
using CollUtil, DateUtil, ThreadUtil
using SnapUtil, CmdUtil
const lock = ReentrantLock()
const stop = Ref(false)
abort() = stop[] = true

function searchDates()
    global PairSearch = Dict()
    global stop[] = false

    exps = reverse!(filter(x -> x < today(), SnapUtil.snapExpirs()))
    twith(ThreadPools.StaticPool(2, 20)) do pool
        for exp in exps[1:20]
            dates = reverse!(filter(x -> x < exp, SnapUtil.snapDates()))
            for backNear in 2:10
                try
                    @tthreads pool for backFar in (backNear+1):20
                        println("Running $(exp) $(backNear) $(backFar) on thread $(Threads.threadid())")
                        searchPairCondors(exp, dates[backNear], dates[backFar])
                    end
                catch e
                    abort()
                    rethrow(e)
                end
            end
        end
    end

    global spairs = collect(PairSearch)
    # sort!(spairs; rev=true, by=pair -> ( typeof(pair.second) == String || !haskey(pair.second, :prob) ? (println(dump(pair)) ; -100.0) : pair.second.prob ));
    sort!(spairs; rev=true, by=pair -> pair.second.evr * pair.second.prob);
    return spairs
end

function searchPairCondors(exp, dateNear, dateFar)
    # println("Searching for dates: ", dateNear, ' ', dateFar)
    snNear = SnapUtil.snapName(dateNear, 2)
    snFar = SnapUtil.snapName(dateFar, 2)

    # evrMax = -Inf
    # configMax = nothing
    # cnt = 1
    for midShort in 6:24
        for midLong in 2:(midShort-2)
            for wShort in 1:12
                # w2 = w1
                for wLong in 1:min(4, midShort-midLong-1)
                    !stop[] || error("stop")
                    args = (exp, snNear, snFar, midShort, wShort, midLong, wLong)
                    met, lms = pairCondor(args...)
                    if met.evr > 0.0 && met.prob > 0.7
                        runSync(lock) do
                            PairSearch[args] = met
                        end
                    end
                    # if met.evr > evrMax
                    #     evrMax = met.evr
                    #     configMax = args
                    #     # @info "Found" mxMax evrMax configMax
                    # end
                    # cnt % 1000 != 0 || println(cnt)
                    # cnt += 1
                end
            end
        end
    end
    # return (evrMax, configMax)
end

function pairCondor(exp::Date, snNear::String, snFar::String, midFar::Int, wFar::Int, midNear::Int, wNear::Int, show=false)
    curpNear = Markets.marketSnap(snNear).curp
    curpFar = Markets.marketSnap(snFar).curp
    oqssNear = Chains.getOqss(Chains.chainSnap(snNear, false)[exp].chain, curpNear; noLimit=true)
    oqssFar = Chains.getOqss(Chains.chainSnap(snFar, false)[exp].chain, curpFar; noLimit=true)
    lmsNear = CmdUtil.findCondor(oqssNear, curpFar, Side.long, midNear, wNear)
    lmsFar = CmdUtil.findCondor(oqssFar, curpFar, Side.short, midFar, wFar)
    lms = vcat(lmsFar, lmsNear)
    if show
        drlms(lmsFar; label="far");
        drlms!(lmsNear; label="near");
        drlms!(lms; label="both");
    end
    met = calcMetrics(snNear, exp, curpNear, lms)
    # global oqssNear1 = oqssNear
    # global oqssFar1 = oqssFar
    # @info "pairCondor" curpNear curpFar midFar wFar midNear wNear
    return met, lms
end

function checkPair(pair)
    (exp, snNear, snFar, midFar, wFar, midNear, wNear) = pair[1]
    # met = pair[2]

    snap(snFar)
    curpFar = market().curp
    oqssFar = Chains.getOqss(chains()[exp].chain, curpFar; noLimit=true)
    lmsFar = CmdUtil.findCondor(oqssFar, curpFar, Side.short, midFar, wFar)

    snap(snNear)
    curpNear = market().curp
    oqssNear = Chains.getOqss(chains()[exp].chain, curpNear; noLimit=true)
    lmsNear = CmdUtil.findCondor(oqssNear, curpFar, Side.long, midNear, wNear)

    # snap(exp, 2)
    lms = vcat(lmsFar, lmsNear)
    drlms(lmsFar; label="far");
    drlms!(lmsNear; label="near");
    drlms!(lms; label="both");
    met = calcMetrics(snNear, exp, curpNear, lms)
    # @info "checkPair" curpNear curpFar midFar wFar midNear wNear
    # global oqssNear2 = oqssNear
    # global oqssFar2 = oqssFar
    return (met, lms)
end

using CalcUtil, Bins
function CalcUtil.calcMetrics(snapName::String, exp::Date, curp::Currency, lms; bins=Bins.inds())
    # targetDate = minimum(getExpiration, lms)
    prob = probsFor(snapName, exp, curp)[1]
    ret = combineTo(Ret, lms, exp, curp)
    return calcMetrics(prob, ret, bins)
end

import Snapshots
using DictUtil
function report()
    global rep = Dict()
    for pair in spairs[1:100]
        (exp, snNear, snFar, midFar, wFar, midNear, wNear) = pair[1]
        tsNear = Snapshots.snapToTs(snNear)
        tsFar = Snapshots.snapToTs(snFar)
        daysFar = bdays(Date(tsFar), exp)
        daysNear = bdays(Date(tsNear), exp)
        incDictKey(rep, :daysFar, daysFar)
        incDictKey(rep, :daysNear, daysNear)
        incDictKey(rep, :midFar, midFar)
        incDictKey(rep, :wFar, wFar)
        incDictKey(rep, :midNear, midNear)
        incDictKey(rep, :wNear, wNear)
    end
    for k in keys(rep)
        v = sort!(collect(rep[k]); rev=true, by=x -> x.second)
        println("$(k) => $(v)")
    end
    return rep
end

end