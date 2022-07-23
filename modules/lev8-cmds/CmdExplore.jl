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

# spairs = sort(ps; rev=true, by=pair -> ( typeof(pair.second) == String || !haskey(pair.second, :evr) ? (println(dump(pair)) ; -100.0) : pair.second.evr ));

function searchDates()
    global PairSearch = Dict()
    global stop[] = false

    exps = reverse!(filter(x -> x < today(), SnapUtil.snapExpirs()))
    twith(ThreadPools.StaticPool(2, 1)) do pool
        for exp in exps[1:1]
            dates = reverse!(filter(x -> x < exp, SnapUtil.snapDates()))
            @tthreads pool for backNear in 2:6
                println("Running $(backNear) on thread $(Threads.threadid())")
                for backFar in (backNear+1):12
                    searchPairCondors(exp, dates[backNear], dates[backFar])
                end
            end
        end
    end
end

function searchPairCondors(exp, dateNear, dateFar)
    # println("Searching for dates: ", dateNear, ' ', dateFar)
    snNear = SnapUtil.snapName(dateNear, 2)
    snFar = SnapUtil.snapName(dateFar, 2)

    evrMax = -Inf
    configMax = nothing
    cnt = 1
    for midShort in 8:18
        for midLong in 4:(midShort-4)
            for wShort in 1:4
                # w2 = w1
                for wLong in 1:min(4, midShort-midLong-1)
                    !stop[] || error("stop")
                    args = (exp, snNear, snFar, midLong, wLong, midShort, wShort)
                    met = pairCondor(args...)
                    runSync(lock) do
                        PairSearch[args] = met
                    end
                    if met.evr > evrMax
                        evrMax = met.evr
                        configMax = args
                        # @info "Found" mxMax evrMax configMax
                    end
                    cnt % 1000 != 0 || println(cnt)
                    cnt += 1
                end
            end
        end
    end
    return (evrMax, configMax)
end

function pairCondor(exp::Date, sn1::String, sn2::String, mid1::Int, w1::Int, mid2::Int, w2::Int, show=false)
    curp1 = Markets.marketSnap(sn1).curp
    oqss1 = Chains.getOqss(Chains.chainSnap(sn1, false)[exp].chain, curp1)
    oqss2 = Chains.getOqss(Chains.chainSnap(sn2, false)[exp].chain, curp1)
    lms1 = CmdUtil.findCondor(oqss1, curp1, Side.long, mid1, w1)
    lms2 = CmdUtil.findCondor(oqss2, curp1, Side.short, mid2, w2)
    lms = vcat(lms1, lms2)
    if show
        drlms(lms1; label="d1");
        drlms!(lms2; label="d2");
        drlms!(lms; label="both");
    end
    curp2 = Markets.marketSnap(sn2).curp
    return calcMetrics(curp2, lms)
    # return lms
end

function checkPair(pair)
    (exp, snNear, snFar, midLong, wLong, midShort, wShort) = pair[1]
    # (exp, snNear, snFar, mid1, mid2, w1, w2) = pair[1]
    met = pair[2]
    snap(snNear)
    curp1 = market().curp
    oqss1 = Chains.getOqss(chains()[exp].chain, curp1)
    lms1 = CmdUtil.findCondor(oqss1, curp1, Side.long, midLong, wLong)
    snap(snFar)
    curp2 = market().curp
    oqss2 = Chains.getOqss(chains()[exp].chain, curp1)
    lms2 = CmdUtil.findCondor(oqss2, curp1, Side.short, midShort, wShort)
    snap(exp, 2)
    drlms(lms1; label="d1");
    drlms!(lms2; label="d2");
    drlms!(vcat(lms1, lms2); label="both");
    println(met)
    return (lms1, lms2)
end

using CalcUtil, Bins
function CalcUtil.calcMetrics(curp::Currency, lms; bins=Bins.inds())
    targetDate = minimum(getExpiration, lms)
    prob = probsFor(targetDate; curp)[1]
    ret = combineTo(Ret, lms, targetDate, curp)
    return calcMetrics(prob, ret, bins)
end

end