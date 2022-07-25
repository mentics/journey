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

function preloadSnapCache()
    empty!(Chains.CHAINS_SNAP)
    empty!(Markets.MARKETS_SNAP)
    names = filter(!isnothing, map(x -> SnapUtil.snapName(x, 2), SnapUtil.snapDates()))
    names = filter(!isnothing, map(x -> SnapUtil.lastSnap(x), SnapUtil.snapDates()))
    for name in names
        Chains.chainSnap(name, false)
        Markets.marketSnap(name, false)
    end
end

function searchDates()
    if length(Chains.CHAINS_SNAP) < 100
        println("Warning chains snap doesn't appear to be preloaded. Need to do that because of multithreading.")
        return
    end
    global PairSearch = Dict()
    global stop[] = false
    snaps = SnapUtil.snapDates()

    backs = NTuple{2,Int}[]
    for near in 1:8, far in (near+1):16
        push!(backs, (near, far))
    end
    println("length backs ", length(backs))
    # backs = [(2, 15)]
    # backs = backs[1:10]

    exps = reverse!(filter(x -> x < today(), SnapUtil.snapExpirs()))
    twith(ThreadPools.QueuePool(2, 10)) do pool
        for exp in exps[1:1]
            @tthreads pool for (backNear, backFar) in backs
            # for backNear in 2 # 1:10
                try
                    # for backFar in 15 # (backNear+1):20
                        near = bdaysBefore(exp, backNear)
                        far = bdaysBefore(exp, backFar)
                        nearOk = near >= MinDate && near in snaps
                        farOk = far >= MinDate && far in snaps
                        if nearOk && farOk
                            println("Running $(exp) $(backNear) $(backFar) on thread $(Threads.threadid())")
                            searchPairCondors(exp, near, far)
                        else
                            println("Skipping due to missing date $(near):$(nearOk) $(far):$(farOk)")
                        end
                    # end
                catch e
                    abort()
                    rethrow(e)
                end
            end
        end
    end

    global spairs = collect(PairSearch)
    # sort!(spairs; rev=true, by=pair -> ( typeof(pair.second) == String || !haskey(pair.second, :prob) ? (println(dump(pair)) ; -100.0) : pair.second.prob ));
    # sort!(spairs; rev=true, by=pair -> pair.second.evr * pair.second.prob);
    sort!(spairs; rev=true, by=pair -> pair.second.evr);
    return spairs
end

function searchPairCondors(exp, dateNear, dateFar)
    snNear = SnapUtil.snapName(dateNear, 2)
    snFar = SnapUtil.snapName(dateFar, 2)
    curpNear = Markets.marketSnap(snNear).curp
    curpFar = Markets.marketSnap(snFar).curp
    binFarMid = Bins.nearest(Float64(curpFar / curpNear))
    # println("binFarMid ", binFarMid)

    # cnt = 1
    for midShort in 6:22
        for midLong in max(2, midShort-8):(midShort-2)
            for wShort in 1:12
                mid = midShort-midLong-1
                for wLong in 1:min(4, )
                    !stop[] || error("stop")
                    args = (exp, snNear, snFar, midShort, wShort, midLong, wLong)
                    res = pairCondor(args...)
                    !isnothing(res) || continue # ( println("Condor not found for ", args) ; continue )
                    ret, met, _ = res
                    vals = getVals(ret)
                    okVals = vals[1] > 0.0 && vals[end] > 0.0 && vals[binFarMid] > 0.0
                    if okVals && met.evr > 0.0 && met.prob > 0.7
                        runSync(lock) do
                            PairSearch[args] = met
                        end
                    end
                    # cnt % 1000 != 0 || println(cnt)
                    # cnt += 1
                end
            end
        end
    end
    # return (evrMax, configMax)
end

function pairCondor(exp::Date, snNear::String, snFar::String, midFar::Int, wFar::Int, midNear::Int, wNear::Int; show=false)
    curpNear = Markets.marketSnap(snNear).curp
    curpFar = Markets.marketSnap(snFar).curp
    oqssNear = nothing
    oqssFar = nothing
    oqssNear = Chains.getOqss(Chains.chainSnap(snNear, false)[exp].chain, curpNear; noLimit=true)
    oqssFar = Chains.getOqss(Chains.chainSnap(snFar, false)[exp].chain, curpFar; noLimit=true)
    lmsNear = CmdUtil.findCondor(oqssNear, curpFar, Side.long, midNear, wNear)
    !isnothing(lmsNear) || return nothing
    lmsFar = CmdUtil.findCondor(oqssFar, curpFar, Side.short, midFar, wFar)
    !isnothing(lmsFar) || return nothing
    lms = vcat(lmsFar, lmsNear)
    if show
        drlms(lmsFar; label="far");
        drlms!(lmsNear; label="near");
        drlms!(lms; label="both");
    end

    ret, met = calcMetrics(snNear, exp, curpNear, lms)
    # ret, met = calcMetLast(exp, snNear)

    return (ret, met, lms)
end

function calcMetLast(exp, lms)
    name = SnapUtil.lastSnap(exp)
    curp = Markets.marketSnap(name).curp
    return calcMetrics(name, exp, curp, lms)
end

checkPair(pair) = checkArgs(pair[1]...; show=false)
function comparePair(pair)
    ret1, met1, lms1 = pairCondor(pair[1]...)
    ret2, met2, lms2 = checkArgs(pair[1]...)
    if lms1 != lms2
        @error "lms don't match" lms1 lms2
    end
    if met1 != met2
        @error "met don't match" met1 met2
    end
end

function checkArgs(exp, snNear, snFar, midFar, wFar, midNear, wNear; show=false)
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

    ret, met = calcMetrics(snNear, exp, curpNear, lms)
    # ret, met = calcMetLast(exp, snNear)

    if show
        drlms(lmsFar; label="far");
        drlms!(lmsNear; label="near");
        drlms!(lms; label="both");
    end
    return (ret, met, lms)
end

using CalcUtil, Bins
function CalcUtil.calcMetrics(snapName::String, exp::Date, curp::Currency, lms; bins=Bins.inds())
    # prob = CmdUtil.probFlat(curp, 0.0)
    prob = probsFor(snapName, exp, curp)[1]
    ret = combineTo(Ret, lms, exp, curp)
    return (ret, calcMetrics(prob, ret, bins))
end

import Snapshots
using DictUtil
function reportOld()
    global repold = Dict()
    for pair in spairs
        (exp, snNear, snFar, midFar, wFar, midNear, wNear) = pair[1]
        pair[2].evr > 0.1 || continue
        tsNear = Snapshots.snapToTs(snNear)
        tsFar = Snapshots.snapToTs(snFar)
        daysFar = bdays(Date(tsFar), exp)
        daysNear = bdays(Date(tsNear), exp)
        incDictKey(repold, :daysFar, daysFar)
        incDictKey(repold, :daysNear, daysNear)
        incDictKey(repold, :midFar, midFar)
        incDictKey(repold, :wFar, wFar)
        incDictKey(repold, :midNear, midNear)
        incDictKey(repold, :wNear, wNear)
    end
    for k in keys(repold)
        v = sort!(collect(repold[k]); rev=true, by=x -> x.second)
        println("$(k) => $(v)")
    end
    return repold
end

function report()
    global rep = Dict()
    for pair in spairs
        (exp, snNear, snFar, midFar, wFar, midNear, wNear) = pair[1]
        # pair[2].evr > 0.1 || continue
        daysNear = bdays(Date(Snapshots.snapToTs(snNear)), exp)
        daysFar = bdays(Date(Snapshots.snapToTs(snFar)), exp)
        incKey(rep, (daysNear, daysFar, midFar, wFar, midNear, wNear))
    end
    return rep
end

function bestForEachExp()
    exps = reverse!(filter(x -> x < today(), SnapUtil.snapExpirs()))
    for exp in exps[1:20]
        println(find(x -> x[1][1] == exp, spairs))
    end
end

function checkExps()
    argss = map(x -> x[1], collect(filter(x -> x[2] >= 12, rep)))
    exps = reverse!(filter(x -> x < today(), SnapUtil.snapExpirs()))
    res = Dict()
    for args in argss
        for exp in exps[1:20]
            met, lms = pairCondor(args2args(exp, args)...)
            if met.evr > 0.0
                incKey(res, args)
                # println(find(x -> x[1][1] == exp, spairs))
            end
        end
    end
    return res
end

tosn(exp::Date, daysBack::Int) = SnapUtil.snapName(bdaysBefore(exp, daysBack), 2)
args2args(exp::Date, args::Tuple) = (exp, tosn(exp, args[1]), tosn(exp, args[2]), args[3:end]...)

best(sym::Symbol) = argmax(rep[sym])
best() = (;daysNear=best(:daysNear), daysFar=best(:daysFar), midFar=best(:midFar), wFar=best(:wFar), midNear=best(:midNear), wNear = best(:wNear))

    # # (exp, snNear, snFar, midFar, wFar, midNear, wNear) = pair[1]
    # bestNt = best()
    # println("Using values: ", bestNt)

MinDate = Date(2022,4,26)
using OutputUtil
function validate(daysNear, daysFar, midFar, wFar, midNear, wNear)
    snaps = SnapUtil.snapDates()
    exps = reverse!(filter(x -> x < today(), SnapUtil.snapExpirs()))
    res = NamedTuple[]
    for exp in exps
        # dates = reverse!(filter(x -> x < exp, SnapUtil.snapDates()))
        near = bdaysBefore(exp, daysNear)
        far = bdaysBefore(exp, daysFar)

        if near >= MinDate && far >= MinDate && near in snaps && far in snaps
            try
                snNear = SnapUtil.snapName(near, 2)
                snFar = SnapUtil.snapName(far, 2)
                met, lms = pairCondor(exp, snNear, snFar, midFar, wFar, midNear, wNear)
                args = (exp, snNear, snFar, midFar, wFar, midNear, wNear)
                push!(res, (;exp, evr=met.evr, prob=met.prob, args))
            catch e
                @error "check" exp near far e
            end
        end
    end
    pretyble(res)
    return res
end

exp4days(days, from=today()) = ( date = bdaysAfter(from, days) ; date in expirs() || println("Not an expir") ; date )

function tradeNear(args)
    oqssNear = Chains.getOqss(Chains.chainSnap(snNear, false)[exp].chain, curpNear; noLimit=true)
    oqssFar = Chains.getOqss(Chains.chainSnap(snFar, false)[exp].chain, curpFar; noLimit=true)
    lmsNear = CmdUtil.findCondor(oqssNear, curpFar, Side.long, midNear, wNear)
    lmsFar = CmdUtil.findCondor(oqssFar, curpFar, Side.short, midFar, wFar)

end

# TODO: single call to recalculate best args
# TODO: Every other day won't match bdaysAfter, so check performance of +1 or -1 near and far
# 2022.07.25:
#  (15, 2, 6, 6, 3, 2)
#  (20, 2, 13, 4, 9, 3)
#  (15, 2, 6, 6, 3, 1)

end