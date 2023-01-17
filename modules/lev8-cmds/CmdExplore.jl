module CmdExplore
using Dates
using SH, Globals, BaseTypes, SmallTypes, RetTypes, StratTypes, LegMetaTypes
using LogUtil
using Shorthand, Between
using Expirations, Markets, Chains
using DrawStrat
using CmdPos

export ce
const ce = @__MODULE__

export sh, shc, shRet, shVals, drsh, drsh!, shLegs # shLegs is reexported form Shorthand
export drlms, drlms!

# drlms(lms; kws...) = drawRet(combineTo(Ret, lms, minimum(getExpiration.(lms)), market().startPrice, getvr()); curp=market().curp, kws...)
# drlms!(lms; kws...) = drawRet!(combineTo(Ret, lms, minimum(getExpiration.(lms)), market().startPrice, getvr()); kws...)
drlms(lms; kws...) = drawRet(combineTo(Ret, lms, market().startPrice); curp=market().curp, kws...)
drlms!(lms; kws...) = drawRet!(combineTo(Ret, lms, market().startPrice); kws...)
drlms!(lm::LegMeta; kws...) = drawRet!(to(Ret, lm, market().startPrice); kws...)

export findShortsToClose
# TODO: move this to scheduled
using Positions
function findShortsToClose()
    filter(positions(;age=Second(0))) do p
        oq = optQuoter(getLeg(p), Action.close)
        if getSide(p) == Side.short && abs(getBid(oq)) <= .02
            println("Found ", oq)
            return true
        end
        return false
    end
end

# shc(args...) = Tuple(lr for lr in sh(args...))
sh(str::AStr, exs::Int...) = sh(str, expir.(exs)...)
sh(str::AStr, exps::Date...)::Vector{LegMeta} = tos(LegMetaOpen, shLegs(str, collect(exps)), Chains.chainLookup)
function shlr(str::AStr, exps=expirs(), curp=market().startPrice)
    lms = sh(str, exps...)
    # exp = minimum(getExpiration, lms)
    # vr = Globals.get(:vtyRatio)
    # [(lm, to(Ret, lm, exp, curp, vr)) for lm in lms]
    [(lm, to(Ret, lm, curp)) for lm in lms]
end
# shRet(str::AStr, exps, sp=market().startPrice) = combineTo(Ret, shlr(str, exps, sp))
shRet(str::AStr, exps, curp=market().curp) = combineTo(Ret, sh(str, exps...), curp)
shVals(str::AStr, exps, curp=market().curp) = getVals(shRet(str, exps, curp))
drsh(str::AStr, exs::Int...; kws...) = drsh(str, getInds(expirs(), exs)...; kws...) # (sp = market().startPrice ; drawRet(shRet(str, expirs()[ex:ex+2], sp), nothing, sp, "sh") )
drsh(str::AStr, exps::Date...; kws...) = (sp = market().startPrice ; drawRet(shRet(str, exps, sp); kws..., curp=sp, label="sh") )
drsh!(str::AStr, exs::Int...; kws...) = drsh!(str, getInds(expirs(), exs)...; kws...)
drsh!(str::AStr, exps::Date...; kws...) = (sp = market().startPrice ; drawRet!(shRet(str, exps, sp); label="sh+") )

getInds(v, inds) = [v[i] for i in inds]

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
    names1 = Iterators.filter(!isnothing, map(x -> SnapUtil.snapName(x, 2), SnapUtil.snapDates()))
    names2 = Iterators.filter(!isnothing, map(x -> SnapUtil.lastSnap(x), SnapUtil.snapDates()))
    for name in Iterators.flatten((names1, names2))
        Chains.chainSnap(name, false)
        Markets.marketSnap(name, false)
    end
end

using FileUtil
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
    # backs = backs[1:4]

    exps = reverse!(filter(x -> x < today(), SnapUtil.snapExpirs()))
    twith(ThreadPools.QueuePool(2, 10)) do pool
        for exp in exps
            @tthreads pool for (backNear, backFar) in backs
            # for (backNear, backFar) in backs
                try
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
    writeJson("../spairs.json", spairs)
    return spairs
end
#==
using Globals
devon()
snap(1)
# using CmdExplore
xx = CmdExplore
xx.preloadSnapCache()
xx.searchDates()
 ==#

function searchPairCondors(exp, dateNear, dateFar)
    snNear = SnapUtil.snapName(dateNear, 2)
    snFar = SnapUtil.snapName(dateFar, 2)
    curpNear = Markets.marketSnap(snNear).curp
    curpFar = Markets.marketSnap(snFar).curp
    binFarMid = Bins.nearest(Float64(curpFar / curpNear))
    # println("binFarMid ", binFarMid)

    # cnt = 1
    for midShort in 6:22
        for wShort in 1:12
            maxTotLong = midShort + wShort - 1
            for midLong in max(1, maxTotLong-4):maxTotLong
                for wLong in 1:(maxTotLong-midLong)
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
    # println("per run ", cnt)
    # return (evrMax, configMax)
end

function pairCondor(exp::Date, snNear::String, snFar::String, midFar::Int, wFar::Int, midNear::Int, wNear::Int; show=false)
    curpNear = Markets.marketSnap(snNear).curp
    curpFar = Markets.marketSnap(snFar).curp
    Chains.oqssSnap(snNear, exp)
    oqssNear = Chains.oqssSnap(snNear, exp)
    # Chains.getOqss(Chains.chainSnap(snNear, false)[exp].chain, curpNear, true)
    oqssFar = Chains.oqssSnap(snFar, exp)
    # Chains.getOqss(Chains.chainSnap(snFar, false)[exp].chain, curpFar, true)
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

checkPair(pair) = checkArgs(pair[1]...; show=false)
function checkArgs(exp, snNear, snFar, midFar, wFar, midNear, wNear; show=false)
    snap(snFar)
    curpFar = market().curp
    oqssFar = Chains.getOqss(chains()[exp].chain)
    lmsFar = CmdUtil.findCondor(oqssFar, curpFar, Side.short, midFar, wFar)

    snap(snNear)
    curpNear = market().curp
    oqssNear = Chains.getOqss(chains()[exp].chain)
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
    prob = probsFor(snapName, exp, curp)
    ret = combineTo(Ret, lms, exp, curp)
    return (ret, calcMetrics(prob, ret, bins))
end

import Snapshots
using DictUtil

function bestForEachExp()
    exps = reverse!(filter(x -> x < today(), SnapUtil.snapExpirs()))
    for exp in exps[1:20]
        println(find(x -> x[1][1] == exp, spairs))
    end
end

tosn(exp::Date, daysBack::Int) = SnapUtil.snapName(bdaysBefore(exp, daysBack), 2)
function args2args(exp::Date, args::Tuple)
    sn1 = tosn(exp, args[1])
    sn2 = tosn(exp, args[2])
    !isnothing(sn1) && !isnothing(sn2) || return nothing
    return (exp, sn1, sn2, args[3:end]...)
end

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
                ret, met, lms = pairCondor(exp, snNear, snFar, midFar, wFar, midNear, wNear)
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


function report()
    track = Dict()
    for pair in spairs
        (exp, snNear, snFar, midFar, wFar, midNear, wNear) = pair[1]
        # pair[2].evr > 0.1 || continue
        daysNear = bdays(Date(Snapshots.snapToTs(snNear)), exp)
        daysFar = bdays(Date(Snapshots.snapToTs(snFar)), exp)
        incKey(track, (daysNear, daysFar, midFar, wFar, midNear, wNear))
    end
    global rep = sort!(collect(track); rev=true, by=x -> x[2])
    return rep
end

function checkExps()
    argss = map(x -> x[1], collect(filter(x -> x[2] >= 12, rep)))
    exps = reverse!(filter(x -> x < today(), SnapUtil.snapExpirs()))
    res = Dict()
    missedAags = 0
    missedConds = 0
    total = 0
    for args in argss
        for exp in exps
            aags = args2args(exp, args)
            !isnothing(aags) || ( missedAags += 1 ; continue )
            cond = pairCondor(aags...)
            !isnothing(cond) || ( missedConds += 1 ; continue )
            total += 1
            ret, met, lms = cond
            if met.evr > 0.0
                incKey(res, args)
                # println(find(x -> x[1][1] == exp, spairs))
            end
        end
    end
    @info "Missed" missedAags missedConds total length(exps)
    return res
end


# function tradeNear(args)
#     oqssNear = Chains.getOqss(Chains.chainSnap(snNear, false)[exp].chain, curpNear, true)
#     oqssFar = Chains.getOqss(Chains.chainSnap(snFar, false)[exp].chain, curpFar, true)
#     lmsNear = CmdUtil.findCondor(oqssNear, curpFar, Side.long, midNear, wNear)
#     lmsFar = CmdUtil.findCondor(oqssFar, curpFar, Side.short, midFar, wFar)

# end

# TODO: single call to recalculate best args
# TODO: Every other day won't match bdaysAfter, so check performance of +1 or -1 near and far
# 2022.07.25:
#  (15, 2, 6, 6, 3, 2)
#  (20, 2, 13, 4, 9, 3)
#  (15, 2, 6, 6, 3, 1)

using Intervals
const OpenInterval = Interval{Currency,Open,Open}
intervalFor(lms) = OpenInterval(extrema(getStrike, lms)...)

findNear(oqs, curp, near) = findfirst(x -> abs(getStrike(x) - curp) <= near, oqs)
findMids(oqss, curp, near) = (;
    callLong=findNear(oqss.call.long, curp, near),
    callShort=findNear(oqss.call.short, curp, near),
    putLong=findNear(oqss.put.long, curp, near),
    putShort=findNear(oqss.put.short, curp, near))

const lockBb = ReentrantLock()
function bb(ex)
    res = []
    expr = expir(ex)
    # @info "bb" market().startDay ex expr (expr - market().startDay) (expr - market().startDay < Day(200))
    expr - market().startDay < Day(200) || return
    MinProfit = .1
    MinProb = .9
    MinRate = .4
    lmsCur = xlms(ex)
    ivalsCur = intervalFor.(Iterators.partition(lmsCur, 4))

    curp = market().curp
    probs = CmdUtil.probsFor(expr)
    prob = probs[1]
    !isnothing(prob) || @logret "bb: Skipping expr" expr
    # pflat = CmdUtil.probFlat(curp, 0.0)

    oqss = Chains.getOqss(chains()[expr].chain, curp);
    # ( midCallLong, midCallShort, midPutLong, midPutShort )
    Threads.@threads for off in -12:24
        mids = findMids(oqss, curp + off, 0.5)
        isnothing(findfirst(isnothing, mids)) || continue
        # println(mids)
        # error("stop")
        for toInner in 1:10
            for toOuter in 1:(7 - (toInner ÷ 2 + 1))
                for lms in (
                    CmdUtil.makeCondorCall(oqss, mids, toInner, toOuter),
                    CmdUtil.makeCondorPut(oqss, mids, toInner, toOuter),
                    CmdUtil.makeCondorIron(oqss, mids, toInner, toOuter)
                )
                    !isnothing(lms) || continue
                    ival = intervalFor(lms)
                    # if !isempty(ivalsCur)
                    #     println("check ", typeof(ivalsCur))
                    #     println(ivalsCur)
                    #     isnothing(findfirst(Intervals.overlaps(ival, ivc) for ivc in ivalsCur)) || continue
                    #     error("stop")
                    # end
                    isnothing(findfirst(Intervals.overlaps(ival, ivc) for ivc in ivalsCur)) || continue
                    # lms = CmdUtil.findCondor(oqss, curp+off, Side.short, mid, w; maxDiff=5)
                    ret = combineTo(Ret, lms, curp)
                    met = calcMetrics(prob, ret)
                    profit = calcProfit(ret)
                    annual = 365 / (expr - today()).value
                    rate = annual * profit / (-met.mn)
                    isOther = met.prob > MinProb && rate > MinRate
                    if profit >= MinProfit && isOther
                        runSync(lockBb) do
                            push!(res, (;args=(mids, toInner, toOuter), lms=collect(lms), ret, met, rate, prob, oqss))
                        end
                    end
                end
            end
        end
    end
    # sort!(res; rev=true, by=x -> x.met.prob * x.rate)
    sort!(res; rev=true, by=x -> x.met.evr)
    # res2 = filter(x -> x[4].mx >= .2, res)
    return res
end

using Calendars
function bball()
    Calendars.ensureCals(today(), expirs()[end])
    res = Dict()
    for ex in 1:20 # 20 is deliberate, it's about where the monthlies end TODO: look up exactly when they do
        y = bb(ex)
        isnothing(y) || (res[ex] = y)
    end
    global bbres = sort!(filter(x -> x[2] != [], collect(res)); by=x -> x[1])
    bbrep()
    return bbres
end

function bbrep()
    pretyble(map(rep, bbres))
    # pretyble(map(repboth, bbres))
end

function rep(o)
    x = o[2][1]
    ex = o[1]
    retBoth = combineTo(Ret, lmsb(ex), C(x.prob.center))
    metBoth = calcMetrics(x.prob, retBoth)
    # (;ex, exp=expir(ex), , profit=calcProfit(retBoth)) # , spread=calcWidth(lms), args=o.args)
    return (;ex, exp=expir(ex), rate=x.rate, prob=x.met.prob, probBoth=metBoth.prob, evr=x.met.evr, evrBoth=metBoth.evr, profit=calcProfit(x.ret), spread=calcWidth(x.lms), args=x.args)
end

# TODO: del
# function repboth(o)
#     x = o[2][1]
#     ex = o[1]
#     retBoth = combineTo(Ret, lmsb(ex), C(x.prob.center))
#     metBoth = calcMetrics(x.prob, retBoth)
#     (;ex, exp=expir(ex), prob=metBoth.prob, profit=calcProfit(retBoth)) # , spread=calcWidth(lms), args=o.args)
# end

# calcProfit(ret) = (ret[1] + ret[end]) / 2 - .02
calcProfit(ret) = min(ret[1], ret[end]) / 2 - .02
calcWidth(lms::Vector{LegMeta}) = ( (mn, mx) = extrema(getStrike, lms) ; return mx - mn )

lms(ex)::Vector{LegMeta} = find(x -> x[1] == ex, bbres)[2][1].lms
lmsb(ex)::Vector{LegMeta} = vcat(xlms(ex), lms(ex))

# TODO: add query to get all Filled trades not entered today so we can check annualized rate if close
# TODO: maybe add filter to avoid options that have low vol/interest
# TODO: try puts and both puts/calls
# TODO: we can enter multiple for same expr if their lows don't overlap because we try to close early

using StoreTrade, TradeInfo, StatusTypes, TradeTypes
function bbToClose()
    isnothing(snap()) || error("Can't show bbToClose when snapped")
    return filter(sort!(findTrades(Filled); by=getTargetDate)) do trade
        targ = getTargetDate(trade)
        !isempty(chain(targ)) || return false
        targ >= Date(2022,8,19) || return false
        ur = TradeInfo.urpnl(trade)
        mn, mx = TradeInfo.minMaxPnl(trade)
        annual = 365 / (targ - toDateMarket(tsFilled(trade))).value
        rate = annual * ur / (-mn)
        # if ur > 0.0 # mx / 2
            println("Rate for $(strShort(targ)) = $(nstr(rate)) ur:$(ur) maxp:$(mx) maxl:$(-mn)")
            # println(trade)
            # return true
        # end
        return false
    end
end

#region Adjusting
# ------------ Adjusting ---------------
import SmallTypes:Style.call,Style.put,Side.long,Side.short
function condorLong(xpir, curp)
    # return vcat(spreadLong(xpir, curp, -6, 1), spreadShort(xpir, curp, 5, 1))
    return vcat(spread(xpir, put, long, coff(curp, -6, 1)), spread(xpir, call, short, coff(curp, 5, 1)))
end
function condorShort(xpir, curp)
    return vcat(spread(xpir, put, short, coff(curp, -6, 1)), spread(xpir, call, long, coff(curp, 5, 1)))
end

# function condorShort(xpir, curp)
#     return vcat(spreadShort(xpir, curp, -6, 1), spreadLong(xpir, curp, 5, 1))
# end
# function condorShort(xpir, mid, w)
#     return vcat(spreadShort(xpir, mid, -(w+1), 1), spreadLong(xpir, mid, w, 1))
# end

function butter(xpir, curp)
    return vcat(spreadLong(xpir, curp, -1, 1), spreadShort(xpir, curp, 1, 1))
end

# function spreadLong(xpir, curp, off, w)
# 	m = floor(curp)
# 	return sh("l$(m+off)p / s$(m+off+w)p", [xpir])
# end
# function spreadShort(xpir, curp, off, w)
# 	m = floor(curp)
# 	return sh("s$(m+off)c / l$(m+off+w)c", [xpir])
# end

spread(xpir, style, side, s1, s2) = sh("$(toCode(side))$(s1)$(toCode(style)) / $(toCode(toOther(side)))$(s2)$(toCode(style))", [xpir])
spread(xpir, style, side, (s1, s2)) = spread(xpir, style, side, s1, s2)
cread(xpir, style, side, curp, w) = ( c = floor(curp) ; spread(xpir, style, side, c - w, c + w) )
coff(curp, off1, rel2) = ( c = floor(curp) ; return (c + off1, c + off1 + rel2) )
# spreadCurp(xpir, style, side, curp, off, w)
# spreadPL(xpir, )

function runadjust(f, init=condorLong)
    xpir = Date(2022,11,11)
    datestart = Date(2022,11,1)
    snap(datestart, 2)
    lmsinit = init(xpir, market().curp)
    qinit = quoter(lmsinit, Action.open)
    println("Initial open: $(qinit)")
    date = bdaysAfter(datestart, 1)
    lmscur = lmsinit
    net = bap(qinit)
    while date < xpir
        snap(date, 2)
        curp = market().curp
        println("Snapped to: ", snap(), " curp: ", curp)
        actions = f(xpir, curp, lmscur)
        netc = 0.0
        neto = 0.0
        for (i, replacement) in actions
            nc = bap(optQuoter(lmscur[i], Action.close))
            no = bap(optQuoter(replacement, Action.open))
            println("Closed $(i) for $(nc) and opened for $(no) for net: $(nc + no)")
            net += nc + no
            lmscur[i] = replacement
            netc += nc
            neto += no
        end
        println("net: ", net, " change: netc=", netc, ", neto=", neto)
        date = bdaysAfter(date, 1)
    end
    println("Snapped to final: ", snap(xpir, 12, 0))
    qfinal = quoter(lmscur, Action.close)
    println("Final close: $(qfinal)")
    net += bap(qfinal)
    global lmsfinal = lmscur
    println("Final net: ", net)
end

function stratNoop(xpir, curp, lmscur)
    return []
end

function strat1(xpir, curp, lmscur)
    if curp < getStrike(lmscur[1])
        return collect(zip(1:4, condorLong(xpir, curp)))
    elseif curp > getStrike(lmscur[end])
        return collect(zip(1:4, condorLong(xpir, curp)))
    end
    return []
end

function stratbutter(xpir, curp, lmscur)
    if curp < getStrike(lmscur[1])
        return collect(zip(1:4, butter(xpir, curp)))
    elseif curp > getStrike(lmscur[end])
        return collect(zip(1:4, butter(xpir, curp)))
    end
    return []
end

function strat2(xpir, curp, lmscur)
    if curp < getStrike(lmscur[1])
        return collect(zip(1:2, spread(xpir, put, long, coff(curp, -6, 1))))
    elseif curp > getStrike(lmscur[end])
        return collect(zip(3:4, spread(xpir, call, short, coff(curp, 5, 1))))
    end
    return []
end

function stratShort1(xpir, curp, lmscur)
    if getStrike(lmscur[1]) < curp < getStrike(lmscur[end])
        return collect(zip(1:4, condorShort(xpir, curp)))
    end
    return []
end
#endregion

end