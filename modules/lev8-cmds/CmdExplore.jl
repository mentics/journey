module CmdExplore
using Dates
using SH, Globals, BaseTypes, SmallTypes, RetTypes, StratTypes, LegMetaTypes, ConstructUtil
using Shorthand, Between
using Expirations, Markets, Chains
using DrawStrat

export sh, shc, shRet, shVals, drsh, drsh!, shLegs # shLegs is reexported form Shorthand
export drlms, drlms!

drlms(lms, label="") = drawRet(combineTo(Ret, lms, minimum(getExpiration.(lms)), market().startPrice, getvr()), nothing, market().startPrice, label)
drlms!(lms, label="") = drawRet!(combineTo(Ret, lms, minimum(getExpiration.(lms)), market().startPrice, getvr()), label)

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
drsh(str::AStr, ex::Int=1) = drsh(str, expirs()[ex:ex+2]) # (sp = market().startPrice ; drawRet(shRet(str, expirs()[ex:ex+2], sp), nothing, sp, "sh") )
drsh(str::AStr, exps) = (sp = market().startPrice ; drawRet(shRet(str, exps, sp); cp=sp, label="sh") )
drsh!(str::AStr, ex::Int=1) = drsh!(str, expirs()[ex:ex+2])
drsh!(str::AStr, exps) = (sp = market().startPrice ; drawRet!(shRet(str, exps, sp); label="sh+") )

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

using CmdStrats, StratGen, Positions
function whyNotCombi(combi::Combi)
    inspreads = findSpreads(CmdStrats.lastSpreads2[], combi)
    # @info "Was in spreads?" inspreads
    oqs = optQuoter.(tos(LegMeta, combi))
    vo = StratGen.validOption(Globals.get(:Strats)[:maxStrikeDist], market().curp)
    optInvalid = findfirst(map(!, vo.(oqs)))
    @info "Options valid?" optInvalid
    legsPos = getLeg.(positions())
    conflict = map(lr -> isConflict.(getLeg(lr), legsPos), combi)
    @info "Is position conflict?" conflict # Tuple(c for c in conflict)

    if (inspreads == (nothing,nothing))
        answer = "because: \n"
        if !isnothing(optInvalid) # optsValid != (false, false, false, false)
            answer *= "  first invalid option found "
            answer *= string(oqs[optInvalid])
            answer *= '\n'
        end
        if conflict != (false, false, false, false)
            ind = findfirst(conflict)
            answer *= "  conflicted with position "
            answer *= string(find(p -> isConflict(combi[ind], getLeg(p)), legsPos))
            answer *= '\n'
        end
        println("Was not in spreads $(answer)")
    else
        println("TODO: was in spreads")
    end
end

using CalcUtil
export tryall
function tryall(ex=3)
    sp = market().startPrice
    vr = getvr()

    expr = expir(3)
    ana(ex; noPos=true, headless=true)
    lmsAll = LegMeta[]
    pv1, pv2 = pvals()

    # score = -10000.0
    ev1 = -10000.0
    ev2 = -10000.0
    evSum = -10000.0
    prob1 = 0.0
    prob2 = 0.0
    mn = -10000.0
    for c in CmdStrats.lastView[]
        append!(lmsAll, tos(LegMeta, c))
        # scoreTmp = scoreFor(lmsAll, expr, sp, vr)

        vals = getVals(combineTo(Ret, lmsAll, expr, sp, vr))
        met1 = calcMetrics(pv1, vals, 1000.0, 0.0)
        met2 = calcMetrics(pv2, vals, 1000.0, 0.0)
        evSumNext = met1.ev + met2.ev

        # println("old evsum ", ev1, " new evSumNext ", evSumNext)
        # println("old ev1 ", ev1, " new ev1 ", met1.ev, " old prob1 ", prob1, " new prob1 ", met1.prob, " old mn ", mn, " new mn ", met1.mn)
        # println("old ev2 ", ev2, " new ev2 ", met2.ev, " old prob2 ", prob2, " new prob2 ", met2.prob, " len ", length(lmsAll))

        # println("old score ", score, " new score ", scoreTmp, )
        # if scoreTmp > score
        #     score = scoreTmp
        # else
        #     resize!(lmsAll, length(lmsAll) - length(c))
        # end
        # if (met1.ev > ev1 || met2.ev > ev2) &&
        #         (met1.prob >= .9 * prob1 || met2.prob >= .9 * prob2) &&
        #         evSumNext > evSum &&
        #         (mn > 0.0 ? mn > 0.0 : met1.mn >= 1.25 * mn)
        # if (met1.prob >= .9 * prob1 || met2.prob >= .9 * prob2)
        if (met1.prob > prob1 || met2.prob > prob2)
            println("new len ", length(lmsAll), " old prob1 ", prob1, " new prob1 ", met1.prob, " old prob2 ", prob2, " new prob2 ", met2.prob)
            met1.ev > ev1 && (ev1 = met1.ev)
            met2.ev > ev2 && (ev2 = met2.ev)
            met1.prob > prob1 && (prob1 = met1.prob)
            met2.prob > prob2 && (prob2 = met2.prob)
            met1.mn > mn && (mn = met1.mn)
        else
            resize!(lmsAll, length(lmsAll) - length(c))
        end
    end
    retAll = combineTo(Ret, lmsAll, expr, sp, vr)
    drawRet(retAll, probs(), market().curp, "")

    vals = getVals(combineTo(Ret, lmsAll, expr, sp, vr))
    met1 = calcMetrics(pv1, vals, 1000.0, 0.0)
    println(met1)
    # pretyble(met1)
end

export t2
function t2(ex=3)
    sp = market().startPrice
    vr = getvr()

    expr = expir(3)
    ana(ex; noPos=true, headless=true)
    pv1, pv2 = pvals()

    score = -10000.0
    ev1 = -10000.0
    ev2 = -10000.0
    evSum = -10000.0
    prob1 = 0.0
    prob2 = 0.0
    mn = -10000.0
    probSum = 0.0
    winner = nothing
    wininds = nothing
    cs = CmdStrats.lastView[]
    probs = zeros(12)

    Threads.@threads for i1 in 1:(length(cs)-2)
        thrid = Threads.threadId()
        lms = LegMeta[]
        for i2 in (i1+1):(length(cs)-1)
            for i3 in (i2+1):length(cs)
            empty!(lms)
            append!(lms, tos(LegMeta, cs[i1]))
            append!(lms, tos(LegMeta, cs[i2]))
            append!(lms, tos(LegMeta, cs[i3]))
            # scoreTmp = scoreFor(lmsAll, expr, sp, vr)

            vals = getVals(combineTo(Ret, lms, expr, sp, vr))
            met1 = calcMetrics(pv1, vals, 1000.0, 0.0)
            # met2 = calcMetrics(pv2, vals, 1000.0, 0.0)
            # evSumNext = met1.ev + met2.ev
            # if (met1.ev > ev1 || met2.ev > ev2) &&
            #         (met1.prob >= .9 * prob1 || met2.prob >= .9 * prob2) &&
            #         evSumNext > evSum &&
            #         (mn > 0.0 ? mn > 0.0 : met1.mn >= 1.25 * mn)
            # if (met1.prob >= .9 * prob1 || met2.prob >= .9 * prob2)
            # probSumNext = met1.prob + met2.prob
            # if (probSumNext > probSum)
            if (met1.ev > 0.0 && met1.prob > probs[thrid])
                # println(" old probSum ", probSum, " new probSum ", probSumNext, " old prob1 ", prob1, " new prob1 ", met1.prob, " old prob2 ", prob2, " new prob2 ", met2.prob)
                # met1.ev > ev1 && (ev1 = met1.ev)
                # met2.ev > ev2 && (ev2 = met2.ev)
                # met1.prob > prob1 && (prob1 = met1.prob)
                # met2.prob > prob2 && (prob2 = met2.prob)
                # met1.mn > mn && (mn = met1.mn)
                # probSum = probSumNext
                probs[thrid] = met1.prob
                winner = copy(lms)
                wininds = (i1, i2, i3)
                println(" new ev ", met1.ev)
                println(wininds, " : ", met1)
                # @goto stop
            # else
            #     resize!(lmsAll, length(lmsAll) - length(c))
            end
        end
        end
    end
    @label stop
    retAll = combineTo(Ret, winner, expr, sp, vr)
    drawRet(retAll, probs(), market().curp, "")

    vals = getVals(combineTo(Ret, winner, expr, sp, vr))
    metWin = calcMetrics(pv1, vals, 1000.0, 0.0)
    println(metWin)
    println(wininds)

    for ind in wininds
        println(metricsFor(ind, expr, sp, vr))
    end

    return wininds
end

metricsFor(i::Int, expr, sp, vr) = metricsFor(collect(arl(i)), expr, sp, vr)

function metricsFor(lms, expr, sp, vr)
    vals = getVals(combineTo(Ret, lms, expr, sp, vr))
    return calcMetrics(pvals()[1], vals, 1000.0, 0.0)
end



#=================== 6/4/2022 ===============#
using OptionUtil, GLMakie
function exploreSpreads1()
    curp = market().curp
    oqs = filter(oq->isCall(oq) && .9 < getStrike(oq)/curp < 1.1, chains()[Date(2022,6,8)].chain)
    oqpairs = []
    for i in 1:(length(oqs)-1)
        oq1, oq2 = oqs[i], oqs[i+1]
        push!(oqpairs, (oq1, oq2))
    end
    display(barplot([(getStrike(p[2])/curp, calcNetShortPerWidth(p...)) for p in oqpairs]))
    display(barplot!([(getStrike(p[2])/curp, calcNetLongPerWidth(p...)) for p in oqpairs]))
    return oqpairs
end

using BlacksPricing, Calendars, ChainTypes, Snapshots, QuoteTypes, OptionMetaTypes
function priceOq(from::DateTime, curp, oq)
    tex = calcTex(from, getExpiration(oq))/24/365 # convert to year for blackscholes
    (tex, priceOption(getStyle(oq), Float64(getStrike(oq)), tex, getIv(oq), Float64(curp)))
end

function overtime(oq::OptionQuote, from::DateTime)
    curp = market().curp
    [priceOq(ts, curp, oq) for ts in from:(-Day(1)):(from - Day(180))]
end

function otCondor(side::Side.T=Side.long)
    snap(6,0,6,58)
    curp = market().curp
    ts = market().tsMarket
    expr = Date(2022,6,3)
    oqsAll = Chains.nearOqs(curp, filter(isCall, chains()[expr].chain), 12)
    len = length(oqsAll)
    # oqs = oqsAll[(end-4):end]
    # oqs = oqsAll[1:2:4*2]
    # oqs = oqsAll[(end-4*2):2:(end)]
    oqs = oqsAll[[1,2,len-1,len]]
    theoqs = theoQuote.(ts, curp, oqs)
    lms = makeCondor(theoqs, side)
    display(drlms(lms))
    foreach( ts:(-Day(8)):(ts - Day(80)) ) do ts
        theoqs = theoQuote.(ts, curp, oqs)
        lms = makeCondor(theoqs, side)
        display(drlms!(lms, string(ts)))
    end
end

function otCondorActual(side::Side.T=Side.long)
    snap(6,0,6,58)
    curp = market().curp
    ts = market().tsMarket

    oqsAll = Chains.nearOqs(curp, filter(isCall, chains()[expir(1)].chain), 12)
    len = length(oqsAll)
    # oqs = oqsAll[(end-4):end]
    # oqs = oqsAll[1:2:4*2]
    # oqs = oqsAll[(end-4*2):2:(end)]
    oqs = oqsAll[[1,2,len-1,len]]
    lms = makeCondor(oqs, side)
    display(drlms(lms))

    for expr in expirs()[10:19]
        oqsAll = Chains.nearOqs(curp, filter(isCall, chains()[expr].chain), 12)
        len = length(oqsAll)
        # oqs = oqsAll[(end-4):end]
        # oqs = oqsAll[1:2:4*2]
        # oqs = oqsAll[(end-4*2):2:(end)]
        oqs = oqsAll[[1,2,len-1,len]]
        lms = makeCondor(oqs, side)
        display(drlms!(lms))
    end
end

function theoQuote(ts, curp, oq::OptionQuote)::OptionQuote
    _, theop = priceOq(ts, curp, oq)
    theoc = C(theop)
    # TODO: theo wasn't within range, what to do?
    # @assert 0.9*getBid(oq) < theop < 1.1*getAsk(oq) "$(0.9*getBid(oq)) < $(theop) < $(1.1*getAsk(oq))"
    return OptionQuote(oq; quot=Quote(theoc, theoc))
end

#==== 7/2/2022 ====#

function searchButterfly()
    s2("c", "c")
    # s2("p", "p")
    # s2("p", "c")
    # s2("c", "p")
    # println(profMax)
    # return candMax
end

function s2(s1, s2)
    curp = market().curp
    low = floor(curp) - 20.0
    high = ceil(curp) + 19.0
    # profMax = 0.028
    profMax = -100.
    candMax = ""
    pass = []
    for str1 in low:1.0:high
        # println("Checking $(str1)")
        cand = "s$(str1)$(s1)@1 / l$(str1+1.0)$(s1)@1 / l$(str1+1.0)$(s2)@1 / s$(str1+2.0)$(s2)@1"
        ret = shRet(cand, 1)
        prof = maximum(getVals(ret))
        if prof > profMax
            candMax = cand
            profMax = prof
        end
        if prof > .028
            push!(pass, cand)
            println(prof, " ", cand)
        end
    end
    lms = mapreduce(sh, vcat, pass)
    drlms(lms)
end

using ConTime, Rets
function test10()
    snap(5,20,7,0)
    dateTarget = expir(8)
    filt = ConTime.dateHourFilter(Date(2022, 5, 19), dateTarget, 14)
    # filt = ConTime.dateFilter(Date(2022, 5, 19), dateTarget)
    lastCenter = C(0.0)
    rets = Vector{Ret}()
    tsPrev = DateTime(0)
    lmsPos = Vector{LegMeta}()
    ConTime.runForSnaps(filt) do name, ts
        @assert ts > tsPrev
        tsPrev = ts
        Main.save[:dir] = -1
        ret1 = nothing
        ret2 = nothing
        if ana(dateTarget; headless=true, lmsPos) > 0
            ret1 = ret(1)
            lms1 = arl(1)
            println("-1 found")
        else
            println("-1 not found")
        end
        if !isnothing(ret1)
            Main.save[:dir] = 1
            if ana(dateTarget; headless=true, lmsPos) > 0
                ret2 = ret(1)
                lms2 = arl(1)
                println("1 found ")
            else
                println("1 not found")
            end

            if !isnothing(ret1) && !isnothing(ret2)
                push!(rets, ret1)
                append!(lmsPos, lms1)
                push!(rets, ret2)
                append!(lmsPos, lms2)
            end
        end

        lastCenter = market().curp
        Main.save[:lastCenter] = lastCenter
    end
    Main.save[:rets] = rets
    cr1 = combineRetsC(rets, lastCenter)
    Main.save[:cret] = cr1
    drawRet(cr1; label="c")
    # drawRet!(ret1; label="1")
    @info "Result" length(rets)
end

function retFor(legs)
    Leg(Option(styl, expir[ex], parse(Currency, xs[2])), Float64(qty), sid)
end

using OptionTypes, LegTypes
function findC(dateTarget, cp, width, sid::Side.T)
    profExp = width / 2
    below = floor(cp)
    above = below + 1.0
    # TODO: could skip some to save time
    left = sid == Side.long ? below - 20 : below
    right = sid == Side.long ? above + 20 : above
    dir = Int(sid)
    for _ in 1:20
        println("Checking ", left, right)
        try
            legs = (
                Leg(Option(Style.put, dateTarget, C(left - width)), 1.0, sid),
                Leg(Option(Style.put, dateTarget, C(left)), 1.0, toOther(sid)),
                Leg(Option(Style.call, dateTarget, C(right)), 1.0, toOther(sid)),
                Leg(Option(Style.call, dateTarget, C(right + width)), 1.0, sid)
            )
            ret = combineTo(Ret, legs, optQuoter, dateTarget, cp, 1.0)
            mn, mx = extrema(getVals(ret))
            if profExp < mx < profExp + .5 && -profExp < mn < -profExp+.5
                return ret
            end
        catch
            # ignore can't quote errors
        end
        left = left + dir
        right = right - dir
    end
    error("not found")
end

function test11()
    snap(5,20,7,0)
    dateTarget = expir(8)
    filt = ConTime.dateHourFilter(Date(2022, 5, 19), dateTarget, 14)
    # filt = ConTime.dateFilter(Date(2022, 5, 19), dateTarget)
    rets = Vector{Ret}()
    width = 4
    ConTime.runForSnaps(filt) do name, ts
        retLong = findC(dateTarget, market().curp, width, Side.long)
        retShort = findC(dateTarget, market().curp, width, Side.short)
        push!(rets, retLong)
        push!(rets, retShort)
    end
    Main.save[:rets] = rets
    cr1 = combineRetsC(rets, market().curp)
    Main.save[:cret] = cr1
    drawRet(cr1; label="c")
    # drawRet!(ret1; label="1")
    @info "Result" length(rets)
end

end