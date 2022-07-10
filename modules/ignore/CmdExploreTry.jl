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

# function retFor(legs)
#     Leg(Option(styl, expir[ex], parse(Currency, xs[2])), Float64(qty), sid)
# end

using OptionTypes, LegTypes, CalcUtil, Scoring
MAX_WIDTH = 2.0
PROF_MIN = 0.2
SCORE_MIN = 0.5
# TODO: this is the same as looking for all the butterflies
function test12()::String
    maxDist = 40 # Float64(config()[:Strats][:maxStrikeDist])
    snap(5,20,7,0)
    exp = expir(8)
    # filt = ConTime.dateHourFilter(Date(2022, 5, 19), exp, 14)
    filt = ConTime.dateFilter(Date(2022, 5, 19), exp)
    # baseSnap = snap()
    # filt = t -> t[1] == baseSnap
    rets = Vector{Ret}()
    retAll = nothing
    ConTime.runForSnaps(filt) do name, ts
        prob = probFlat(Float64(cp), 0.0)
        # scorer = ret -> calcMetrics(prob, ret).prob
        retCand = findCondor(maxDist, prob, retAll)

        if !isnothing(retCand)
            push!(rets, retCand)
            isnothing(retAll) ? (retAll = retCand) : (retAll = combineRetsC((retAll, retCand), cp))
        end
    end
    Main.save[:rets] = rets
    Main.save[:retall] = retAll
    !isnothing(retAll) || return "Nothing found"

    drawRet(retAll; label="all")
    drawRet!(rets[1]; label="1")
    drawRet!(rets[end]; label="end")
    return "$(length(rets)) found"
end

function findCondor(maxDist, retAll, exp)
    cp = market().curp
    tex = calcTex(market().tsMarket, exp)
    prob = CmdStrats.makeProbs(tex, exp, cp)[1]

    strikeExtent = (floor(cp) - maxDist, ceil(cp) + maxDist)
    retCand = nothing
    scoreMax = SCORE_MIN
    for inner in 2.0:maxDist
        for width in 1.0:MAX_WIDTH
            template = (0, width, width + inner, 2 * width + inner)
            forEachTemplate(strikeExtent, exp, retAll, cp, template) do retC2, retLong, retShort, strikesLong, strikesShort
                met = calcMetrics(prob, retC2)
                score = met.prob
                if score > scoreMax
                    # if !isnothing(retAll)
                    #     retBoth = combineRetsC((retC2, retAll), cp)
                    #     mnBoth, mxBoth = extrema(getVals(retBoth))
                    #     # @info "Checking retAll" mnBoth -MAX_WIDTH
                    #     mnBoth > -MAX_WIDTH || return
                    # end
                    # mps = findMinPrices(retC2)
                    # if length(mps) != 2
                    #     Main.save[:rets] = rets
                    #     @warn "unexpected mps len" mps
                    #     drawRet(retC2)
                    # end
                    # minPrice1, minPrice2 = mps
                    # for ret in rets
                    #     if valAtPrice(ret, minPrice1) < 0.0 || valAtPrice(ret, minPrice2) < 0.0
                    #         return
                    #     end
                    # end

                    # for ret in rets
                    #     retCheck = combineRetsC((retC2, ret), cp)
                    #     mnCheck = minimum(getVals(retCheck))
                    #     mnCheck > met.mn || return
                    #     # @info "Checking" -MAX_WIDTH mnCheck
                    # end

                    # Main.save[:retLong] = retLong
                    # Main.save[:retShort] = retShort
                    retCand = retC2
                    scoreMax = score
                end
            end
        end
    end
    return retCand
end

using Bins
function findMinPrices(ret)
    vals = getVals(ret)
    res = Vector{Float64}()
    mn = 0.0
    maxI = 0
    look = false
    for i in 2:length(vals)
        pv = vals[i-1]
        v = vals[i]
        if pv >= 0.0 && v < 0.0
            look = true
            mn = 0.0
        elseif pv <= 0.0 && v > 0.0
            look = false
        end
        if look && v < mn
            mn = v
            maxI = i
        end
        if look && v > mn + .1
            push!(res, getCenter(ret) * Bins.xs()[maxI])
            look = false
        end
    end
    return res
end

function forEachTemplate(f, (strikeMin, strikeMax), exp, retAll, cp, t)
    forEachShort((strikeMin, strikeMax), exp, retAll, cp, t) do retShort, strikesShort
        forEachLong(exp, cp, strikesShort) do retLong, strikesLong
            retC2 = combineRetsC((retLong, retShort), cp)
            f(retC2, retLong, retShort, strikesLong, strikesShort)
        end
    end

#     for left in strikeMin:(strikeMax - t[4])
#         strikes = (x->x+left).(t)
#         legs = condor(exp, Style.put, Style.call, strikes, Side.short)
#         isnothing(findfirst(!isQuotable, getOption.(legs))) || continue

#         retShort = combineTo(Ret, legs, optQuoter, exp, cp, 1.0)
#         _, mxShort = extrema(getVals(retShort))
#         profShort = (mxShort - mxExp)
#         profShort >= PROF_MIN || continue

#         resLong = findLongInner(PROF_MIN, exp, cp, strikes)
#         !isnothing(resLong) || continue
#         retLong, strikesLong = resLong

#         retC2 = combineRetsC((retLong, retShort), cp)
#         mnC2, mxC2 = extrema(getVals(retC2))
#         profC2 = mxC2
#         profC2 > profMax || continue

#         if !isnothing(retAll)
#             retBoth = combineRetsC((retC2, retAll), cp)
#             mnBoth, mxBoth = extrema(getVals(retBoth))
#             @info "Checking retAll" mnBoth -MAX_WIDTH
#             mnBoth > -MAX_WIDTH || continue
#         end

#         profMax = profC2
#         res = retC2
#         # @info "Found retC2" prof profMax strikes strikesLong
#     end
#     return (res, profMax)
end

function forEachShort(f, (strikeMin, strikeMax), exp, retAll, cp, t)
    mxExp = (t[2] - t[1]) / 2
    for left in strikeMin:(strikeMax - t[4])
        strikes = (x->x+left).(t)
        legs = condor(exp, Style.put, Style.call, strikes, Side.short)
        isnothing(findfirst(!isQuotable, getOption.(legs))) || continue

        retShort = combineTo(Ret, legs, optQuoter, exp, cp, 1.0)
        mn, mx = extrema(getVals(retShort))
        ((mx - mxExp) >= PROF_MIN && (mn + mxExp) >= PROF_MIN) || continue

        f(retShort, strikes)
    end
end

function forEachLong(f, exp, cp, strikes)
    width = strikes[2] - strikes[1]
    mxExp = width / 2
    addMax = floor(round((strikes[3] + strikes[2])/2; digits=1)) - width - strikes[1]
    for add in 1.0:addMax
        ss = (strikes[1]+add, strikes[1]+add+width, strikes[4]-add-width, strikes[4]-add)
        legs = condor(exp, Style.put, Style.call, ss, Side.long)
        isnothing(findfirst(!isQuotable, getOption.(legs))) || continue

        retLong = combineTo(Ret, legs, optQuoter, exp, cp, 1.0)
        mn, mx = extrema(getVals(retLong))
        ((mx - mxExp) >= PROF_MIN && (mn + mxExp) >= PROF_MIN) || continue

        f(retLong, ss)
    end
end

# function findLongInner(profMin, exp, cp, strikes)
#     width = strikes[2] - strikes[1]
#     mxExp = width / 2
#     addMax = floor(round((strikes[3] + strikes[2])/2; digits=1)) - width - strikes[1]
#     for add in 1.0:addMax
#         ss = (strikes[1]+add, strikes[1]+add+width, strikes[4]-add-width, strikes[4]-add)
#         legs = condor(exp, Style.put, Style.call, ss, Side.long)
#         isnothing(findfirst(!isQuotable, getOption.(legs))) || continue

#         retLong = combineTo(Ret, legs, optQuoter, exp, cp, 1.0)
#         mn, mx = extrema(getVals(retLong))
#         # @info "Long check" ss (mx - mxExp)
#         (mx - mxExp) > profMin || continue
#         if ss[1] == 376.0 && ss[2] == 380.0
#             @info "Found long" ss (mx-mxExp)
#         end

#         return (retLong, ss)
#     end
#     return nothing
# end

function condor(exp, s1, s2, strikes, sid)
    legs = (
        Leg(Option(s1, exp, strikes[1]), 1.0, sid),
        Leg(Option(s1, exp, strikes[2]), 1.0, toOther(sid)),
        Leg(Option(s2, exp, strikes[3]), 1.0, toOther(sid)),
        Leg(Option(s2, exp, strikes[4]), 1.0, sid)
    )
    return legs
end


function findC(dateTarget, retCur, cp, width, sid::Side.T)
    minProf = .12
    profExp = width / 2
    below = floor(cp)
    if below % 2 != 0
        below -= 1.0
    end
    above = below + 2.0
    # TODO: could skip some to save time
    left = sid == Side.long ? below - 20 : below
    right = sid == Side.long ? above + 20 : above
    dir = 2 * Int(sid)
    for _ in 1:10
        try
            strikes = (C(left - width), C(left), C(right), C(right + width))
            legs = (
                Leg(Option(Style.put, dateTarget, strikes[1]), 1.0, sid),
                Leg(Option(Style.put, dateTarget, strikes[2]), 1.0, toOther(sid)),
                Leg(Option(Style.call, dateTarget, strikes[3]), 1.0, toOther(sid)),
                Leg(Option(Style.call, dateTarget, strikes[4]), 1.0, sid)
            )
            ret = combineTo(Ret, legs, optQuoter, dateTarget, cp, 1.0)
            mn, mx = extrema(getVals(ret))

            # @info "Checking2" left right profExp mn mx
            if profExp+minProf < mx < profExp + .5 && -profExp+minProf < mn < -profExp+.5
                retBoth = combineRetsC((ret, retCur), cp)
                curmn, curmx = extrema(getVals(retBoth))
                if curmn > MAX_WIDTH
                    println("Found $(sid) $(width) $(cp) $(strikes)")
                    return ret
                end
            end
        catch e
            @error "Exception in findC" snap() dateTarget cp width sid e
            return nothing
        end
        left = left + dir
        right = right - dir
    end
    @warn "findC: Not found" snap() dateTarget cp width sid
    return nothing
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
        if !isnothing(retLong) && !isnothing(retShort)
            ret = combineRetsC((retLong, retShort), market().curp)
            push!(rets, ret)
        end
    end
    Main.save[:rets] = rets
    cr1 = combineRetsC(rets, market().curp)
    Main.save[:cret] = cr1
    drawRet(cr1; label="c")
    # drawRet!(ret1; label="1")
    @info "Result" length(rets)
end


function testScore()
    snap(5,20,7,0)
    dateTarget = expir(8)
    # filt = ConTime.dateHourFilter(Date(2022, 5, 19), dateTarget, 14)
    filt = ConTime.dateFilter(Date(2022, 5, 19), dateTarget)
    rets = Vector{Ret}()
    lmsPos = Vector{LegMeta}()
    ConTime.runForSnaps(filt) do name, ts
        if ana(dateTarget; headless=true, lmsPos) > 0
            push!(rets, ret(1))
            append!(lmsPos, arl(1))
        end
    end
    Main.save[:rets] = rets
    cret = combineRetsC(rets, market().curp)
    Main.save[:cret] = cret
    Main.save[:lmsPos] = lmsPos
    drawRet(cret; label="c")
    # drawRet!(ret1; label="1")
    met = calcMetrics(probsFor(8)[1], cret)
    @info "Result" length(rets) met
end

