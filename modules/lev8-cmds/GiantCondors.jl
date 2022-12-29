module GiantCondors
using Dates
using SH, BaseTypes, SmallTypes, LegMetaTypes
using DateUtil, ChainUtil, Pricing
import HistData as HD
using Markets, Expirations, Chains
import CmdPos:xlegs
import BacktestSimple as bt
import BacktestSimple:rond

#region Public
function getParamsLive()
    return (;
        balInit = 100,
        ExtremaBdays = 20,
        marginMaxRat = 0.98,
        marginPerDayRat = 0.2,
        maxPerTrade = 100,
        Put = (;xbdays=10:84, probMin=.99, takeRateMin=.2),
        Call = (;xbdays=10:84, probMin=.95, takeRateMin=.2),
    )
end

# function getParams()
#     return (;
#         balInit = 1000,
#         ExtremaBdays = 20,
#         marginMaxRat = 0.98,
#         marginPerDayRat = 0.2,
#         maxPerTrade = 100,
#         Put = (;xbdays=10:124, moveMin=.1, moveSdevs=2., offMaxRat=40/400, profMinRat=.8/400, takeRat=0.24/400),
#         Call = (;xbdays=10:124, moveMin=.1, moveSdevs=2., offMaxRat=40/400, profMinRat=.8/400, takeRat=0.24/400),
#     )
# end

function getParams()
    return (;
        balInit = 1000,
        ExtremaBdays = 20,
        marginMaxRat = 0.98,
        marginPerDayRat = 0.2,
        maxPerTrade = 100,
        Put = (;xbdays=10:84, probMin=.999, takeRateMin=.2),
        Call = (;xbdays=10:84, probMin=.95, takeRateMin=.2),
    )
end

import Kelly, QuoteTypes
function calcScore1(ctx, tmult, lo, sho)
    # # TODO: consider greeks?
    theta = getGreeks(lo).theta - getGreeks(sho).theta
    delta = getGreeks(lo).delta - getGreeks(sho).delta
    # # @show getGreeks(sho).theta getGreeks(lo).theta theta
    # # theta >= 0.01 || return -1000.0
    theta >= abs(delta)/3 || return -1000.0
    # println("delta: $(delta) theta: $(theta)")
    risk = calcRisk(lo, sho)
    # Wrong: ret = F(bap(sho, .1) - bap(lo, .1))
    ret = F(netoq2(lo, sho))
    ret > 0.0 || ( bt.log("ERROR: Found ret < 0.0", ret) ; return -100.0 )
    rate = calcRate(tmult, ret, risk)

    move = strlo < ctx.curp ?
        (1.0 - strsho / ctx.curp) :
        (strsho / ctx.curp - 1.0)
    TopMove = .2
    moveBonus = (move - .16) / (TopMove - .16)
    winRate = 0.96 + .0399*moveBonus
    kel = Kelly.simple(winRate, ret, risk)
    score = rate * kel
    # println("calcScore:")
    # @show tmult ret risk rate move moveBonus winRate kel score (move - .16) (TopMove - .16)
    return score * (theta / delta)
end

function calcScore3(ctx, prob, side, tmult, innerStrike::Float64, lo, sho)
    neto, risk = openInfo(lo, sho)
    neto > 0.0 || return nothing

    # Subtract out the worth of the money over time to adjust for the kelly calc
    ret = neto - .01 - risk * (.1 / tmult)
    rate = calcRate(tmult, ret, risk)

    mov = innerStrike / ctx.curp
    winRate = side == Side.long ? ProbUtil.cdfFromRight(prob, mov) : ProbUtil.cdfFromLeft(prob, mov)
    kel = Kelly.simple(winRate, F(ret), F(risk))
    kel > 0 || return nothing

    return (kel * rate, neto, risk, ret, rate)
end
resScore(r)::Float64 = isnothing(r) ? -1 : r[1]
resNeto(r)::Float64 = r[2]
resRisk(r)::Float64 = r[3]

makeCtx(acct) = (;
    ts = acct[:ts],
    curp = F(acct[:curp]),
    vix = F(acct[:vix])
)

function live(side=Side.long)
    p = getParamsLive()
    ps = side == Side.long ? p.Put : p.Call
    mkt = market()
    date = mkt.startDay
    curp = mkt.curp
    vix = mkt.vix/100

    # TODO: should use today's hi/lo to start with, not curp
    # hi, lo = HD.extrema(bdaysBefore(date, p.ExtremaBdays), date-Day(1), curp, curp)

    getOqss = xpir -> ChainUtil.oqssEntry(chain(xpir).chain, curp, xlegs(xpir))
    xpirs = DateUtil.matchAfter(date, expirs(), ps.xbdays)
    # offMax = curp * ps.offMaxRat
    # profMin = curp * ps.profMinRat
    # extrema = HD.extrema(HD.dataDaily(), bdaysBefore(date, p.ExtremaBdays), date - Day(1), curp, curp)
    # lms = findGC((;curp, vix), getOqss, xpirs, date, side, offMax, profMin, vix, extrema, ps.moveMin, ps.moveSdevs)

    res = findGC((;ts=mkt.tsMarket, curp, vix), getOqss, xpirs, date, side, vix, ps.probMin)
    if !isnothing(res)
        lms, r = res
        return (collect(lms), r)
    else
        return
    end
    return isnothing(lms) ? nothing : collect(lms)
end

marginPerDay(acct) = acct[:params].marginPerDayRat * acct[:bal]

function stratDay(acct)
    # acct[:openLong] = nothing
    # acct[:openShort] = nothing
    # acct[:margin] < getParams().marginMax || ( acct[:missed] += 1 )
end

function strat(acct)
    p = acct[:params]
    # bt.checkClose((bt.checkSides, bt.checkThreaten), acct, p)
    bt.checkClose((bt.checkSides,), acct, p)
    curp = acct[:curp]

    # TODO: maybe be more precise about last trade under margin: could check margin during candidate search

    marginAvail = acct[:bal] * p.marginMaxRat

    if canOpenPut(acct, marginAvail)
        # offMax = curp*p.Put.offMaxRat
        res = back(acct, Side.long; p.Put...) # offMax, profMin=curp * p.Put.profMinRat,
        if !isnothing(res)
            lms, r = res
            # @assert neto > 0.0 "neto:$(neto), risk:$(risk)"
            # @assert risk / getQuantity(lms[1]) <= offMax
            # lo = acct[:extrema].lo
            # mov = getStrike(lms[2]) / lo
            mov = getStrike(lms[2]) / curp
            bt.openTrade(acct, lms, "long spread: mov=$(rond(mov)) r=$(r)")
        end
    end

    if canOpenCall(acct, marginAvail)
        # offMax = curp*p.Call.offMaxRat
        res = back(acct, Side.short; p.Call...) # offMax, profMin=curp * p.Call.profMinRat,
        if !isnothing(res)
            lms, r = res
            # @assert neto > 0.0 "neto:$(neto), risk:$(risk)"
            # @assert risk <= offMax
            # hi = acct[:extrema].hi
            # mov = getStrike(lms[1]) / hi
            mov = getStrike(lms[1]) / curp
            bt.openTrade(acct, lms, "short spread: mov=$(rond(mov))")
        end
    end
end

function canOpenCall(acct, mx)
    risk = acct[:marginDay].call.risk
    avail = marginPerDay(acct)
    if risk >= avail
        # bt.log("Hit max margin day: call $(risk) / $(avail)")
        return false
    end

    risk = acct[:margin].call.risk
    if risk >= mx
        # bt.log("Hit max margin: call $(risk) / $(avail)")
        return false
    end
    return true
end

function canOpenPut(acct, mx)
    risk = acct[:marginDay].put.risk
    avail = marginPerDay(acct)
    if risk >= avail
        # bt.log("Hit max margin day: put $(risk) / $(avail)")
        return false
    end

    risk = acct[:margin].put.risk
    if risk >= mx
        # bt.log("Hit max margin: put $(risk) / $(avail)")
        return false
    end
    return true
end

function back(acct, side; xbdays, probMin, kws...) # , offMax, profMin, moveMin, moveSdevs, kws...)
    date = acct[:date]
    xoqss = acct[:xoqss]
    getOqss = xpir -> xoqss[xpir]
    xpirs = DateUtil.matchAfter(date, acct[:xpirs], xbdays)
    res = findGC(makeCtx(acct), getOqss, xpirs, date, side, acct[:vix], probMin) # , offMax, profMin, acct[:extrema], moveMin, moveSdevs)
    if !isnothing(res)
        lms, r = res
        qty = qtyForMargin(acct, side, resRisk(r))
        if qty > 0
            return (withQuantity.(lms, F(qty)), r)
        end
        # return res
    end
    return nothing
end

function qtyForMargin(acct, side, risk)
    margin = side == Side.long ? acct[:margin].put.risk : acct[:margin].call.risk
    marginDay = side == Side.long ? acct[:marginDay].put.risk : acct[:marginDay].call.risk
    params = acct[:params]
    bal = acct[:bal]
    avail = bal * params.marginMaxRat - margin
    availDay = bal * params.marginPerDayRat - marginDay
    return min(params.maxPerTrade, round(Int, min(avail, availDay) / risk, RoundDown))
end
#endregion

#region Local
import ProbKde, ProbUtil
function findGC(ctx, getOqss, xpirs, date, side, vix, probMin) # , offMax, profMin, hilo, moveMin, moveSdevs)
    findEntry = side == Side.long ? findSpreadEntryLong : findSpreadEntryShort
    best = nothing
    bestScore = 0.0

    for xpir in xpirs
        tmult = timult(date, xpir)

        prob = ProbKde.probToClose(F(ctx.curp), 100*F(vix), ctx.ts, xpir)
        if side == Side.long
            strikeExt, _ = ProbUtil.probFromRight(prob, probMin)
        else
            strikeExt, _ = ProbUtil.probFromLeft(prob, probMin)
        end

        calcScore = (lo, sho, innerStrike) -> calcScore3(ctx, prob, side, tmult, innerStrike, lo, sho)
        oqss = getOqss(xpir)

        res = findEntry(oqss, calcScore, strikeExt) # , offMax, profMin
        !isnothing(res) || continue
        score = resScore(res)
        if score > bestScore
            best = res[2]
            bestScore = score
        end
    end
    return best
end

function findSpreadEntryLong(oqss, calcScore, strikeExt, debug=false) # , offMax, profMinRaw)
    shorts = oqss.put.short
    longs = oqss.put.long
    if isempty(longs)
        bt.log("WARN: oqss.put.long was empty")
        return nothing
    end
    if isempty(shorts)
        bt.log("WARN: oqss.put.short was empty")
        return nothing
    end

    best = nothing
    bestScore = 0.0
    for ilo in eachindex(longs)
        lo = longs[ilo]
        lostr = getStrike(lo)
        lostr <= strikeExt || break
        # TODO: optimize, it's looping over more than necessary
        for isho in eachindex(shorts)
            sho = shorts[isho]
            shostr = getStrike(sho)
            shostr > lostr || continue
            shostr <= strikeExt || break

            res = calcScore(lo, sho, F(shostr))
            score = resScore(res)
            if score > bestScore
                bestScore = score
                best = (res, lo, sho)
            end
        end
    end

    if isnothing(best)
        return nothing
    else
        return (bestScore, ((LegMetaOpen(best[2], Side.long, 1.0), LegMetaOpen(best[3], Side.short, 1.0)), best[1]))
    end
end

function findSpreadEntryShort(oqss, calcScore, strikeExt, debug=false) # , offMax, profMinRaw
    shorts = oqss.put.short
    longs = oqss.put.long
    if isempty(longs)
        bt.log("WARN: oqss.put.long was empty")
        return nothing
    end
    if isempty(shorts)
        bt.log("WARN: oqss.put.short was empty")
        return nothing
    end

    best = nothing
    bestScore = 0.0
    for ilo in Iterators.reverse(eachindex(longs))
        lo = longs[ilo]
        lostr = getStrike(lo)
        lostr >= strikeExt || break
        # TODO: optimize, it's looping over more than necessary
        for isho in Iterators.reverse(eachindex(shorts))
            sho = shorts[isho]
            shostr = getStrike(sho)
            shostr < lostr || continue
            shostr >= strikeExt || break

            res = calcScore(lo, sho, F(shostr))
            score = resScore(res)
            if score > bestScore
                bestScore = score
                best = (res, lo, sho)
            end
        end
    end

    if isnothing(best)
        return nothing
    else
        return (bestScore, ((LegMetaOpen(best[3], Side.short, 1.0), LegMetaOpen(best[2], Side.long, 1.0)), best[1]))
    end
end



netoq2(lo, sho) = netoqL(lo) + netoqS(sho)
netoqL(lo) = bap(QuoteTypes.newQuote(getQuote(lo), DirSQA(Side.long, 1.0, Action.open)), .1)
netoqS(sho) = bap(sho, .1)

function findLongSpreadEntry2(oqss, calcScore, offMax, profMinRaw, strikeExt, debug=false)
    profMin = round(profMinRaw; digits=2)
    shorts = oqss.put.short
    longs = oqss.put.long
    if isempty(longs)
        bt.log("WARN: oqss.put.long was empty")
        return nothing
    end
    if isempty(shorts)
        bt.log("WARN: oqss.put.short was empty")
        return nothing
    end
    ilo = 1
    lo = longs[ilo]
    isho = findfirst(oq -> getBid(oq) > profMin, shorts)
    if isnothing(isho)
        # TODO: why would this happen? I think because it used up all the options and so they're filtered out (ie. in poss)
        # global keepLongShoNothing = (profMin, shorts)
        bt.log("WARN: Long isho was nothing")
        return nothing
    end
    sho = shorts[isho]
    getStrike(sho) <= strikeExt || ( debug && bt.log("Strike $(getStrike(sho)) out of range $(strikeExt) with sufficient profMin $(profMin)") ; return nothing )
    debug && bt.log(@str ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho))

    # move ilo to min by strike
    while getStrike(sho) - getStrike(lo) > offMax
        ilo += 1
        lo = longs[ilo]
    end

    # find isho such that profMin is satisfied
    neto = netoq2(lo, sho)
    debug && bt.log(@str neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho))
    while neto < profMin
        # move isho up to get more prof while still under strikeExt
        getStrike(shorts[isho+1]) <= strikeExt || ( bt.log("WARN: long: Ran out of strikes to get profMin $(profMin)") ; return nothing )
        isho += 1
        sho = shorts[isho]
        # move ilo to make it valid
        while getStrike(sho) - getStrike(lo) > offMax
            ilo += 1
            lo = longs[ilo]
        end
        neto = netoq2(lo, sho)
        debug && bt.log(@str neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho))
    end
    # lo,sho now have valid values, strike is narrow enough and profMin is satisfied
    @assert getStrike(sho) - getStrike(lo) <= offMax
    @assert getStrike(lo) < getStrike(sho)
    @assert neto >= profMin
    score = calcScore(lo, sho)
    best = (score, lo, sho)
    debug && bt.log(@str score neto getStrike(lo) getStrike(sho) strikeExt)

    # Loop through all the other valid states and score them
    while true
        netSho = netoqS(sho)
        # Loop through ilos and score valid ones
        while netSho + netoqL(longs[ilo+1]) >= profMin && ilo+1 < isho
            ilo += 1
            lo = longs[ilo]
            score = calcScore(lo, sho)
            debug && ( neto = netoq2(lo, sho) ; bt.log(@str score neto getStrike(lo) getStrike(sho) strikeExt) )
            if score > best[1]
                best = (score, lo, sho)
            end
        end

        # Try next sho if there are more
        strsho = getStrike(shorts[isho+1])
        if strsho <= strikeExt
            isho += 1
            sho = shorts[isho]

            while strsho - getStrike(lo) > offMax
                ilo += 1
                lo = longs[ilo]
            end
        else
            break
        end
    end
    if netoq2(best[2], best[3]) <= 0.0
        # global keep = (;lo, sho, neto, )
        bt.log("ERROR: long entry returning loss", lo, sho)
        return nothing
    end
    @assert getStrike(best[2]) < getStrike(best[3])
    b = (best[1], LegMetaOpen(best[2], Side.long, 1.0), LegMetaOpen(best[3], Side.short, 1.0))

    if !debug
        neto = bap(b[2:3], .1)
        if neto < profMin
            global kerrBest1 = b
            bt.log("ERROR: (long) invalid neto < profMin $(neto) for profMin $(profMin)")
            findLongSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, true)
            # error("stop")
            return nothing
        end
        # @assert neto >= profMin "neto > 0.0: $(neto), $(profMin), $(b)"

        strikeWidth = abs(getStrike(best[2]) - getStrike(best[3]))
        risk = abs(getStrike(best[2]) - getStrike(best[3])) - neto
        if strikeWidth > offMax || risk > offMax
            global kerrBest2 = b
            bt.log("ERROR: (long) invalid strike width $(strikeWidth) or risk $(risk) for offMax $(offMax)")
            findLongSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, true)
            # error("stop")
            return nothing
        end
    end

    return b
end

function findShortSpreadEntry2(oqss, calcScore, offMax, profMinRaw, strikeExt, debug=false)
    profMin = round(profMinRaw; digits=2)
    shorts = oqss.call.short
    longs = oqss.call.long
    if isempty(longs)
        bt.log("WARN: oqss.call.long was empty")
        return nothing
    end
    if isempty(shorts)
        bt.log("WARN: oqss.call.short was empty")
        return nothing
    end
    # println("short entry: $(length(shorts)) $(length(longs))")
    ilo = lastindex(longs)
    lo = longs[ilo]
    isho = findlast(oq -> getBid(oq) > profMin, shorts)
    if isnothing(isho)
        # TODO: why would this happen? I think because it used up all the options and so they're filtered out (ie. in poss)
        global keepShortShoNothing = (profMin, shorts)
        bt.log("WARN: Short isho was nothing")
        return nothing
    end
    sho = shorts[isho]
    getStrike(sho) >= strikeExt || ( debug && bt.log("Strike $(getStrike(sho)) out of range $(strikeExt) with sufficient profMin $(profMin)") ; return nothing )

    debug && bt.log(@str ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho))
    # move ilo to min by strike
    while getStrike(lo) - getStrike(sho) > offMax
        ilo -= 1
        lo = longs[ilo]
    end

    # find isho such that profMin is satisfied
    neto = netoq2(lo, sho)
    debug && bt.log(@str neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho))
    while neto < profMin
        # move isho down to get more prof while still above strikeExt
        getStrike(shorts[isho-1]) >= strikeExt || ( bt.log("WARN: short: Ran out of strikes to get profMin $(profMin)") ; return nothing )
        isho -= 1
        sho = shorts[isho]
        # move ilo to make it valid
        while getStrike(lo) - getStrike(sho) > offMax
            ilo -= 1
            lo = longs[ilo]
        end
        neto = netoq2(lo, sho)
        debug && bt.log(@str neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho))
    end
    # lo,sho now have valid values, strike is narrow enough and profMin is satisfied
    @assert getStrike(lo) - getStrike(sho) <= offMax
    @assert getStrike(lo) > getStrike(sho)
    @assert neto >= profMin
    score = calcScore(lo, sho)
    best = (score, lo, sho)
    debug && bt.log(@str score neto getStrike(lo) getStrike(sho) strikeExt)

    # Loop through all the other valid states and score them
    while true
        netSho = netoqS(sho)
        # Loop through ilos and score valid ones
        while netSho + netoqL(longs[ilo-1]) >= profMin && ilo-1 > isho
            ilo -= 1
            lo = longs[ilo]
            score = calcScore(lo, sho)
            debug && ( neto = netoq2(lo, sho) ; bt.log(@str score neto getStrike(lo) getStrike(sho) strikeExt) )
            if score > best[1]
                best = (score, lo, sho)
            end
        end

        # Try next sho if there are more
        strsho = getStrike(shorts[isho-1])
        if strsho >= strikeExt
            isho -= 1
            sho = shorts[isho]
            while getStrike(lo) - strsho > offMax
                ilo -= 1
                lo = longs[ilo]
            end
        else
            break
        end
    end
    if netoq2(lo, sho) <= 0.0
        # global keep = (;lo, sho, neto, )
        bt.log("ERROR: short entry returning loss", lo, sho)
        return nothing
    end
    @assert getStrike(best[3]) < getStrike(best[2])
    b = (best[1], LegMetaOpen(best[3], Side.short, 1.0), LegMetaOpen(best[2], Side.long, 1.0))

    if !debug
        neto = bap(b[2:3], .1)
        if neto < profMin
            global kerrBest1 = b
            bt.log("ERROR: (short) invalid neto < profMin $(neto) for profMin $(profMin)")
            findShortSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, true)
            # error("stop")
            return nothing
        end
        # @assert neto >= profMin "neto > 0.0: $(neto), $(profMin), $(b)"

        strikeWidth = abs(getStrike(best[2]) - getStrike(best[3]))
        risk = abs(getStrike(best[2]) - getStrike(best[3])) - neto
        if strikeWidth > offMax || risk > offMax
            global kerrBest2 = b
            bt.log("ERROR: (short) invalid strike width $(strikeWidth) or risk $(risk) for offMax $(offMax)")
            findShortSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, true)
            # error("stop")
            return nothing
        end
    end

    # println("short: returning b")
    return b
end
#endregion

#region Calcs
function openInfo(lo, sho)::Tuple{Currency,Currency} # NamedTuple{(:neto,:risk), Tuple{Currency,Currency}}
    neto = netoq2(lo, sho)
    neto > 0 || return (neto, CZ)
    if neto > 0
        risk = abs(getStrike(lo) - getStrike(sho)) - neto
    end
    return (neto, risk)
end
#endregion

end