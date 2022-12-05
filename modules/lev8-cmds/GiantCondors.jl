module GiantCondors
using Dates
using SH, BaseTypes, SmallTypes, LegMetaTypes
using DateUtil, ChainUtil, LogUtil, Pricing
using Markets, Expirations, Chains
import CmdPos:xlegs
import Backtests as bt
import Backtests:rond

#region Public
function getParams()
    return (;
        MaxOpen = 100000,
        Put = (;xbdays=20:84, moveMin=.18, offMax=20.0, profMin=0.4, take=0.12),
        Call = (;xbdays=20:84, moveMin=.14, offMax=20.0, profMin=0.3, take=0.1),
    )
end

function live(side=Side.long) # ; xbdays=40:80, offMax=40.0, profMin=0.8, moveMin=0.2)
    p = getParams()
    ps = side == Side.long ? p.Put : p.Call
    date = market().startDay
    curp = market().curp
    strikeExt = curp * (side == Side.long ? (1.0 - ps.moveMin) : (1.0 + ps.moveMin))
    getOqss = xpir -> ChainUtil.getOqss(chain(xpir).chain, curp, xlegs(xpir))
    xpirs = DateUtil.matchAfter(date, expirs(), ps.xbdays)
    res = findGC(getOqss, xpirs, date, side, ps.offMax, ps.profMin, strikeExt)
    return isnothing(res) ? nothing : collect(res)
end

function strat(acct)
    p = getParams()
    bt.checkClose((bt.checkSides,), acct, p)

    length(acct[:poss]) < p.MaxOpen || return # ( println("Hit max open") ; return )
    curp = acct[:curp]

    lms = back(acct, Side.long; p.Put...)
    if !isnothing(lms)
        neto = bap(lms, .1)
        @assert neto > 0.0
        rat = getStrike(lms[2]) / curp
        bt.openTrade(acct, lms, neto, "long spread: rat=$(rond(rat)), neto=$(neto)", abs(getStrike(lms[1]) - getStrike(lms[2])) - neto)
    end
    lms = back(acct, Side.short; p.Call...)
    if !isnothing(lms)
        neto = bap(lms, .1)
        @assert neto > 0.0
        rat = getStrike(lms[1]) / curp
        bt.openTrade(acct, lms, neto, "short spread: rat=$(rond(rat)), neto=$(neto)", abs(getStrike(lms[1]) - getStrike(lms[2])) - neto)
    end
end

function back(acct, side; xbdays, offMax, profMin, moveMin, kws...)
    date = acct[:date]
    xoqss = acct[:xoqss]
    getOqss = xpir -> xoqss[xpir]
    xpirs = DateUtil.matchAfter(date, acct[:xpirs], xbdays)
    # TODO: have use the recent high (period = xpir days) and require rat above that
    strikeExt = acct[:curp] * (side == Side.long ? (1.0 - moveMin) : (1.0 + moveMin))
    return findGC(getOqss, xpirs, date, side, offMax, profMin, strikeExt)
end
#endregion

#region Local
function findGC(getOqss, xpirs, date, side, offMax, profMin, strikeExt)
    findEntry = side == Side.long ? findLongSpreadEntry : findShortSpreadEntry
    lmsBest = nothing
    scoreBest = 0.0
    for xpir in xpirs
        tmult = timult(date, xpir)
        calcScore = (lo, sho) -> calcScore1(tmult, lo, sho)
        oqss = getOqss(xpir)
        res = findEntry(oqss, calcScore, offMax, profMin, strikeExt)
        !isnothing(res) || continue
        score, lm1, lm2 = res
        if score > scoreBest
            lmsBest = (lm1, lm2)
            scoreBest = score
        end
    end
    return lmsBest
end

netoq2(lo, sho) = netoqL(lo) + netoqS(sho)
netoqL(lo) = bap(QuoteTypes.newQuote(getQuote(lo), DirSQA(Side.long, 1.0, Action.open)), .1)
netoqS(sho) = bap(sho, .1)

import Kelly, QuoteTypes
function calcScore1(tmult, lo, sho)
    # TODO: consider greeks?
    # theta = getGreeks(lo).theta - getGreeks(sho).theta
    # @show getGreeks(sho).theta getGreeks(lo).theta theta
    # theta >= 0.01 || return -1000.0
    risk = F(abs(getStrike(lo) - getStrike(sho)))
    # Wrong: ret = F(bap(sho, .1) - bap(lo, .1))
    ret = F(netoq2(lo, sho))
    ret > 0.0 || ( println("Found ret < 0.0") ; return -100.0 )
    rate = calcRate(tmult, ret, risk)
    kel = Kelly.simple(0.99, ret, risk)
    score = rate * kel
    # println("calcScore:")
    # @show tmult ret risk rate kel score
    return score
end

function findLongSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt)
    shorts = oqss.put.short
    longs = oqss.put.long
    if isempty(longs)
        println("oqss.put.long was empty")
        return nothing
    end
    if isempty(shorts)
        println("oqss.put.short was empty")
        return nothing
    end
    ilo = 1
    lo = longs[ilo]
    isho = findfirst(oq -> getBid(oq) > profMin, shorts)
    sho = shorts[isho]
    getStrike(sho) <= strikeExt || return nothing
    # @show ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)

    # move ilo to min by strike
    while getStrike(sho) - getStrike(lo) > offMax
        ilo += 1
        lo = longs[ilo]
    end

    # find isho such that profMin is satisfied
    neto = netoq2(lo, sho)
    # @show neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
    while neto < profMin
        # move isho up to get more prof while still under strikeExt
        getStrike(shorts[isho+1]) <= strikeExt || return nothing
        isho += 1
        sho = shorts[isho]
        # move ilo to make it valid
        while getStrike(sho) - getStrike(lo) > offMax
            ilo += 1
            lo = longs[ilo]
        end
        neto = netoq2(lo, sho)
        # @show neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
    end
    # lo,sho now have valid values, strike is narrow enough and profMin is satisfied
    @assert getStrike(sho) - getStrike(lo) <= offMax
    @assert neto >= profMin
    score = calcScore(lo, sho)
    best = (score, lo, sho)
    # @show score neto getStrike(lo) getStrike(sho) strikeExt

    # Loop through all the other valid states and score them
    while true
        netSho = netoqS(sho)
        # Loop through ilos and score valid ones
        while netSho + netoqL(longs[ilo+1]) >= profMin
            ilo += 1
            lo = longs[ilo]
            score = calcScore(lo, sho)
            # neto = netoq2(lo, sho) ; @show score neto getStrike(lo) getStrike(sho) strikeExt
            if score > best[1]
                best = (score, lo, sho)
            end
        end

        # Try next sho if there are more
        if getStrike(shorts[isho+1]) <= strikeExt
            isho += 1
            sho = shorts[isho]
        else
            break
        end
    end
    if netoq2(lo, sho) <= 0.0
        # global keep = (;lo, sho, neto, )
        @log error "long entry returning loss" lo sho
        return nothing
    end
    return (best[1], LegMetaOpen(best[2], Side.long, 1.0), LegMetaOpen(best[3], Side.short, 1.0))
end

function findShortSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt)
    shorts = oqss.call.short
    longs = oqss.call.long
    if isempty(longs)
        println("oqss.call.long was empty")
        return nothing
    end
    if isempty(shorts)
        println("oqss.call.short was empty")
        return nothing
    end
    ilo = lastindex(longs)
    lo = longs[ilo]
    isho = findlast(oq -> getBid(oq) > profMin, shorts)
    sho = shorts[isho]
    getStrike(sho) >= strikeExt || return nothing

    # @show ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
    # move ilo to min by strike
    while getStrike(lo) - getStrike(sho) > offMax
        ilo -= 1
        lo = longs[ilo]
    end

    # find isho such that profMin is satisfied
    neto = netoq2(lo, sho)
    # @show neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
    while neto < profMin
        # move isho down to get more prof while still above strikeExt
        getStrike(shorts[isho-1]) >= strikeExt || return nothing
        isho -= 1
        sho = shorts[isho]
        # move ilo to make it valid
        while getStrike(lo) - getStrike(sho) > offMax
            ilo -= 1
            lo = longs[ilo]
        end
        neto = netoq2(lo, sho)
        # @show neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
    end
    # lo,sho now have valid values, strike is narrow enough and profMin is satisfied
    @assert getStrike(lo) - getStrike(sho) <= offMax
    @assert neto >= profMin
    score = calcScore(lo, sho)
    best = (score, lo, sho)
    # @show score neto getStrike(lo) getStrike(sho) strikeExt

    # Loop through all the other valid states and score them
    while true
        netSho = netoqS(sho)
        # Loop through ilos and score valid ones
        while netSho + netoqL(longs[ilo-1]) >= profMin
            ilo -= 1
            lo = longs[ilo]
            score = calcScore(lo, sho)
            # neto = netoq2(lo, sho) ; @show score neto getStrike(lo) getStrike(sho) strikeExt
            if score > best[1]
                best = (score, lo, sho)
            end
        end

        # Try next sho if there are more
        if getStrike(shorts[isho-1]) >= strikeExt
            isho -= 1
            sho = shorts[isho]
        else
            break
        end
    end
    if netoq2(lo, sho) <= 0.0
        # global keep = (;lo, sho, neto, )
        @log error "short entry returning loss" lo sho
        return nothing
    end
    return (best[1], LegMetaOpen(best[3], Side.short, 1.0), LegMetaOpen(best[2], Side.long, 1.0))
end
#endregion

end