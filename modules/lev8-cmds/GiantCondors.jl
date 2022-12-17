module GiantCondors
using Dates
using SH, BaseTypes, SmallTypes, LegMetaTypes
using DateUtil, ChainUtil, LogUtil, Pricing
import HistData as hd
using Markets, Expirations, Chains
import CmdPos:xlegs
import Backtests as bt
import Backtests:rond

#region Public
function getParamsLive()
    # TODO: When we find better ones from testing, copy them to here
    return (;
        MaxOpen = 100,
        Put = (;xbdays=10:84, moveMin=.08, offMax=20.0, profMin=0.6, take=0.12),
        Call = (;xbdays=10:84, moveMin=.06, offMax=20.0, profMin=0.4, take=0.12),
    )
end

# TODO: max open take into account long and short balance
function getParams()
    return (;
        MaxOpen = 100,
        Put = (;xbdays=10:84, moveMin=.14, offMax=30.0, profMin=.8, take=0.16),
        Call = (;xbdays=10:84, moveMin=.12, offMax=30.0, profMin=.8, take=0.16),
    )
end

import Kelly, QuoteTypes
function calcScore1(ctx, tmult, lo, sho)
    # # TODO: consider greeks?
    # theta = getGreeks(lo).theta - getGreeks(sho).theta
    # delta = getGreeks(lo).delta - getGreeks(sho).delta
    # # @show getGreeks(sho).theta getGreeks(lo).theta theta
    # # theta >= 0.01 || return -1000.0
    # theta >= abs(delta)/3 || return -1000.0
    # println("delta: $(delta) theta: $(theta)")
    strlo = getStrike(lo)
    strsho = getStrike(sho)
    risk = F(abs(strlo - strsho))
    # Wrong: ret = F(bap(sho, .1) - bap(lo, .1))
    ret = F(netoq2(lo, sho))
    ret > 0.0 || ( println("Found ret < 0.0") ; return -100.0 )
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
    return score
end

makeCtx(acct) = (;
    curp = acct[:curp]
)

function live(side=Side.long) # ; xbdays=40:80, offMax=40.0, profMin=0.8, moveMin=0.2)
    p = getParamsLive()
    ps = side == Side.long ? p.Put : p.Call
    date = market().startDay
    curp = market().curp
    # TODO: get current day high/low
    strikeExt = curp * (side == Side.long ? (1.0 - ps.moveMin) : (1.0 + ps.moveMin))
    getOqss = xpir -> ChainUtil.getOqss(chain(xpir).chain, curp, xlegs(xpir))
    xpirs = DateUtil.matchAfter(date, expirs(), ps.xbdays)
    res = findGC((;curp), getOqss, xpirs, date, side, ps.offMax, ps.profMin, strikeExt)
    return isnothing(res) ? nothing : collect(res)
end

function stratDay(acct)
    acct[:openLong] = nothing
    acct[:openShort] = nothing
end

function strat(acct)
    p = getParams()
    bt.checkClose((bt.checkSides,), acct, p)

    length(acct[:poss]) < p.MaxOpen || return # ( println("Hit max open") ; return )
    # length(acct[:todayOpens]) < 2 || return # Only one open per side per day
    # curp = acct[:curp]

    if isnothing(acct[:openLong])
        lo = acct[:extrema].lo
        strikeExt = lo * (1.0 - p.Put.moveMin)
        # @show "long" lo strikeExt
        lms = back(acct, Side.long, strikeExt; p.Put...)
        if !isnothing(lms)
            neto = bap(lms, .1)
            risk = abs(getStrike(lms[1]) - getStrike(lms[2])) - neto
            @assert neto > 0.0
            @assert risk <= p.Put.offMax
            # rat = getStrike(lms[2]) / curp
            rat = getStrike(lms[2]) / lo
            acct[:openLong] = bt.openTrade(acct, lms, neto, "long spread: rat=$(rond(rat)), neto=$(neto)", risk)
        end
    end
    if isnothing(acct[:openShort])
        hi = acct[:extrema].hi
        strikeExt = hi * (1.0 + p.Call.moveMin)
        lms = back(acct, Side.short, strikeExt; p.Call...)
        # println("short $(acct[:curp]) $(hi) $(strikeExt) $(isnothing(lms))")
        if !isnothing(lms)
            neto = bap(lms, .1)
            risk = abs(getStrike(lms[1]) - getStrike(lms[2])) - neto
            @assert neto > 0.0
            @assert risk <= p.Call.offMax
            # rat = getStrike(lms[1]) / curp
            rat = getStrike(lms[1]) / hi
            acct[:openShort] = bt.openTrade(acct, lms, neto, "short spread: rat=$(rond(rat)), neto=$(neto)", risk)
        end
    end
end

function back(acct, side, strikeExt; xbdays, offMax, profMin, kws...)
    date = acct[:date]
    xoqss = acct[:xoqss]
    getOqss = xpir -> xoqss[xpir]
    xpirs = DateUtil.matchAfter(date, acct[:xpirs], xbdays)
    return findGC(makeCtx(acct), getOqss, xpirs, date, side, offMax, profMin, strikeExt)
end
#endregion

#region Local
function findGC(ctx, getOqss, xpirs, date, side, offMax, profMin, strikeExt)
    findEntry = side == Side.long ? findLongSpreadEntry : findShortSpreadEntry
    lmsBest = nothing
    scoreBest = 0.0
    for xpir in xpirs
        tmult = timult(date, xpir)
        calcScore = (lo, sho) -> calcScore1(ctx, tmult, lo, sho)
        oqss = getOqss(xpir)
        res = findEntry(oqss, calcScore, offMax, profMin, strikeExt)
        !isnothing(res) || continue
        score, lm1, lm2 = res
        # println("findGC: findEntry was not nothing $(score)")
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

function findLongSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, debug=false)
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
    if isnothing(isho)
        # TODO: why would this happen? I think because it used up all the options and so they're filtered out (ie. in poss)
        global keepLongShoNothing = (profMin, shorts)
        println("Long isho was nothing")
        return nothing
    end
    sho = shorts[isho]
    getStrike(sho) <= strikeExt || return nothing
    debug && @show ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)

    # move ilo to min by strike
    while getStrike(sho) - getStrike(lo) > offMax
        ilo += 1
        lo = longs[ilo]
    end

    # find isho such that profMin is satisfied
    neto = netoq2(lo, sho)
    debug && @show neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
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
        debug && @show neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
    end
    # lo,sho now have valid values, strike is narrow enough and profMin is satisfied
    @assert getStrike(sho) - getStrike(lo) <= offMax
    @assert getStrike(lo) < getStrike(sho)
    @assert neto >= profMin
    score = calcScore(lo, sho)
    best = (score, lo, sho)
    debug && @show score neto getStrike(lo) getStrike(sho) strikeExt

    # Loop through all the other valid states and score them
    while true
        netSho = netoqS(sho)
        # Loop through ilos and score valid ones
        while netSho + netoqL(longs[ilo+1]) >= profMin && ilo+1 < isho
            ilo += 1
            lo = longs[ilo]
            score = calcScore(lo, sho)
            debug && ( neto = netoq2(lo, sho) ; @show score neto getStrike(lo) getStrike(sho) strikeExt )
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
    @assert getStrike(best[2]) < getStrike(best[3])
    b = (best[1], LegMetaOpen(best[2], Side.long, 1.0), LegMetaOpen(best[3], Side.short, 1.0))

    if !debug
        neto = bap(b[2:3], .1)
        if neto < profMin
            global kerrBest1 = b
            println("ERROR: (long) invalid neto < profMin $(neto) for profMin $(profMin)")
            findLongSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, true)
            # error("stop")
            return nothing
        end
        # @assert neto >= profMin "neto > 0.0: $(neto), $(profMin), $(b)"

        strikeWidth = abs(getStrike(best[2]) - getStrike(best[3]))
        risk = abs(getStrike(best[2]) - getStrike(best[3])) - neto
        if strikeWidth > offMax || risk > offMax
            global kerrBest2 = b
            println("ERROR: (long) invalid strike width $(strikeWidth) or risk $(risk) for offMax $(offMax)")
            findLongSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, true)
            # error("stop")
            return nothing
        end
    end

    return b
end

function findShortSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, debug=false)
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
    # println("short entry: $(length(shorts)) $(length(longs))")
    ilo = lastindex(longs)
    lo = longs[ilo]
    isho = findlast(oq -> getBid(oq) > profMin, shorts)
    if isnothing(isho)
        # TODO: why would this happen? I think because it used up all the options and so they're filtered out (ie. in poss)
        global keepShortShoNothing = (profMin, shorts)
        println("Short isho was nothing")
        return nothing
    end
    sho = shorts[isho]
    getStrike(sho) >= strikeExt || return nothing # ( println("strike not enough $(getStrike(sho)) $(sho)") ; return nothing )

    debug && @show ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
    # move ilo to min by strike
    while getStrike(lo) - getStrike(sho) > offMax
        ilo -= 1
        lo = longs[ilo]
    end

    # find isho such that profMin is satisfied
    neto = netoq2(lo, sho)
    debug && @show neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
    while neto < profMin
        # move isho down to get more prof while still above strikeExt
        getStrike(shorts[isho-1]) >= strikeExt || ( println("short: Ran out of strikes to get profMin $(profMin)") ; return nothing )
        isho -= 1
        sho = shorts[isho]
        # move ilo to make it valid
        while getStrike(lo) - getStrike(sho) > offMax
            ilo -= 1
            lo = longs[ilo]
        end
        neto = netoq2(lo, sho)
        debug && @show neto ilo isho getStrike(lo) getStrike(sho) strikeExt netoqL(lo) netoqS(sho)
    end
    # lo,sho now have valid values, strike is narrow enough and profMin is satisfied
    @assert getStrike(lo) - getStrike(sho) <= offMax
    @assert getStrike(lo) > getStrike(sho)
    @assert neto >= profMin
    score = calcScore(lo, sho)
    best = (score, lo, sho)
    debug && @show score neto getStrike(lo) getStrike(sho) strikeExt

    # Loop through all the other valid states and score them
    while true
        netSho = netoqS(sho)
        # Loop through ilos and score valid ones
        while netSho + netoqL(longs[ilo-1]) >= profMin && ilo-1 > isho
            ilo -= 1
            lo = longs[ilo]
            score = calcScore(lo, sho)
            debug && ( neto = netoq2(lo, sho) ; @show score neto getStrike(lo) getStrike(sho) strikeExt )
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
        @logboth error "short entry returning loss" lo sho
        return nothing
    end
    @assert getStrike(best[3]) < getStrike(best[2])
    b = (best[1], LegMetaOpen(best[3], Side.short, 1.0), LegMetaOpen(best[2], Side.long, 1.0))

    if !debug
        neto = bap(b[2:3], .1)
        if neto < profMin
            global kerrBest1 = b
            println("ERROR: (short) invalid neto < profMin $(neto) for profMin $(profMin)")
            findShortSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, true)
            # error("stop")
            return nothing
        end
        # @assert neto >= profMin "neto > 0.0: $(neto), $(profMin), $(b)"

        strikeWidth = abs(getStrike(best[2]) - getStrike(best[3]))
        risk = abs(getStrike(best[2]) - getStrike(best[3])) - neto
        if strikeWidth > offMax || risk > offMax
            global kerrBest2 = b
            println("ERROR: (short) invalid strike width $(strikeWidth) or risk $(risk) for offMax $(offMax)")
            findShortSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt, true)
            # error("stop")
            return nothing
        end
    end

    # println("short: returning b")
    return b
end
#endregion

end