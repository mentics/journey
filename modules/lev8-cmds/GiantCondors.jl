module GiantCondors
using Dates
using SH, BaseTypes, SmallTypes, LegMetaTypes
using DateUtil, ChainUtil, Pricing
using Markets, Expirations, Chains
import CmdPos:xlegs
import Backtests as bt
import Backtests:rond

#region Public
function live(side=Side.long; xbdays=40:80, offMax=40.0, profMin=0.8, moveMin=0.2)
    date = market().startDay
    curp = market().curp
    strikeExt = curp * (side == Side.long ? (1.0 - moveMin) : (1.0 + moveMin))
    getOqss = xpir -> ChainUtil.getOqss(chain(xpir).chain, curp, xlegs(xpir))
    xpirs = DateUtil.matchAfter(date, expirs(), xbdays)
    findGC(getOqss, xpirs, date, side, offMax, profMin, strikeExt)
end

function strat(acct)
    p = (;
        MaxOpen = 10000,
        XpirBdays = 40:80,
        Put = (;moveMin=.2, offMax=30.0, profMin=0.8, take=0.4),
        Call = (;moveMin=.14, off=30.0, profMin=0.8, take=0.4),
    )
    bt.checkClose((bt.checkSides,), acct, p)

    length(acct[:poss]) < p.MaxOpen || return # ( println("Hit max open") ; return )
    curp = acct[:curp]

    lms = back(acct, Side.long; p.Put...)
    if !isnothing(lms)
        neto = bap(lms, .1)
        # TODO:
        # julia> bt.run(gc.strat; maxSeconds=99999999)
        # ERROR: AssertionError: neto > 0.0
        # Stacktrace:
        #  [1] strat(acct::Dict{Symbol, Any})
        #    @ GiantCondors C:\data\julia\journey\modules\lev8-cmds\GiantCondors.jl:35
        #  [2] run(f::typeof(GiantCondors.strat); maxSeconds::Int64)
        #    @ Backtests C:\data\julia\journey\modules\lev8-cmds\backtest\Backtests.jl:98
        #  [3] top-level scope
        #    @ REPL[146]:1
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

function back(acct, side; xbdays=40:80, offMax=40.0, profMin=0.8, moveMin=0.2, kws...)
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

import Kelly
function calcScore1(tmult, lo, sho)
    # TODO: consider greeks?
    risk = F(abs(getStrike(lo) - getStrike(sho)))
    ret = F(bap(sho, .1) - bap(lo, .1))
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
    ilo = 1
    lo = longs[ilo]
    isho = findfirst(oq -> getBid(oq) > profMin, shorts)
    sho = shorts[isho]
    getStrike(sho) <= strikeExt || return nothing

    while getStrike(sho) - getStrike(lo) > offMax
        ilo += 1
        lo = longs[ilo]
    end
    best = (calcScore(lo, sho), lo, sho)

    neto = getBid(sho) - getAsk(lo) # Choosing worst case at this stage
    while neto < profMin
        isho += 1
        sho = shorts[isho]
        while getStrike(sho) - getStrike(lo) > offMax
            ilo += 1
            lo = longs[ilo]
        end
        neto = getBid(sho) - getAsk(lo) # Choosing worst case at this stage
    end

    while getStrike(sho) <= strikeExt
        shoBid = getBid(sho)
        while true
            if shoBid - getAsk(longs[ilo+1]) < profMin
                lo = longs[ilo]
                break
            else
                ilo += 1
            end
        end

        score = calcScore(lo, sho)
        if score > best[1]
            best = (score, lo, sho)
        end
        isho += 1
        sho = shorts[isho]
    end
    return (best[1], LegMetaOpen(best[2], Side.long, 1.0), LegMetaOpen(best[3], Side.short, 1.0))
end

function findShortSpreadEntry(oqss, calcScore, offMax, profMin, strikeExt)
    shorts = oqss.call.short
    longs = oqss.call.long
    ilo = lastindex(longs)
    isho = findlast(oq -> getBid(oq) > profMin, shorts)
    lo = longs[ilo]
    sho = shorts[isho]
    getStrike(sho) >= strikeExt || return nothing

    while getStrike(lo) - getStrike(sho) > offMax
        ilo -= 1
        lo = longs[ilo]
    end
    best = (calcScore(lo, sho), lo, sho)

    neto = getBid(sho) - getAsk(lo) # Choosing worst case at this stage
    while neto < profMin
        # @show ilo isho neto
        isho -= 1
        sho = shorts[isho]
        while getStrike(lo) - getStrike(sho) > offMax
            ilo -= 1
            lo = longs[ilo]
        end
        neto = getBid(sho) - getAsk(lo) # Choosing worst case at this stage
        # @show getStrike(sho) getStrike(lo) neto
    end

    while getStrike(sho) >= strikeExt
        shoBid = getBid(sho)
        while true
            if shoBid - getAsk(longs[ilo-1]) < profMin
                lo = longs[ilo]
                break
            else
                ilo -= 1
            end
        end

        score = calcScore(lo, sho)
        if score > best[1]
            best = (score, lo, sho)
        end
        isho -= 1
        sho = shorts[isho]
    end
    return (best[1], LegMetaOpen(best[3], Side.short, 1.0), LegMetaOpen(best[2], Side.long, 1.0))
end
#endregion

end