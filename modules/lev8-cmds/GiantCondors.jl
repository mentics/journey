module GiantCondors
using Dates
using SH, BaseTypes, SmallTypes, LegMetaTypes
using DateUtil, Pricing
import HistData as HD
using Markets, Expirations, Chains
import CmdPos:xlegs
import BacktestSimple as bt
import BacktestSimple:rond
import OptionUtil
import ChainUtil as cu

# TODO: add check to roll dangerous puts/(calls?) when they get too close to expiration
# TODO: try out using fromPrice = curp now with the rolling
# TODO: Roll at the best time: might need to be able to detect when last call is before expiration so we can do it then
# TODO: Need to restrict how much can widen rolls by available margin

# TODO:
# When going into the money, the spread gets weaker because the "inner" strike is now further away from curp, so has less extrinsic,
# so you get less than the intrinsic spread price. That makes it harder to match the basis for rolling.
# Instead of that, how about "doubling down": increase the number of contracts and go at the money to maximize intrinsic spread value?

# TODO: take rate for rolling doesn't work right: rate needs to be calculated considering basis?

#region Params
function getParamsLive()
    return (;
        balInit = 100,
        ExtremaBdays = 20,
        marginMaxRat = 0.6,
        marginPerDayRat = 0.1,
        maxPerTrade = 1,
        Put = (;xbdays=(16,84), probMin=.98, rollDay = 1),
        Call = (;xbdays=(16,84), probMin=.98, rollDay = 1),
    )
end

function getParams()
    return (;
        balInit = 1000,
        ExtremaBdays = 20,
        marginMaxRat = 0.6,
        marginPerDayRat = .1,
        maxPerTrade = 100,
        Put = (;xbdays=(16,84), probMin=.98, rollDay = 1),
        Call = (;xbdays=(16,84), probMin=.98, rollDay = 1),
    )
end
#endregion

#region Scoring
import Kelly
function calcScore3(ctx, prob, side, tmult, innerStrike::Float64, lo, sho; all=false)
    minRate=0.01
    minRateAdj=0.01
    minNetVal=0.2
    minKel=-10.01
    MoneyValueAPR=0.05
    curp = ctx.curp

    # 2020-07-17 (235.000, 241.000) basis:-4.680 spreadWidth:6.000 net.1:1.930 net.5:2.610 (extrin1 = 33.145, extrin2 = 32.695, extrinDiff = 0.450)

    # netp1 = bt.netOpen(lo, Side.long, sho, Side.short, 0.1)
    # netp5 = bt.netOpen(lo, Side.long, sho, Side.short, 0.5)
    # spreadWidth = -(getStrike.((sho, lo))...)
    # spreadWidth >= -basis || return
    # # s1 = getStrike(lo)
    # # s2 = getStrike(sho)
    # # net1 = bt.netOpen(lo, Side.long)
    # # net2 = bt.netOpen(sho, Side.short)
    # extrin1 = OptionUtil.calcExtrin(lo, curp)
    # extrin2 = OptionUtil.calcExtrin(sho, curp)
    # extrinDiff = extrin1[3] - extrin2[3]
    # println("$(getExpiration(lo)) $(getStrike.((lo, sho))) basis:$(basis) spreadWidth:$(spreadWidth) net.1:$(netp1) net.5:$(netp5) $((;extrin1=extrin1[3], extrin2=extrin2[3], extrinDiff))")

    netVal, risk = bt.calcVal(1, lo, sho, CZ)
    @assert risk > 0.0 "risk $(risk) <= 0.0 $((;netVal, strikes=getStrike.((lo, sho))))"
    netVal >= 0.01 || ( deb("netVal must be positive") ; return nothing )
    # println((;neto=bt.netOpen(lo, Side.long, sho, Side.short), netVal=netVal, risk))
    all || netVal >= minNetVal || ( deb("neto $(netVal) < $(minNetVal)") ; return nothing )

    rate = calcRate(tmult, netVal, risk)
    all || rate >= minRate || ( deb("rate $(rate) < $(minRate)") ; return nothing )
    # Subtract out the worth of the money over time to adjust for the kelly calc
    # which is using 5% here.
    # And 0.01 buy back
    ExpDurRatioAvg = 1.0 # 0.5
    ret = round(netVal - .01 - risk * (MoneyValueAPR * ExpDurRatioAvg / tmult), RoundDown; digits=2)
    all || ret > 0.0 || ( deb("ret $(ret) <= 0.0 $(netVal)") ; return nothing )
    rateAdj = calcRate(tmult, ret, risk)
    all || rateAdj > minRateAdj || ( deb("rateAdj $(rateAdj) <= $(minRateAdj) $(rate) $(ret)") ; return nothing )

    mov = innerStrike / curp
    winRate = side == Side.long ? ProbUtil.cdfFromRight(prob, innerStrike) : ProbUtil.cdfFromLeft(prob, innerStrike)
    theta = getGreeks(lo).theta - getGreeks(sho).theta
    kel = Kelly.simple(winRate, F(ret), F(risk))
    if winRate <= 0.0 || ret <= 0.0
        score = rate * (1.0 + theta) # * theta # TODO: need to deal with if theta is negative
    else
        all || kel > minKel || ( deb("kel $(kel) <= $(minKel)") ; return nothing )
        score = kel * rate * (1.0 + theta) # * theta
    end

    # return (;score=kel * rate, neto, risk, ret, kel, rate, winRate, mov, prob)
    return (;score, netVal, risk, kel, ret, rate, rateAdj, winRate, mov, theta)
end
# deb(str) = startswith(str, "rateAdj") ? println(str) : return
# deb(str) = println(str)
deb(str) = return
resScore(r)::Float64 = r[1]
resNetVal(r)::Float64 = r[2]
resRisk(r)::Float64 = r[3]
resKel(r)::Float64 = r[4]
#endregion

#region Rolling
function openTradeRoll(acct, params, trade)
    MinNetVal = 0.2

    date = acct.date
    @assert date <= bt.getTradeExpiration(trade) # to make sure I don't test incorrectly
    curp = acct.curp
    vix100 = F(acct.vix)*100
    dateMin = bdaysAfter(date, 20)
    dateMax = bdaysAfter(date, 200) # Date(2025,1,1)
    checkXpirs = filter(x -> dateMin < x < dateMax, acct.xpirs[2:end])
    # global checkXpirs = DateUtil.matchAfter(date, acct.xpirs, params.xbdays)
    multiple = trade.multiple
    side = bt.getTradeSide(trade)
    basisAll = multiple * bt.calcTradeCurClose(trade)
    # TODO: using min is long specific
    strikeExt = getInnerStrike(trade) # min(acct.extrema.lo, getInnerStrike(trade)) # , C(-.1))
    spreadWidth = bt.getSpreadWidth(trade.legs)
    findEntry = side == Side.long ? findSpreadEntryLong : findSpreadEntryShort

    # multipleNew = multiple # round(Int, 1.2 * trade.multiple, RoundUp)
    maxWidthNew = 2 + 1.5 * spreadWidth
    # maxWidthNew = 10 * spreadWidth

    best = nothing
    bestScore = -Inf

    for xpir in checkXpirs
        # @show xpir
        tmult = timult(date, xpir)
        # prob = ProbKde.probToClose(F(curp), vix100, acct.ts, xpir)
        # It may add to multiple, so we can adjust the basis we need to meet
        # basisAdj = round(trade.multiple * basis/(multipleNew), RoundUp; digits=2)

        # calcScore = (lo, sho, innerStrike) -> calcScore3((;curp), prob, side, basisAdj, tmult, innerStrike, lo, sho; all=true)

        # calcScore = (lo, sho, innerStrike) -> begin
        #     neto, risk = openInfo(lo, sho, basisAdj)
        #     rate = calcRate(tmult, neto, risk)
        #     return rate
        # end

        calcScore = (lo, sho, innerStrike) -> begin
            # riskNet < trade.risk || ( println("risk increased $(neto) $(riskNet) $(basis) ", (;riskNet, trisk=trade.risk)) ; return ) # Avoid increasing risk (ie. net margin). TODO: we could allow a little increase
            # risk < trade.risk || return # Avoid increasing risk (ie. net margin). TODO: we could allow a little increase
            neto = bt.netOpen(lo, Side.long, sho, Side.short)
            neto > MinNetVal || return
            # println("weird")
            k = curp / innerStrike # TODO: long specific
            if multiple * neto > (-basisAll + MinNetVal)
                multNew = multiple
                multRat = 1.0
            else
                # netVal = neto + basisAll / multiple
                # neto * multNew + basisAll > multNew * MinNetVal
                # basisAll > multNew * (MinNetVal - neto)
                # multNew > basisAll / (neto - MinNetVal)
                multNew = round(Int, -F(basisAll) / (neto - MinNetVal), RoundUp)
                multRat = multNew / multiple
            end
            netVal, riskVal = bt.calcVal(multNew, lo, sho, basisAll)
            valNew = multNew * neto
            valOld = (-basisAll + MinNetVal - 0.01)
            if valNew < valOld
                println((;basisAll, neto, multiple, multNew, multRat, innerStrike, netVal, riskVal, spreadWidth=bt.getSpreadWidth((lo, sho))))
                error((;valNew, valOld))
            end
            rate = calcRate(tmult, netVal, riskVal)
            rate > 0.0 || error("rate <= 0 when shouldn't") # return # later calcs assume it's positive
            score = k * rate / multRat
            # TODO: check available margin balance
            return (;score, multNew, netVal, riskVal, rate)
        end

        # oqss = acct.xoqss[xpir]
        oqss = acct.xoqssAll[xpir]
        res = findEntry(oqss, calcScore, strikeExt; maxStrikeWidth=maxWidthNew)
        !isnothing(res) || continue
        score = res[1]
        if score > bestScore
            best = res[2]
            bestScore = score
        end
    end
    if !isnothing(best)
        lms, r = best
        outBdays = bdays(date, getExpiration(lms))
        widthNew = bt.getSpreadWidth(lms)
        multNew = r[2]
        dmult = "$(multiple)->$(multNew)"
        dwidth = "$(spreadWidth)->$(widthNew)"
        driskVal = "$(bt.getTradeRiskVal(trade))->$(widthNew - bt.netOpen(lms) - basisAll/multNew)"
        strikeImprov = getInnerStrike(side, lms) - getInnerStrike(trade)
        tradeNew = bt.openTrade(acct, lms, multNew, basisAll, "rolling id:$(trade.id) $((;side, outBdays, dmult, dwidth, driskVal, strikeImprov, r=string(r)))")
        return tradeNew
    end

    bt.log("ERROR: Failed to roll trade: $(trade.id)")
    return
end
#endregion

#region Exit
function checkExit(acct, trade, params)
    rateCur = bt.calcTradeCurRateVal(trade, acct.date)
    mult = 2.0
    if rateCur > mult * trade.open.rate
        return "cur trade rate $(rateCur) > $(mult) * trade.open.rate $(mult * trade.open.rate)"
    end

    # trade.rate >= p.takeRateMin && return "rate high enough $(t.rate)"

    # TODO: use prob to determine if > % chance of being a worse loss than current loss
    # theta = getGreeks(t.lmsc).theta
    date = acct.date
    curp = acct.curp
    # cutoffDays = .5 * bdays(trade.open.date, trade.targetDate)
    # thresh = max(2, cutoffDays)
    # daysLeft = bdays(date, trade.targetDate)
    # if daysLeft <= thresh
    #     netcXpir = OptionUtil.netExpired(trade.lmsTrack[], curp)
    #     netcCur = bt.getTradeCurClose(trade)
    #     bt.log("checkExit #$(trade.id): $((;xpir=bt.getExpir(trade), cutoffDays, thresh, netcCur, netcXpir))")
    #     if netcCur > netcXpir
    #         return "checkExit $((;cutoffDays, thresh, netcCur, netcXpir))"
    #     end
    # end

    # vix = acct.vix
    # t.rate >= (.8 - vix)/.6 && return "rate >= 1.0"
    # qty = getQuantity(t.lmsc[1])
    # if t.style == Style.call
    #     t.curVal >= qty * t.trade[:curp] * p.Call.takeRat && return "Call take min profit"
    # else
    #     t.curVal >= qty * t.trade[:curp] * p.Put.takeRat && return "Put take min profit"
    # end

    bdaysLeft = bdays(date, trade.targetDate)
    curVal = bt.calcTradeCurVal(trade)
    if bdaysLeft <= params.rollDay && isInTheMoney(trade, curp) # && curVal < 0.0
        tradeRoll = openTradeRoll(acct, params, trade)
        !isnothing(tradeRoll) || error("Could not roll trade $(trade.id)")
        if tradeRoll.multiple * tradeRoll.open.neto <= trade.multiple * (-bt.calcTradeCurClose(trade))
            global keepTradeExit = trade
            global keepTradeRoll = tradeRoll
            error("Roll $(trade.id)->$(tradeRoll.id) didn't cover exit $(tradeRoll.open.neto) <= $(-bt.calcTradeCurClose(trade))")
        end
        return "roll for $(curVal) < 0.0"
    end

    return nothing
end
#endregion

#region Strat
function stratDay(acct)
    # acct.openLong = nothing
    # acct.openShort = nothing
    # acct.margin < getParams().marginMax || ( acct.missed += 1 )
end

function strat(acct)
    p = acct.params
    # balStart = acct.bal
    # bt.checkClose((bt.checkSides, bt.checkThreaten), acct, p)
    # bt.checkClose((bt.checkSides, bt.checkExit), acct, p)
    bt.checkClose((checkExit,), acct, p)

    curp = acct.curp

    # TODO: maybe be more precise about last trade under margin: could check margin during candidate search

    marginAvail = acct.bal * p.marginMaxRat

    if canOpenPut(acct, marginAvail)
        # offMax = curp*p.Put.offMaxRat
        res = back(acct, Side.long; p.Put...) # offMax, profMin=curp * p.Put.profMinRat,
        if !isnothing(res)
            multiple, lms, r = res
            # @assert neto > 0.0 "neto:$(neto), risk:$(risk)"
            # @assert risk / getQuantity(lms[1]) <= offMax
            # lo = acct.extrema.lo
            # mov = getStrike(lms[2]) / lo
            mov = getStrike(lms[2]) / curp
            bt.openTrade(acct, lms, multiple, CZ, "long spread: mov=$(rond(mov)) r:$(string(r))")
        end
    end

    if canOpenCall(acct, marginAvail)
        # offMax = curp*p.Call.offMaxRat
        res = back(acct, Side.short; p.Call...) # offMax, profMin=curp * p.Call.profMinRat,
        if !isnothing(res)
            multiple, lms, r = res
            # @assert neto > 0.0 "neto:$(neto), risk:$(risk)"
            # @assert risk <= offMax
            # hi = acct.extrema.hi
            # mov = getStrike(lms[1]) / hi
            mov = getStrike(lms[1]) / curp
            bt.openTrade(acct, lms, multiple, CZ, "short spread: mov=$(rond(mov))")
        end
    end
    # balEnd = acct.bal
    # if balEnd < balStart
    #     error("bal went down $(balStart) $(balEnd)")
    # end
end

function canOpenCall(acct, mx)
    risk = acct.marginDay.call.risk
    avail = marginPerDay(acct)
    if risk >= avail
        # bt.log("Hit max margin day: call $(risk) / $(avail)")
        return false
    end

    risk = acct.margin.call.risk
    if risk >= mx
        # bt.log("Hit max margin: call $(risk) / $(avail)")
        return false
    end
    return true
end

function canOpenPut(acct, mx)
    risk = acct.marginDay.put.risk
    avail = marginPerDay(acct)
    if risk >= avail
        # bt.log("Hit max margin day: put $(risk) / $(avail)")
        return false
    end

    risk = acct.margin.put.risk
    if risk >= mx
        # bt.log("Hit max margin: put $(risk) / $(avail)")
        return false
    end
    return true
end
#endregion

#region What
makeCtx(side, acct) = (;
    ts = acct.ts,
    curp = F(acct.curp),
    vix = F(acct.vix),
    fromPrice = side == Side.long ? acct.extrema.lo : acct.extrema.hi,
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

    getOqss = xpir -> cu.oqssEntry(chain(xpir).chain, curp, xlegs(xpir))
    xpirs = DateUtil.matchAfter(date, expirs(), ps.xbdays)
    # offMax = curp * ps.offMaxRat
    # profMin = curp * ps.profMinRat
    extrema = HD.extrema(HD.dataDaily(), bdaysBefore(date, p.ExtremaBdays), date - Day(1), curp, curp)
    fromPrice = side == Side.long ? extrema.lo : extrema.hi

    # res = findGC((;ts=mkt.tsMarket, curp, vix, fromPrice), getOqss, xpirs, date, side, vix, ps.probMin)
    res = findGC((;ts=mkt.tsMarket, curp, vix, fromPrice=curp), getOqss, xpirs, date, side, vix, ps.probMin)
    if !isnothing(res)
        lms, r = res
        return (collect(lms), r)
    else
        return
    end
    # return isnothing(lms) ? nothing : collect(lms)
end

function back(acct, side; xbdays, probMin, kws...) # , offMax, profMin, moveMin, moveSdevs, kws...)
    date = acct.date
    xoqss = acct.xoqss
    getOqss = xpir -> xoqss[xpir]
    xpirs = DateUtil.matchAfter(date, acct.xpirs, xbdays)
    res = findGC(makeCtx(side, acct), getOqss, xpirs, date, side, acct.vix, probMin) # , offMax, profMin, acct.extrema, moveMin, moveSdevs)
    if !isnothing(res)
        lms, r = res
        multiple = qtyForMargin(acct, side, resRisk(r), resKel(r))
        if multiple > 0
            return (multiple, lms, r)
        end
        # return res
    end
    return nothing
end

function qtyForMargin(acct, side, risk, kel)
    margin = side == Side.long ? acct.margin.put.risk : acct.margin.call.risk
    marginDay = side == Side.long ? acct.marginDay.put.risk : acct.marginDay.call.risk
    params = acct.params
    bal = acct.bal
    avail = bal * params.marginMaxRat - margin
    availDay = bal * params.marginPerDayRat - marginDay
    return min(params.maxPerTrade, round(Int, min(kel * avail, availDay) / risk, RoundDown))
end
#endregion

#region Local
import ProbKde, ProbUtil
function findGC(ctx, getOqss, xpirs, date, side, vixrat, probMin) # , offMax, profMin, hilo, moveMin, moveSdevs)
    findEntry = side == Side.long ? findSpreadEntryLong : findSpreadEntryShort
    best = nothing
    bestScore = -Inf
    vix = 100 * F(vixrat)

    for xpir in xpirs
        tmult = timult(date, xpir)

        prob = ProbKde.probToClose(F(ctx.fromPrice), vix, ctx.ts, xpir)
        if side == Side.long
            strikeExt, _ = ProbUtil.probFromRight(prob, probMin)
        else
            strikeExt, _ = ProbUtil.probFromLeft(prob, probMin)
        end
        # @show side xpir strikeExt ctx.curp ctx.fromPrice
        calcScore = (lo, sho, innerStrike) -> calcScore3(ctx, prob, side, tmult, innerStrike, lo, sho)
        oqss = getOqss(xpir)

        res = findEntry(oqss, calcScore, strikeExt) # , offMax, profMin
        !isnothing(res) || continue
        score = res[1]
        if score > bestScore
            best = res[2]
            bestScore = score
        end
    end
    return best
end
#endregion

#region findEntry
function findSpreadEntryLong(oqss, calcScore, strikeExt; maxStrikeWidth=40, debug=false) # , offMax, profMinRaw)
    shorts = oqss.put.short
    longs = oqss.put.long
    # @show strikeExt length(shorts) length(longs)
    if isempty(longs)
        bt.log("WARN: oqss.put.long was empty")
        return nothing
    end
    if isempty(shorts)
        bt.log("WARN: oqss.put.short was empty")
        return nothing
    end

    netoMax = 0.0
    netoBest = 0.0
    best = nothing
    bestScore = -Inf
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
            shostr - lostr <= maxStrikeWidth || break

            neto = bt.netOpen(lo, Side.long, sho, Side.short)
            if neto > netoMax
                netoMax = neto
            end
            res = calcScore(lo, sho, F(shostr))
            !isnothing(res) || continue
            score = resScore(res)
            if score > bestScore
                bestScore = score
                best = (res, lo, sho)
                netoBest = neto
            end
        end
    end

    # println((;netoMax, netoBest))
    if isnothing(best)
        return nothing
    else
        return (bestScore, ((LegMetaOpen(best[2], Side.long, 1.0), LegMetaOpen(best[3], Side.short, 1.0)), best[1]))
    end
end

function findSpreadEntryShort(oqss, calcScore, strikeExt; maxStrikeWidth=40, debug=false) # , offMax, profMinRaw
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
    bestScore = -Inf
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
            lostr - shostr <= maxStrikeWidth || break

            res = calcScore(lo, sho, F(shostr))
            !isnothing(res) || continue
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
#endregion

#region Calcs
bt.calcVal(multiple, lo::cu.OptionQuote, sho::cu.OptionQuote, basisAll)::Tuple{Currency,Currency} =
        bt.calcVal(multiple, bt.netOpen(lo, Side.long, sho, Side.short), bt.getSpreadWidth((lo, sho)), basisAll)

getInnerStrike(trade, off::Currency=CZ) = getInnerStrike(bt.getTradeSide(trade), trade.legs, off)
function getInnerStrike(side::Side.T, legs, off::Currency=CZ)
    strike = getStrike(legs[side == Side.long ? 2 : 1])
    return strike + Int(side) * off
end

# netoq2(lo, sho) = netoqL(lo) + netoqS(sho)
# netoqL(lo) = bap(QuoteTypes.newQuote(getQuote(lo), DirSQA(Side.long, 1.0, Action.open)), .1)
# netoqS(sho) = bap(sho, .1)
function isInTheMoney(trade, curp)
    if bt.getTradeSide(trade) == Side.long
        return getStrike(trade.legs[2]) > curp
    else
        return getStrike(trade.legs[1]) < curp
    end
end

marginPerDay(acct) = acct.params.marginPerDayRat * acct.bal
#endregion

end