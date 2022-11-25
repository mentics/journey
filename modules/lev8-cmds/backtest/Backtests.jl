module Backtests
using Dates
using Globals, BaseTypes, SmallTypes, OptionTypes, LegMetaTypes, OptionMetaTypes
using DateUtil, CollUtil, DictUtil
using Expirations, Markets, Snapshots, Chains
import SH
import LegTypes
import SnapUtil
import OptionUtil
import Shorthand

export bt
const bt = @__MODULE__

# TODO: use historical data for VIX to fill in VIX when not in Markets

const TradeType = Dict{Symbol,Any}

function run(f; snapStart=SnapUtil.countSnaps())
    devon()
    LogUtil.resetLog(:backtest)
    global acct = Dict(
        :bal => C(0.0),
        :real => C(0.0),
        :trades => Vector{TradeType}(),
        :poss => Vector{TradeType}(),
        :todayOpens => Vector{TradeType}(),
        :todayCloses => Vector{TradeType}(),
        :riskDays => 0.0
    )
    num = snapStart
    dateStart = SnapUtil.snapDate(num)
    datePrev = Date(0)
    # Stop at 2 because 1 might be currently executing
    for i in num:-1:2
        snap(i)
        name = snap()
        mkt = market()
        ts = mkt.tsMarket
        date = toDateMarket(ts)

        # for trade in acct[:poss]
        #     if getExpir(trade) < date
        #         error("getexpir:$(getExpir(trade)) < date:$(date); ", trade)
        #     end
        #     for lm in trade[:lms]
        #         if SH.getExpiration(lm) < date
        #             error("lmexpir:$(SH.getExpiration(lm)) < date:$(date); ", trade)
        #         end
        #     end
        # end

        if date != datePrev
            # TODO: closing and opening on same day might affect this a little, but I think buying power allows this so maybe it's ok
            acct[:riskDays] += (date - datePrev).value * getRisk(acct[:poss])
            empty!(acct[:todayOpens])
            empty!(acct[:todayCloses])
            datePrev = date
        end

        out("Start $(name) SPY: $(mkt.curp), VIX: $(mkt.vix)")
        acct[:ts] = ts
        acct[:date] = date
        acct[:xpirs] = SnapUtil.snapExpirs(name)
        acct[:greeks] = isempty(acct[:poss]) ? GreeksZero : getGreeks(acct[:poss])
        acct[:greeksExpirs] = isempty(acct[:poss]) ? Dict{Date,GreeksType}() : greeksForExpirs(acct[:poss])
        f(acct)

        tind = 1
        while tind < length(acct[:poss])
            trade = acct[:poss][tind]
            xpir = getExpir(trade)
            if snap() == SnapUtil.lastSnapBefore(xpir + Day(1))
                lmsc = requote(optQuoter, trade[:lms], Action.close)
                closeTrade(acct, trade, lmsc, netClose(trade), "expired")
            else
                tind += 1
            end
        end

        acct[:urpnl] = urpnl(acct[:poss])
        out("End $(name): $(acct[:bal]) rpnl:$(acct[:real]) urpnl:$(acct[:urpnl]) #open:$(length(acct[:poss])) risk:$(getRisk(acct[:poss]))")
        # delta:$(rond(acct[:greeks].delta))

        # for trade in acct[:poss]
        #     if getExpir(trade) <= date
        #         out("WARN: getexpir:$(getExpir(trade)) < date:$(date); ", trade)
        #     end
        #     for lm in trade[:lms]
        #         if SH.getExpiration(lm) <= date
        #             out("WARN: lmexpir:$(SH.getExpiration(lm)) < date:$(date); ", trade)
        #         end
        #     end
        # end
    end
    dateEnd = SnapUtil.snapDate(snap())
    total = acct[:real] + acct[:urpnl]
    # We're going to pretend we closed everything at the last snap, so don't need to add risk.
    # acct[:riskDays] += getRisk(acct[:poss]) # model it for end of last day, so only * 1 day
    rate = calcRate(dateStart, dateEnd, total, acct[:riskDays])
    out("Summary $(dateStart) - $(dateEnd):")
    out("  rpnl = $(acct[:real]), urpnl = $(acct[:urpnl])")
    out("  Total: $(total)")
    out("  rate: $(rond(rate))")
end

# TODO: this is only correct for short put strategy
getRisk(trade) = SH.getStrike(trade[:lms][1])
getRisk(trades::Coll) = sum(getRisk, trades; init=0.0)

urpnl(trades::Coll) = sum(urpnl, trades)
function urpnl(trade)
    qt = quoter(trade[:lms], Action.close)
    netc = usePrice(qt)
    return trade[:neto] + netc
end

OptionMetaTypes.getGreeks(trade::TradeType) = getGreeks(requote(optQuoter, trade[:lms], Action.close))

function greeksForExpirs(trades::Coll)::Dict{Date,GreeksType}
    d = Dict{Date,GreeksType}()
    for trade in trades
        date = trade[:targetDate]
        gks = getGreeks(trade)
        if !haskey(d, date)
            d[date] = gks
        else
            d[date] = greeksAdd(d[date], gks)
        end
    end
    return d
end

import Joe
import Quoting:requote
function strat1(acct)
    # ts = acct[:ts]
    RateMin = 0.5
    MaxDelta = 0.3
    date = acct[:date]
    for trade in acct[:poss]
        dateOpen = toDateMarket(trade[:tsOpen])
        dateOpen < date || continue         # don't close today's entries
        xpir = getExpir(trade)
        daysLeft = bdays(date, xpir)
        daysTotal = bdays(dateOpen, xpir)
        xpirRatio = (1 + daysLeft) / daysTotal
        neto = trade[:neto]
        lmsc = requote(optQuoter, trade[:lms], Action.close)
        # TODO: maybe use lmsc instead of quoting again?
        qt = quoter(trade[:lms], Action.close)
        netc = usePrice(qt)
        curVal = neto + netc
        # theta = getTheta(lmsc)
        # if curVal > 0.0 || daysLeft <= 2 || theta < -0.005
            timt = DateUtil.timult(dateOpen, date)
            mn = min(OptionUtil.legsExtrema(trade[:lms]...)...)
            rate = timt * curVal / (-mn)
            label = nothing
            if rate >= (xpirRatio * RateMin)
                label = "rate: $(rond(rate))"
            # elseif daysLeft <= 2
            #     label = "daysLeft"
            # elseif theta < -0.015
            #     label = "theta: $(rond(theta))"
            # elseif curVal <= -trade[:met].mx
            #     label = "loss < -max profit: $(curVal) <= $(-trade[:met].mx)"
            end
            if !isnothing(label)
                closeTrade(acct, trade, lmsc, netc, label)
            end
        # end
    end

    isLegAllowed = entryFilterOption(acct)
    for xpir in first(acct[:xpirs], 21)
        xpir > bdaysAfter(date, 3) || continue
        posLms = collect(Iterators.filter(x -> SH.getExpiration(x) == xpir, cu.flatmap(x -> x[:lms], acct[:poss])))
        res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms, all=true)
        # res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms)
        !isempty(res) || continue
        deltaAcct = acct[:greeks].delta
        deltaXpir = haskey(acct[:greeksExpirs], xpir) ? acct[:greeksExpirs][xpir].delta : 0.0
        filter!(res) do r
            gks = getGreeks(r.lms)
            deltaAdd = gks.delta
            return r.roiEv >= 0.1 &&
                gks.theta >= 0.0 &&
                (
                    abs(deltaAcct + deltaAdd) < MaxDelta ||
                    abs(deltaAcct + deltaAdd) < abs(deltaAcct)
                ) &&
                (
                    abs(deltaXpir + deltaAdd) < MaxDelta ||
                    abs(deltaXpir + deltaAdd) < abs(deltaXpir)
                )
        end
        !isempty(res) || continue
        # sort!(res; rev=true, by=r -> r.roiEv)
        sort!(res; rev=true, by=r -> r.met.prob)
        r = res[1]
        # if r.roiEv > 0.2
            trade = openTrade(acct, r.lms, netOpen(r.lms), "joe roiEv=$(rond(r.roiEv))")
            trade[:met] = r.met
        # end
    end
end

function tradeInfo(trade)
    xpir = getExpir(trade)
    daysLeft = bdays(date, xpir)
    daysTotal = bdays(dateOpen, xpir)
    xpirRatio = (1 + daysLeft) / daysTotal
    neto = trade[:neto]
    lmsc = requote(optQuoter, trade[:lms], Action.close)
    # TODO: maybe use lmsc instead of quoting again?
    qt = quoter(trade[:lms], Action.close)
    netc = usePrice(qt)
    curVal = neto + netc
    timt = DateUtil.timult(dateOpen, date)
    mn = min(OptionUtil.legsExtrema(trade[:lms]...)...)
    rate = timt * curVal / (-mn)
    return (; xpir, daysLeft, daysTotal, xpirRatio, neto, lmsc, qt, netc, curVal, timt, mn, rate)
end

checkRateRatio(t, p) = t.rate >= (t.xpirRatio * p.RateMin) ? "rate: $(rond(t.rate))" : nothing

function check(fs, args...)
    ss = filter(!isnothing, map(x -> x(args...), fs))
    return isempty(ss) ? nothing : join(ss, " ; ")
end

function checkClose(fs, acct, args...)
    date = acct[:date]
    for trade in acct[:poss]
        dateOpen = toDateMarket(trade[:tsOpen])
        dateOpen < date || continue         # don't close today's entries
        t = tradeInfo(trade)
        label = check(fs, t, args...)
        # # elseif daysLeft <= 2
        # #     label = "daysLeft"
        # # elseif theta < -0.015
        # #     label = "theta: $(rond(theta))"
        # # elseif curVal <= -trade[:met].mx
        # #     label = "loss < -max profit: $(curVal) <= $(-trade[:met].mx)"
        # end
        if !isnothing(label)
            closeTrade(acct, trade, lmsc, netc, label)
        end
    end
end

function stratShortSpread()
    params = (;
        RateMin = 0.5,
        MaxDelta = 0.3,
        XpirBdays = (7,20)
    )
    checkClose((checkRateRatio,), acct, params)

    date = acct[:date]
    xpirs = filter(x -> params.XpirBdays[1] <= bdays(date, x) <= params.XpirBdays[2], acct[:xpirs])
    xprs = whichExpir.(xpirs)

    isLegAllowed = entryFilterOption(acct)
    posLms = cu.flatmap(x -> x[:lms], acct[:poss])

    rs = [(;lms, met=j.calcMet(lms)) for lms in j.getSpreads(expir(4))]


    jorn(xprs, isLegAllowed; condors=false, spreads=true, all=true, nopos, posLms)
    for i in  eachindex(j.ress)
        isassigned(j.ress, i) || continue
        res = j.ress[i]
        xpir = expir(i)
        # posLms = collect(Iterators.filter(x -> SH.getExpiration(x) == xpir, cu.flatmap(x -> x[:lms], acct[:poss])))
        # res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms, all=true)
        # res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms)
        !isempty(res) || continue

        r = res[1]
        # if r.roiEv > 0.2
            trade = openTrade(acct, r.lms, netOpen(r.lms), "joe roiEv=$(rond(r.roiEv))")
            trade[:met] = r.met
        # end
    end
end

using Combos
function stratSellPut(acct)
    MaxOpen = 10
    OpenXpirAfter = Day(13)
    MinMoveRatio = .95
    SafeExpire = 1.0
    RollNear = 1.0
    MinRate = .1
    date = acct[:date]
    curp = market().curp
    for trade in acct[:poss]
        dateOpen = toDateMarket(trade[:tsOpen])
        dateOpen < date || continue         # don't close today's entries
        xpir = getExpir(trade)
        daysLeft = bdays(date, xpir)
        label = nothing
        strike = SH.getStrike(trade[:lms][1])
        if snap() != SnapUtil.lastSnap() && snap() == SnapUtil.lastSnapBefore(xpir + Day(1)) && strike > curp - SafeExpire
            label = "roll last snap before xpir:$(xpir), $(strike) > ($(curp) - $(SafeExpire))"
        elseif (daysLeft <= 6 && strike > curp + RollNear)
            label = "roll near xpir:$(xpir), strike:$(strike) > curp:$(curp) + $(RollNear)"
        end
        if !isnothing(label)
            lmsc = requote(optQuoter, trade[:lms], Action.close)
            # TODO: maybe use lmsc instead of quoting again?
            qt = quoter(trade[:lms], Action.close)
            netc = usePrice(qt)
            closeTrade(acct, trade, lmsc, netc, label)

            entry, oq = c.findRoll("SPY", strike, -netc; silent=true)
            lms = [LegMeta(oq, 1.0, Side.short)]
            openTrade(acct, lms, netOpen(lms), "rolled $(rond(entry.rate))")
        end
    end

    if length(acct[:poss]) < MaxOpen
        entry = findEntry(date + OpenXpirAfter, MinMoveRatio)
        if entry.rate > MinRate
            lms = [LegMeta(entry.oq, 1.0, Side.short)]
            openTrade(acct, lms, netOpen(lms), "short put curp=$(curp) strike=$(entry.strike) rate=$(rond(entry.rate))")
        end
    end
end

function findEntry(dateMin, ratio)
    xpirMin = xp.expirGte(dateMin)
    cands = c.look("SPY"; ratio, dateMin=xpirMin)
    sort!(cands; by=x->x.rate)
    return cands[end]
end

#region Actions
function openTrade(acct, lms, neto, label)
    date = minimum(SH.getExpiration, lms)
    trade = Dict(
        :id => nextTradeId(acct),
        :lms => lms,
        :neto => neto,
        :tsOpen => acct[:ts],
        :labelClose => label,
        :targetDate => date
    )
    push!(acct[:trades], trade)
    push!(acct[:poss], trade)
    push!(acct[:todayOpens], trade)
    acct[:bal] += neto
    gks = getGreeks(lms)
    acct[:greeks] = greeksAdd(acct[:greeks], gks)
    acct[:greeksExpirs][date] = greeksAdd(get!(acct[:greeksExpirs], date, GreeksZero), gks)
    out("Open #$(trade[:id]) $(Shorthand.tosh(lms[1])): '$(label)' neto:$(neto)") # deltaX: $(rond(delta)) -> $(rond(deltaNew)) deltaAcct: $(rond(acct[:delta]))")
    return trade
end
function closeTrade(acct, trade, lms, netc, label)
    # NOTE: snap might have already moved when called here, so don't use anything except acct
    trade[:lmsc] = lms
    trade[:netc] = netc
    trade[:tsClosed] = acct[:ts]
    trade[:labelClose] = label
    push!(acct[:todayCloses], trade)
    cu.del!(x -> x[:id] == trade[:id], acct[:poss])
    acct[:bal] += netc
    acct[:real] += trade[:neto] + netc
    trade[:labelClose] = label
    out("Close #$(trade[:id]) $(Shorthand.tosh(lms[1])): '$(label)' neto:$(trade[:neto]) netc:$(netc) pnl=$(trade[:neto] + netc) ; $(openDur(trade)) days open, $(bdaysLeft(acct[:date], trade)) days left")
    return
end
nextTradeId(acct) = length(acct[:trades]) + 1
#endregion

#region Utils
rond(x::Real) = round(x; digits=4)
usePrice(q) = SH.bap(q, .2) # SH.getBid(q)
netOpen(lms) = sum(usePrice, lms)
# priceOpen(trade) = usePrice(quoter(trade[:lms], Action.open))
netClose(trade) = usePrice(quoter(trade[:lms], Action.close))
getExpir(trade) = trade[:targetDate] # minimum(SH.getExpiration, lms)
openDur(trade) = bdays(toDateMarket(trade[:tsOpen]), toDateMarket(trade[:tsClosed])) + 1
bdaysLeft(from, trade) = bdays(from, getExpir(trade))

function entryFilterOption(acct)
    d = entryFilterLookup(acct)
    return function(opt, side)
        status = get(d, SH.getOption(opt), 0)
        return !(status != 0 && (status == 2 || status != Int(side)))
    end
end

function entryFilterLookup(acct)
    # can't go opposite current poss and today's opens and closes
    global legsPos = cu.mapflatmap(SH.getLeg, x -> x[:lms], acct[:poss])
    global legsOpens = cu.mapflatmap(SH.getLeg, x -> x[:lms], acct[:todayOpens])
    global legsCloses = cu.mapflatmap(x -> LegTypes.switchSide(SH.getLeg(x)), x -> x[:lms], acct[:todayCloses])
    global d = Dict{Option,Int}()
    for leg in flat(legsPos, legsOpens, legsCloses)
        opt = SH.getOption(leg)
        side = Int(SH.getSide(leg))
        status = get(d, opt, 0)
        status2 = status == 0 ? side : (status == side ? status : 2)
        # if status2 == 2
        #     error("status == 2 ", leg, ' ', status, ' ', d[opt])
        #     # @show "How did we get status 2?" leg opt d[opt]
        # end
        d[opt] = status2
    end
    return d
end
#endregion

# function findCurp(chs)
#     chs = filter(isPut, chs)
#     chs = sort!(chs, by=x->abs(x.meta.delta + 0.5))
#     return SH.getStrike(chs[1])
# end

using LogUtil
out(args...) = LogUtil.logit(:backtest, args...)

end