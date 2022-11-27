module Backtests
using Dates
using Globals, BaseTypes, SmallTypes, OptionTypes, LegMetaTypes, OptionMetaTypes
using DateUtil, CollUtil, DictUtil
using Expirations, Markets, Snapshots, Chains
using SH
import LegTypes
import SnapUtil
import OptionUtil
import Shorthand
import Quoting:requote
using Joe

export bt
const bt = @__MODULE__

const TradeType = Dict{Symbol,Any}

#region CurStrat
checkRateRatio(acct, t, p) = t.rate >= (t.xpirRatio * p.RateMin) ? "rate: $(rond(t.rate))" : nothing
checkProfit(acct, t, p) = t.curVal >= p.MinTakeProfit ? "take min profit" : nothing
function checkThreaten(acct, t, p)
    s = getStrike(t.lmsc[1])
    threat = (s - acct[:curp]) / s
    return threat <= p.ThreatenPercent ? "threat: $(threat)" : nothing
end

function checkSides(acct, t, p)
    if getStyle(t.lmsc[1]) == Style.call
        t.curVal >= p.Call.take && return "Call take min profit"
    else
        t.curVal >= p.Put.take && return "Put take min profit"
    end
    return nothing
end

function stratGiantCondor(acct)
    p = (;
        MaxOpen = 100,
        XpirBdays = 40,
        Put = (;rat=0.8, off=30.0, prof=0.8, take=0.4),
        Call = (;rat=1.12, off=20.0, prof=0.7, take=0.2),
    )
    # checkClose((checkRateRatio,), acct, params)
    checkClose((checkSides,), acct, p)

    date = acct[:date]
    # xpirs = filter(x -> params.XpirBdays[1] <= bdays(date, x) <= params.XpirBdays[2], acct[:xpirs])
    # xprs = xp.whichExpir.(xpirs)
    curp = acct[:curp]

    # isLegAllowed = entryFilterOption(acct)
    # posLms = collect(cu.flatmap(x -> x[:lms], acct[:poss]))

    # rs = [(;lms, met=j.calcMet(lms)) for lms in j.getSpreads(expir(4))]

    # for i in xprs
        length(acct[:poss]) < p.MaxOpen || ( println("Hit max open") ; return )
        # xpir = expir(i)
        xpir = xp.expirGte(bdaysAfter(date, p.XpirBdays))

        lms = findLongSpreadEntry(xpir, curp, p.Put)
        !(isnothing(lms) || bap(lms) < p.Put.prof) || return
        neto = netOpen(lms)
        @assert neto > 0.0
        openTrade(acct, lms, neto, "long spread greeks: $(getGreeks(lms))", abs(getStrike(lms[1]) - getStrike(lms[2])) - neto)

        lms = findShortSpreadEntry(xpir, curp, p.Call)
        !(isnothing(lms) || bap(lms) < p.Call.prof) || return
        neto = netOpen(lms)
        @assert neto > 0.0
        openTrade(acct, lms, neto, "short spread greeks: $(getGreeks(lms))", abs(getStrike(lms[1]) - getStrike(lms[2])) - neto)
    # end
end

function findLongSpreadEntry(xpir, curp, p)
    oqss = CH.getOqss(xpir, curp)
    # oql = find(oq -> getStrike(oq) > 0.8 * curp, oqss.put.long)
    # oql = find(oq -> bap(oq) >= .04, oqss.put.long)
    oqs = find(oq -> getStrike(oq) >= p.rat * curp, oqss.put.short)
    !isnothing(oqs) || ( println("Could not find put oqs") ; return nothing )
    soqs = getStrike(oqs)
    soql = soqs - p.off
    oql = find(oq -> getStrike(oq) >= soql - 0.25, oqss.put.long)
    !isnothing(oql) || ( println("Could not find put offset oql $(soql) from oqs $(soqs)") ; return nothing )
    @assert getStrike(oql) < getStrike(oqs)
    # TODO: It appears there may be cases where theta > delta, so might be worth searching for
    return [to(LegMeta, oql, Side.long), to(LegMeta, oqs, Side.short)]
end

# TODO: have use the recent high (period = xpir days) and go rat above that
function findShortSpreadEntry(xpir, curp, p)
    oqss = CH.getOqss(xpir, curp)
    oqs = find(oq -> getStrike(oq) <= p.rat * curp, Iterators.reverse(oqss.call.short))
    !isnothing(oqs) || ( println("Could not find call oqs") ; return nothing )
    soqs = getStrike(oqs)
    soql = soqs + p.off
    oql = find(oq -> getStrike(oq) <= soql + 0.25, Iterators.reverse(oqss.call.long))
    !isnothing(oql) || ( println("Could not find call offset oql $(soql) from oqs $(soqs)") ; return nothing )
    @assert getStrike(oql) > getStrike(oqs)
    # TODO: It appears there may be cases where theta > delta, so might be worth searching for
    return [to(LegMeta, oqs, Side.short), to(LegMeta, oql, Side.long)]
end

# TODO: try adjusting strat dep on VIX
function stratSimpleCondor(acct)
    params = (;
        MaxOpen = 1000,
        # ThreatenPercent = -0.01,
        MinProfit = 0.04,
        MaxWidth = 5.0,
        XpirBdays = (12,40),
        RateMin = 0.1,
        MinTakeProfit = 0.01,
    )
    # checkClose((checkRateRatio,), acct, params)
    checkClose((checkProfit,), acct, params)

    date = acct[:date]
    xpirs = filter(x -> params.XpirBdays[1] <= bdays(date, x) <= params.XpirBdays[2], acct[:xpirs])
    xprs = xp.whichExpir.(xpirs)
    curp = acct[:curp]

    isLegAllowed = entryFilterOption(acct)
    posLms = collect(cu.flatmap(x -> x[:lms], acct[:poss]))

    # rs = [(;lms, met=j.calcMet(lms)) for lms in j.getSpreads(expir(4))]

    for i in xprs
        length(acct[:poss]) < params.MaxOpen || ( println("Hit max open") ; break )
        xpir = expir(i)
        filtOq = oq -> abs(getStrike(oq) / curp - 1.0) < 0.015
        res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms, all=true, condors=true, spreads=false, filtOq)
        filter!(res) do r
            return getSide(r.lms[1]) == getSide(r.lms[end]) == Side.short &&
                r.met.mx >= params.MinProfit &&
                getStrike(r.lms[4]) - getStrike(r.lms[1]) <= params.MaxWidth &&
                # getStrike(r.lms[2]) < curp + 2 &&
                # getStrike(r.lms[3]) > curp - 2 &&
                getStrike(r.lms[2]) - getStrike(r.lms[1]) == getStrike(r.lms[4]) - getStrike(r.lms[3])
        end
        sort!(res; rev=true, by=x -> x.met.mx / (-x.met.mn))
        # res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms)
        !isempty(res) || continue

        r = res[1]
        openTrade(acct, r.lms, netOpen(r.lms), "j", -r.met.mn)
    end
end
#endregion

#region Process
function run(f; snapStart=65)
    devon()
    LogUtil.resetLog(:backtest)
    global acct = Dict(
        :bal => C(0.0),
        :real => C(0.0),
        :trades => Vector{TradeType}(),
        :poss => Vector{TradeType}(),
        :todayOpens => Vector{TradeType}(),
        :todayCloses => Vector{TradeType}(),
        :riskDays => 0.0,
        :lastTradeId => 0
    )
    dateStart = SnapUtil.snapDate(snapStart)
    datePrev = Date(0)
    # Stop at count-1 because 1 might be currently executing
    for i in snapStart:(SnapUtil.countSnaps()-1)
        snap(i)
        name = snap()
        mkt = market()
        ts = mkt.tsMarket
        date = toDateMarket(ts)
        days = bdays(dateStart, date)

        # for trade in acct[:poss]
        #     if getExpir(trade) < date
        #         error("getexpir:$(getExpir(trade)) < date:$(date); ", trade)
        #     end
        #     for lm in trade[:lms]
        #         if getExpiration(lm) < date
        #             error("lmexpir:$(getExpiration(lm)) < date:$(date); ", trade)
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
        acct[:curp] = mkt.curp
        acct[:xpirs] = SnapUtil.snapExpirs(name)
        acct[:greeks] = isempty(acct[:poss]) ? GreeksZero : getGreeks(acct[:poss])
        acct[:greeksExpirs] = isempty(acct[:poss]) ? Dict{Date,GreeksType}() : greeksForExpirs(acct[:poss])
        f(acct)

        tind = 1
        while tind <= length(acct[:poss])
            trade = acct[:poss][tind]
            xpir = getExpir(trade)
            if snap() == SnapUtil.lastSnapBefore(xpir + Day(1))
                lmsc = requote(optQuoter, trade[:lms], Action.close)
                netc = OptionUtil.netExpired(lmsc, mkt.curp)
                if abs(mkt.curp - getStrike(lmsc[1])) < 5.0 || abs(mkt.curp - getStrike(lmsc[end])) < 5.0
                    netc -= 0.02
                end
                closeTrade(acct, trade, lmsc, netc, "expired $(netClose(trade))")
            else
                tind += 1
            end
        end

        acct[:urpnl] = urpnl(acct[:poss])
        # total = acct[:real] + acct[:urpnl]
        rate = calcRate(dateStart, date, acct[:real], acct[:riskDays] / days)
        out("End $(name): $(rond(acct[:bal])) rpnl:$(rond(acct[:real])) urpnl:$(rond(acct[:urpnl])) #open:$(length(acct[:poss])) risk:$(acct[:riskDays] / days) rate:$(rate)")
        # delta:$(rond(acct[:greeks].delta))

        # for trade in acct[:poss]
        #     if getExpir(trade) <= date
        #         out("WARN: getexpir:$(getExpir(trade)) < date:$(date); ", trade)
        #     end
        #     for lm in trade[:lms]
        #         if getExpiration(lm) <= date
        #             out("WARN: lmexpir:$(getExpiration(lm)) < date:$(date); ", trade)
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
    out("  risk-days: $(acct[:riskDays])")
    out("  rate: $(rond(rate))")
end

nextTradeId(acct) = acct[:lastTradeId] += 1

function check(fs, args...)
    ss = filter(!isnothing, map(x -> x(args...), fs))
    return isempty(ss) ? nothing : join(ss, " ; ")
end

function checkClose(fs, acct, args...)
    date = acct[:date]
    for trade in acct[:poss]
        dateOpen = toDateMarket(trade[:tsOpen])
        dateOpen < date || continue         # don't close today's entries
        t = tradeInfo(trade, date)
        label = check(fs, acct, t, args...)
        # # elseif daysLeft <= 2
        # #     label = "daysLeft"
        # # elseif theta < -0.015
        # #     label = "theta: $(rond(theta))"
        # # elseif curVal <= -trade[:met].mx
        # #     label = "loss < -max profit: $(curVal) <= $(-trade[:met].mx)"
        # end
        if !isnothing(label)
            closeTrade(acct, trade, t.lmsc, t.netc, label)
        end
    end
end
#endregion

#region Actions
function openTrade(acct, lms, neto, label, risk)
    date = minimum(getExpiration, lms)
    trade = Dict(
        :id => nextTradeId(acct),
        :lms => lms,
        :neto => neto,
        :tsOpen => acct[:ts],
        :labelClose => label,
        :targetDate => date,
        :risk => risk
    )
    push!(acct[:trades], trade)
    push!(acct[:poss], trade)
    push!(acct[:todayOpens], trade)
    acct[:bal] += neto
    gks = getGreeks(lms)
    acct[:greeks] = greeksAdd(acct[:greeks], gks)
    acct[:greeksExpirs][date] = greeksAdd(get!(acct[:greeksExpirs], date, GreeksZero), gks)
    out("Open #$(trade[:id]) $(Shorthand.tosh(lms, acct[:xpirs])): '$(label)' neto:$(neto)") # deltaX: $(rond(delta)) -> $(rond(deltaNew)) deltaAcct: $(rond(acct[:delta]))")
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
    out("Close #$(trade[:id]) $(Shorthand.tosh(lms[1])): '$(label)' neto:$(rond(trade[:neto])) netc:$(netc) pnl=$(rond(trade[:neto] + netc)) ; $(openDur(trade)) days open, $(bdaysLeft(acct[:date], trade)) days left")
    return
end
#endregion

#region GetData
getRisk(trade) = trade[:risk] # getStrike(trade[:lms][1])
getRisk(trades::Coll) = sum(getRisk, trades; init=0.0)

urpnl(trades::Coll) = sum(urpnl, trades; init=0.0)
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

function tradeInfo(trade, date)
    xpir = getExpir(trade)
    daysLeft = bdays(date, xpir)
    dateOpen = toDateMarket(trade[:tsOpen])
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
#endregion

#region Utils
rond(x::Real) = round(x; digits=4)
usePrice(q) = bap(q, .2) # getBid(q)
netOpen(lms) = sum(usePrice, lms)
# priceOpen(trade) = usePrice(quoter(trade[:lms], Action.open))
netClose(trade) = usePrice(quoter(trade[:lms], Action.close))
getExpir(trade) = trade[:targetDate] # minimum(getExpiration, lms)
openDur(trade) = bdays(toDateMarket(trade[:tsOpen]), toDateMarket(trade[:tsClosed])) + 1
bdaysLeft(from, trade) = bdays(from, getExpir(trade))
aboveRat(s, curp) = (s - curp) / s

function entryFilterOption(acct)
    d = entryFilterLookup(acct)
    return function(opt, side)
        status = get(d, getOption(opt), 0)
        return !(status != 0 && (status == 2 || status != Int(side)))
    end
end

function entryFilterLookup(acct)
    # can't go opposite current poss and today's opens and closes
    global legsPos = cu.mapflatmap(getLeg, x -> x[:lms], acct[:poss])
    global legsOpens = cu.mapflatmap(getLeg, x -> x[:lms], acct[:todayOpens])
    global legsCloses = cu.mapflatmap(x -> LegTypes.switchSide(getLeg(x)), x -> x[:lms], acct[:todayCloses])
    global d = Dict{Option,Int}()
    for leg in flat(legsPos, legsOpens, legsCloses)
        opt = getOption(leg)
        side = Int(getSide(leg))
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

using LogUtil
out(args...) = LogUtil.logit(:backtest, args...)

function track(lms, start)
    num = start
    while SnapUtil.snapDate(num) <= getExpiration(lms)
        snap(num)
        print(market().curp, ": ")
        println(quoter(lms, Action.close))
        num += 1
    end
end

# function findCurp(chs)
#     chs = filter(isPut, chs)
#     chs = sort!(chs, by=x->abs(x.meta.delta + 0.5))
#     return getStrike(chs[1])
# end
#endregion

#region PrevStrats
# function strat1(acct)
#     # ts = acct[:ts]
#     RateMin = 0.5
#     MaxDelta = 0.3
#     date = acct[:date]
#     for trade in acct[:poss]
#         dateOpen = toDateMarket(trade[:tsOpen])
#         dateOpen < date || continue         # don't close today's entries
#         xpir = getExpir(trade)
#         daysLeft = bdays(date, xpir)
#         daysTotal = bdays(dateOpen, xpir)
#         xpirRatio = (1 + daysLeft) / daysTotal
#         neto = trade[:neto]
#         lmsc = requote(optQuoter, trade[:lms], Action.close)
#         # TODO: maybe use lmsc instead of quoting again?
#         qt = quoter(trade[:lms], Action.close)
#         netc = usePrice(qt)
#         curVal = neto + netc
#         # theta = getTheta(lmsc)
#         # if curVal > 0.0 || daysLeft <= 2 || theta < -0.005
#             timt = DateUtil.timult(dateOpen, date)
#             mn = min(OptionUtil.legsExtrema(trade[:lms]...)...)
#             rate = timt * curVal / (-mn)
#             label = nothing
#             if rate >= (xpirRatio * RateMin)
#                 label = "rate: $(rond(rate))"
#             # elseif daysLeft <= 2
#             #     label = "daysLeft"
#             # elseif theta < -0.015
#             #     label = "theta: $(rond(theta))"
#             # elseif curVal <= -trade[:met].mx
#             #     label = "loss < -max profit: $(curVal) <= $(-trade[:met].mx)"
#             end
#             if !isnothing(label)
#                 closeTrade(acct, trade, lmsc, netc, label)
#             end
#         # end
#     end

#     isLegAllowed = entryFilterOption(acct)
#     for xpir in first(acct[:xpirs], 21)
#         xpir > bdaysAfter(date, 3) || continue
#         posLms = collect(Iterators.filter(x -> getExpiration(x) == xpir, cu.flatmap(x -> x[:lms], acct[:poss])))
#         res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms, all=true)
#         # res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms)
#         !isempty(res) || continue
#         deltaAcct = acct[:greeks].delta
#         deltaXpir = haskey(acct[:greeksExpirs], xpir) ? acct[:greeksExpirs][xpir].delta : 0.0
#         filter!(res) do r
#             gks = getGreeks(r.lms)
#             deltaAdd = gks.delta
#             return r.roiEv >= 0.1 &&
#                 gks.theta >= 0.0 &&
#                 (
#                     abs(deltaAcct + deltaAdd) < MaxDelta ||
#                     abs(deltaAcct + deltaAdd) < abs(deltaAcct)
#                 ) &&
#                 (
#                     abs(deltaXpir + deltaAdd) < MaxDelta ||
#                     abs(deltaXpir + deltaAdd) < abs(deltaXpir)
#                 )
#         end
#         !isempty(res) || continue
#         # sort!(res; rev=true, by=r -> r.roiEv)
#         sort!(res; rev=true, by=r -> r.met.prob)
#         r = res[1]
#         # if r.roiEv > 0.2
#             trade = openTrade(acct, r.lms, netOpen(r.lms), "joe roiEv=$(rond(r.roiEv))", -r.met.mn)
#             trade[:met] = r.met
#         # end
#     end
# end

# using Combos
# function stratSellPut(acct)
#     MaxOpen = 10
#     OpenXpirAfter = Day(13)
#     MinMoveRatio = .95
#     SafeExpire = 1.0
#     RollNear = 1.0
#     MinRate = .1
#     date = acct[:date]
#     curp = market().curp
#     for trade in acct[:poss]
#         dateOpen = toDateMarket(trade[:tsOpen])
#         dateOpen < date || continue         # don't close today's entries
#         xpir = getExpir(trade)
#         daysLeft = bdays(date, xpir)
#         label = nothing
#         strike = getStrike(trade[:lms][1])
#         if snap() != SnapUtil.lastSnap() && snap() == SnapUtil.lastSnapBefore(xpir + Day(1)) && strike > curp - SafeExpire
#             label = "roll last snap before xpir:$(xpir), $(strike) > ($(curp) - $(SafeExpire))"
#         elseif (daysLeft <= 6 && strike > curp + RollNear)
#             label = "roll near xpir:$(xpir), strike:$(strike) > curp:$(curp) + $(RollNear)"
#         end
#         if !isnothing(label)
#             lmsc = requote(optQuoter, trade[:lms], Action.close)
#             # TODO: maybe use lmsc instead of quoting again?
#             qt = quoter(trade[:lms], Action.close)
#             netc = usePrice(qt)
#             closeTrade(acct, trade, lmsc, netc, label)

#             entry, oq = c.findRoll("SPY", strike, -netc; silent=true)
#             lms = [LegMeta(oq, 1.0, Side.short)]
#             openTrade(acct, lms, netOpen(lms), "rolled $(rond(entry.rate))", getStrike(lms[1]))
#         end
#     end

#     if length(acct[:poss]) < MaxOpen
#         entry = findEntry(date + OpenXpirAfter, MinMoveRatio)
#         if entry.rate > MinRate
#             lms = [LegMeta(entry.oq, 1.0, Side.short)]
#             openTrade(acct, lms, netOpen(lms), "short put curp=$(curp) strike=$(entry.strike) rate=$(rond(entry.rate))", getStrike(lms[1]))
#         end
#     end
# end

# function findEntry(dateMin, ratio)
#     xpirMin = xp.expirGte(dateMin)
#     cands = c.look("SPY"; ratio, dateMin=xpirMin)
#     sort!(cands; by=x->x.rate)
#     return cands[end]
# end

# function stratShortSpread(acct)
#     params = (;
#         MaxOpen = 100,
#         RateMin = 0.5,
#         ThreatenPercent = -0.01,
#         MinAbove = 0.05,
#         XpirBdays = (7,20)
#     )
#     checkClose((checkRateRatio,checkThreaten), acct, params)

#     date = acct[:date]
#     xpirs = filter(x -> params.XpirBdays[1] <= bdays(date, x) <= params.XpirBdays[2], acct[:xpirs])
#     xprs = xp.whichExpir.(xpirs)
#     curp = acct[:curp]

#     isLegAllowed = entryFilterOption(acct)
#     posLms = collect(cu.flatmap(x -> x[:lms], acct[:poss]))

#     # rs = [(;lms, met=j.calcMet(lms)) for lms in j.getSpreads(expir(4))]

#     # jorn(xprs; condors=false, spreads=true, all=true, nopos=true, posLms)
#     # res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms, all=true)
#     # for i in eachindex(j.ress)
#     for i in xprs
#         length(acct[:poss]) < params.MaxOpen || ( println("Hit max open") ; break )
#         # isassigned(j.ress, i) || continue
#         # j.filt(x -> x.delta < 0.0)
#         # j.sor(x -> x.met.prob)
#         # res = j.ress[i]
#         xpir = expir(i)
#         # posLms = collect(Iterators.filter(x -> getExpiration(x) == xpir, cu.flatmap(x -> x[:lms], acct[:poss])))
#         res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms, all=true, condors=false, spreads=true)
#         filter!(res) do r
#             return getSide(r.lms[1]) == Side.short && aboveRat(getStrike(r.lms[1]), curp) > params.MinAbove
#         end
#         sort!(res; rev=true, by=x -> x.met.evr)
#         # res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms)
#         !isempty(res) || continue

#         r = res[1]
#         trade = openTrade(acct, r.lms, netOpen(r.lms), "joe above=$(rond(aboveRat(getStrike(r.lms[1]), curp)))", -r.met.mn)
#     end
# end
#endregion

end