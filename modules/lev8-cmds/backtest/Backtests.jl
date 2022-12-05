module Backtests
using Dates
import StatsBase
using SH, Globals, BaseTypes, SmallTypes, OptionTypes, LegTypes, LegMetaTypes, OptionMetaTypes
using DateUtil, CollUtil, DictUtil, ChainUtil
using Pricing, Between
import OptionUtil
import Shorthand
import HistSpy as hspy
using Calendars

#= Explore pricing
lmss = bt.findLongSpreadEntry(expir(16), market().curp, (;rat=.8, off=30.0))
lmss2 = bt.findLongSpreadEntry(expir(16), market().curp, (;rat=1.0, off=5.0))
priceOption(market().curp, texPY(market().tsMarket, lmss[2]), lmss[2], .35)
priceOption(market().curp, texPY(market().tsMarket, lmss2[1]), lmss2[1], .276)
=#

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

function lyzeRisk()
    closed = filter(t -> haskey(t, :close), acct[:trades])
    risks = map(getTradeRate, closed)
end

getTradeGreeks(trade) = getGreeks(trade[:lmsTrack])
getTradePnl(trade) = trade[:open][:neto] + trade[:close][:netc]
function getTradeRate(trade)
    pnl = getTradePnl(trade)
    # @show toDateMarket(trade[:open][:ts]), toDateMarket(trade[:close][:ts]), pnl, trade[:risk]
    return calcRate(toDateMarket(trade[:open][:ts]), toDateMarket(trade[:close][:ts]), pnl, trade[:risk])
end
getStyl(trade::Dict{Symbol,Any}) = getStyle(trade[:legs][1])
getTradeRat(trade) = getStyl(trade) == Style.call ? getStrike(trade[:legs][1]) / trade[:curp] : getStrike(trade[:legs][2]) / trade[:curp]
getTradeRisk(trade) = trade[:risk]
getTradeExpDur(trade) = bdays(Date(trade[:open][:ts]), Date(trade[:targetDate]))
#endregion

#region Process
function run(f; maxSeconds=60)
    tstart = time()
    hspy.db()
    LogUtil.resetLog(:backtest)
    global acct = Dict(
        :bal => C(0.0),
        :real => C(0.0),
        :trades => Vector{TradeType}(),
        :poss => Vector{TradeType}(),
        :todayOpens => Vector{TradeType}(),
        :todayCloses => Vector{TradeType}(),
        # :riskDays => 0.0,
        :lastTradeId => 0,
        :rates => Vector{Float64}()
    )
    tss = filter!(sort(hspy.getTss())) do x
        # Don't run on first and last (other if inside loop) ts of the day
        return DateTime(2019,12,31) < x < DateTime(2021,1,1) &&
            Time(x) != Time(getMarketOpen(Date(x))) + Second(10)
    end
    dateStart = toDateMarket(tss[1])
    datePrev = Date(0)
    for ts in tss
        date = toDateMarket(ts)
        # days = bdays(dateStart, date)

        if date != datePrev
            # TODO: closing and opening on same day might affect this a little, but I think buying power allows this so maybe it's ok
            # acct[:riskDays] += (date - datePrev).value * getRisk(acct[:poss])
            empty!(acct[:todayOpens])
            empty!(acct[:todayCloses])
            datePrev = date
        end

        curp = hspy.getUnder(ts)
        out("Start $(formatLocal(ts)) SPY: $(curp)") # TODO: , VIX: $(mkt.vix)")
        acct[:ts] = ts
        acct[:date] = date
        acct[:curp] = curp
        acct[:xpirs] = hspy.getExpirs(ts)
        xoqss = hspy.getXoqss(ts, curp, collect(flatmap(x -> x[:legs], acct[:poss])))
        # TODO: opt
        unxoqss = hspy.getXoqss(ts, curp)
        acct[:xoqss] = xoqss
        lup = lookup(unxoqss)
        acct[:lup] = lup
        updatePossLmsClose(lup, acct[:poss])
        # acct[:greeks] = calcGreeks(acct[:poss]) # isempty(acct[:poss]) ? GreeksZero : getGreeks(acct[:poss])
        # acct[:greeksExpirs] = isempty(acct[:poss]) ? Dict{Date,GreeksType}() : greeksForExpirs(lup, acct[:poss])

        # Run strat for this time

        if ts == getMarketClose(date)
            tind = 1
            while tind <= length(acct[:poss])
                trade = acct[:poss][tind]
                xpir = getExpir(trade)
                if ts == getMarketClose(xpir)
                    # lmsc = tosLLMC(trade[:lmsTrack], lup)
                    lmsc = trade[:lmsTrack]
                    netc = OptionUtil.netExpired(lmsc, curp)
                    if abs(curp - getStrike(lmsc[1])) < 5.0 || abs(curp - getStrike(lmsc[end])) < 5.0
                        netc -= 0.02 # adjust to pay to close and not just let expire
                    end
                    closeTrade(acct, trade, lmsc, netc, "expired $(getQuote(trade[:lmsTrack]))")
                else
                    tind += 1
                end
            end
        else
            f(acct)
        end

        acct[:urpnl] = urpnl(acct[:poss])
        # rate = calcRate(dateStart, date, acct[:real], acct[:riskDays] / days)
        # risk:$(acct[:riskDays] / days)
        rate = StatsBase.mean(acct[:rates])
        out("End $(formatLocal(ts)): $(rond(acct[:bal])) rpnl:$(rond(acct[:real])) urpnl:$(rond(acct[:urpnl])) #open:$(length(acct[:poss])) rateAvg:$(rond(rate))")

        if time() > tstart + maxSeconds
            break
        end
    end
    total = acct[:real] + acct[:urpnl]
    # days = bdays(dateStart, toDateMarket(to))
    dateEnd = acct[:date]
    # We're going to pretend we closed everything at the last time, so don't need to add risk.
    # acct[:riskDays] += getRisk(acct[:poss]) # model it for end of last day, so only * 1 day
    # rate = calcRate(dateStart, dateEnd, total, acct[:riskDays] / days)
    rate = StatsBase.mean(acct[:rates])
    out("Summary $(dateStart) - $(dateEnd):")
    out("  rpnl = $(acct[:real]), urpnl = $(acct[:urpnl])")
    out("  Total: $(total)")
    # out("  risk-days: $(acct[:riskDays])")
    out("  rate: $(rond(rate))")
end

function updatePossLmsClose(lup, trades)
    i = 1
    for trade in trades
        try
            trade[:lmsTrack] = tosLMC(trade[:legs], lup)
            # trade[:lmsTrack] = map(leg -> LegMetaClose(leg, lup(getOption(leg)), trade[:legs]))
        catch e
            println("Trade $(i): ", trade)
            rethrow(e)
        end
        i += 1
    end
end

nextTradeId(acct) = acct[:lastTradeId] += 1

function check(fs, args...)
    ss = filter(!isnothing, map(x -> x(args...), fs))
    return isempty(ss) ? nothing : join(ss, " ; ")
end

function checkClose(fs, acct, args...)
    date = acct[:date]
    for trade in acct[:poss]
        dateOpen = toDateMarket(trade[:open].ts)
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
function openTrade(acct, lms::Coll{LegMetaOpen}, neto, label, risk)
    lup = acct[:lup]
    lmsTrack = tosLLMC(lms, lup)
    targetDate = minimum(getExpiration, lms)
    trade = Dict(
        :id => nextTradeId(acct),
        :legs => map(getLeg, lms),
        :open => (;label, ts=acct[:ts], neto, quotes=map(getQuote, lms), metas=map(getOptionMeta, lms)),
        :targetDate => targetDate,
        :risk => risk,
        :lmsTrack => lmsTrack,
        :curp => acct[:curp]
    )
    trade[:rat] = getTradeRat(trade)
    push!(acct[:trades], trade)
    push!(acct[:poss], trade)
    push!(acct[:todayOpens], trade)
    acct[:bal] += neto
    # acct[:greeks] = updateGreeks(acct, targetDate, getGreeks(lms))
    out("Open #$(trade[:id]) $(Shorthand.tosh(lms, acct[:xpirs])): '$(label)' neto:$(neto)") # deltaX: $(rond(delta)) -> $(rond(deltaNew)) deltaAcct: $(rond(acct[:delta]))")
    return trade
end
function closeTrade(acct, trade, lms::Coll{LegMetaClose}, netc, label)
    trade[:close] = (;label, ts=acct[:ts], netc, quotes=map(getQuote, lms), metas=map(getOptionMeta, lms))
    push!(acct[:todayCloses], trade)
    cu.del!(x -> x[:id] == trade[:id], acct[:poss])
    acct[:bal] += netc
    neto = trade[:open].neto
    pnl = neto + netc
    # trade[:pnl] = pnl
    acct[:real] += pnl
    push!(acct[:rates], getTradeRate(trade))
    out("Close #$(trade[:id]) $(Shorthand.tosh(lms[1])): '$(label)' neto:$(rond(neto)) netc:$(netc) pnl=$(rond(pnl)) rate=$(getTradeRate(trade)) ; $(openDur(trade)) days open, $(bdaysLeft(acct[:date], trade)) days left")
    return
end
#endregion

#region GetData
getRisk(trade) = trade[:risk] # getStrike(trade[:lms][1])
getRisk(trades::Coll) = sum(getRisk, trades; init=0.0)

urpnl(trades::Coll) = sum(x -> urpnl(x), trades; init=0.0)
function urpnl(trade)
    qt = trade[:lmsTrack]
    netc = bap(qt, .1)
    return trade[:open].neto + netc
end

function lookup(xoqss)
    function lup(exp::Date, style::Style.T, strike::Currency) # ::Union{Nothing,OptionQuote}
        try
            res = find(x -> getStrike(x) == strike, getfield(xoqss[exp], Symbol(style)).long)
            !isnothing(res) || println("WARN: Backtest could not quote $(exp), $(style), $(strike)")
            return res
        catch e
            @show exp style strike
            rethrow(e)
        end
    end
    function lup(o::Option)
        try
            exp = getExpiration(o)
            style = getStyle(o)
            strike = getStrike(o)
            res = find(x -> getStrike(x) == strike, getfield(xoqss[exp], Symbol(style)).long)
            !isnothing(res) || println("WARN: Backtest could not quote $(exp), $(style), $(strike)")
            return res
        catch e
            @show exp style strike
            rethrow(e)
        end
    end
    return lup
end

function calcGreeks(trades)
    d = Dict{Date,Greeks}()
    total = GreeksZero
    for trade in trades
        date = getExpir(trade) # [:targetDate]
        gks = getGreeks(trade[:lmsTrack])
        if !haskey(d, date)
            d[date] = gks
        else
            d[date] = addGreeks(d[date], gks)
        end
        total = addGreeks(total, gks)
    end
    return (;total, xpirs=d)
end

# function updateGreeks(acct, targetDate, gks)
#     total, xpirs = acct[:greeks]
#     total = addGreeks(total, gks)
#     if haskey(xpirs, targetDate)
#         xpirs[targetDate] = addGreeks(xpirs[targetDate], gks)
#     else
#         xpirs[targetDate] = gks
#     end
#     return (;total, xpirs)
# end

function tradeInfo(trade, date)
    op = trade[:open]
    xpir = getExpir(trade)
    daysLeft = bdays(date, xpir)
    dateOpen = toDateMarket(op.ts)
    daysTotal = bdays(dateOpen, xpir)
    xpirRatio = (1 + daysLeft) / daysTotal
    neto = op.neto
    # lmsc = tos(LegMetaClose, trade[:lms])
    lmsc = trade[:lmsTrack]
    qt = getQuote(lmsc)
    netc = bap(qt, .1)
    curVal = neto + netc
    timt = DateUtil.timult(dateOpen, date)
    mn = min(OptionUtil.legsExtrema(neto, trade[:legs]...)...)
    rate = timt * curVal / (-mn)
    return (; xpir, daysLeft, daysTotal, xpirRatio, neto, lmsc, qt, netc, curVal, timt, mn, rate)
end
#endregion

#region Utils
rond(x::Real) = round(x; digits=4)
# usePrice(q) = bap(q, .2) # getBid(q)
# netOpen(lms) = sum(usePrice, lms)
# priceOpen(trade) = usePrice(quoter(trade[:lms], Action.open))
# netClose(trade) = usePrice(quoter(trade[:lms], Action.close))
getExpir(trade) = trade[:targetDate] # minimum(getExpiration, lms)
openDur(trade) = bdays(toDateMarket(trade[:open].ts), toDateMarket(trade[:close].ts)) + 1
bdaysLeft(from, trade) = bdays(from, getExpir(trade))
# aboveRat(s, curp) = (s - curp) / s

# NOTE: I think this is handled by the filtering in getOqss
# function entryFilterOption(acct)
#     d = entryFilterLookup(acct)
#     return function(opt, side)
#         status = get(d, getOption(opt), 0)
#         return !(status != 0 && (status == 2 || status != Int(side)))
#     end
# end

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
#endregion

#region MaybeMove
toLMC(leg::Leg, lup) = LegMetaClose(leg, lup(getOption(leg)))
toLMC(lm::LegMetaOpen, lup) = toLMC(getLeg(lm), lup)
tosLLMC(lms, lup) = map(lm -> toLMC(getLeg(lm), lup), lms)
tosLMC(legs, lup) = map(leg -> toLMC(leg, lup), legs)
#endregion

end