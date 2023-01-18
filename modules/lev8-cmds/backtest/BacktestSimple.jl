module BacktestSimple
using Dates, MutableNamedTuples
import StatsBase
using SH, Globals, BaseTypes, SmallTypes, OptionTypes, LegTypes, LegMetaTypes, OptionMetaTypes, ChainTypes, QuoteTypes
using DateUtil, CollUtil, DictUtil
using Pricing, Between
import OptionUtil
import Shorthand
import HistData as HD
import SimpleStore as SS
using Calendars
import ChainUtil

#= Explore pricing
lmss = bt.findLongSpreadEntry(expir(16), market().curp, (;rat=.8, off=30.0))
lmss2 = bt.findLongSpreadEntry(expir(16), market().curp, (;rat=1.0, off=5.0))
priceOption(market().curp, texPY(market().tsMarket, lmss[2]), lmss[2], .35)
priceOption(market().curp, texPY(market().tsMarket, lmss2[1]), lmss2[1], .276)
=#

# const TradeType = Dict{Symbol,Any}
const TradeType8 = NamedTuple{
        (:id, :legs, :open, :targetDate, :multiple, :curp, :lmsTrack, :close, :pnl, :pnlAll, :rateVal, :rat),
        Tuple{Int64,
            Tuple{LegTypes.Leg, LegTypes.Leg},
            NamedTuple{(:label, :ts, :date, :neto, :basisAll, :netVal, :riskVal, :rate, :quotes, :metas),
                Tuple{String, DateTime, Date, Currency, Currency, Currency, Currency, Float64, Tuple{QuoteTypes.Quote, QuoteTypes.Quote}, Tuple{OptionMetaTypes.OptionMeta, OptionMetaTypes.OptionMeta}}},
            Date, Int64, Currency,
            Ref{Tuple{LegMetaTypes.LegMetaClose, LegMetaTypes.LegMetaClose}},
            Ref{NamedTuple{(:label, :ts, :netc, :quotes, :metas),
                Tuple{String, DateTime, Currency, Tuple{QuoteTypes.Quote, QuoteTypes.Quote}, Tuple{OptionMetaTypes.OptionMeta, OptionMetaTypes.OptionMeta}}}},
            Ref{Currency}, Ref{Currency}, Ref{Float64}, Ref{Float64}}
        }

import Statistics:quantile
function lyze()
    acct = ac
    closes = filter(t -> isClosed(t), acct.trades)
    calls = filter(t -> getTradeStyle(t) == Style.call, closes)
    puts = filter(t -> getTradeStyle(t) == Style.put, closes)
    println("Total: $(length(closes)), calls: $(length(calls)), puts: $(length(puts))")
    wins = filter(t -> rawTradePnl1(t) > 0.0, closes)
    losses = filter(t -> rawTradePnl1(t) <= 0.0, closes)
    @assert length(closes) == length(wins) + length(losses)
    loscall = filter(t -> getTradeStyle(t) == Style.call, losses)
    wincall = filter(t -> getTradeStyle(t) == Style.call, wins)
    losput = filter(t -> getTradeStyle(t) == Style.put, losses)
    winput = filter(t -> getTradeStyle(t) == Style.put, wins)
    println("Losses: $(length(loscall)) calls, $(length(losput)) puts")
    rateValCall = map(getTradeRateVal, calls)
    rateValPut = map(getTradeRateVal, puts)
    println("Call rate quantile:\n$(!isempty(rateValCall) ? rond.(quantile(rateValCall, .1:.1:.9)) : "no calls")")
    println("Put rate quantile:\n$(!isempty(rateValPut) ? rond.(quantile(rateValPut, .1:.1:.9)) : "no puts")")
    expdurcall = map(calcTradeExpDur, calls)
    expdurput = map(calcTradeExpDur, puts)
    println("Call expdur quantile:\n$(!isempty(expdurcall) ? rond.(quantile(expdurcall, .1:.1:.9)) : "no calls")")
    println("Put expdur quantile:\n$(!isempty(expdurput) ? rond.(quantile(expdurput, .1:.1:.9)) : "no puts")")
end

import DrawUtil, GLMakie
function realBals(acct)
    DrawUtil.drawDates(acct.realBals)
end
function bals(acct)
    DrawUtil.drawDates(acct.bals)
end

tofrac(x::Millisecond) = Dates.value(x) * 1e-3 / 3600 / 24

#region CurStrat
# checkRateRatio(acct, t, p) = t.rate >= (t.xpirRatio * p.RateMin) ? "rate: $(rond(t.rate))" : nothing
# checkProfit(acct, t, p) = t.curVal >= p.MinTakeProfit ? "take min profit" : nothing
# function checkThreaten(acct, t, p)
#     curp = acct.curp
#     if t.style == Style.put
#         threat = curp / getStrike(t.lmsc[2])
#         if threat < 1.001
#             log("checkThreaten put #$(t.id): $(threat) theta:$(getGreeks(t.lmsc).theta) curval:$(t.curVal)")
#             # return "threat: $(threat)"
#         end
#     else
#         threat = getStrike(t.lmsc[1]) / curp
#         if threat < 1.001
#             log("checkThreaten call #$(t.id): $(threat) theta:$(getGreeks(t.lmsc).theta) curval:$(t.curVal)")
#             # return "threat: $(threat)"
#         end
#     end
#     return nothing
#     # s = getStrike(t.lmsc[1])
#     # threat = (s - acct.curp) / s
#     # return threat <= p.ThreatenPercent ? "threat: $(threat)" : nothing
# end

# function checkSides(acct, t, p)
#     # vix = acct.vix
#     # t.rate >= (.8 - vix)/.6 && return "rate >= 1.0"
#     t.rate >= p.takeRateMin && return "rate high enough $(t.rate)"
#     # qty = getQuantity(t.lmsc[1])
#     # if t.style == Style.call
#     #     t.curVal >= qty * t.trade.curp * p.Call.takeRat && return "Call take min profit"
#     # else
#     #     t.curVal >= qty * t.trade.curp * p.Put.takeRat && return "Put take min profit"
#     # end
#     return nothing
# end

# General calcs
# (basisAll = -105.000, neto = 0.020, multiple = 10, multNew = 5350, multRat = 535.0, innerStrike = 223.0, netVal = 0.000, riskVal = 1.000)
calcVal(multiple, neto, spreadWidth, basisAll) = (
    # println("in calvcal", (;multiple, neto, spreadWidth, basisAll)) ;
    netVal = (multiple * neto + basisAll) / multiple ; return (netVal, spreadWidth - netVal) )
getSpreadWidth(legs::Coll) = ( @assert length(legs) == 2 ; abs(getStrike(legs[1]) - getStrike(legs[2])) )

# Anytime
getSpreadWidth(trade::NamedTuple) = getSpreadWidth(trade.legs)
getTradeGreeks(trade) = getGreeks(trade.lmsTrack[])
getTradeStyle(trade) = getStyle(trade.legs[1])
getTradeSide(trade) = getSide(trade.legs[1])
getTradeExpiration(trade) = getExpiration(trade.legs[1])
calcTradeRat(trade)::Float64 = getTradeStyle(trade) == Style.call ? F(getStrike(trade.legs[1])) / trade.curp : F(getStrike(trade.legs[2])) / trade.curp
calcTradeExpDur(trade) = bdays(Date(trade.open.ts), Date(trade.targetDate))
calcTradeMarginAll(trade) = trade.multiple * getSpreadWidth(trade)
getTradeRiskVal(trade) = trade.open.riskVal

# During
calcTradeCurVal(trade) = trade.open.neto + trade.open.basisAll/trade.multiple + calcTradeCurClose(trade)
calcTradeCurClose(trade) = netClose(trade.lmsTrack[])
calcTradeCurRateVal(trade, to) = calcRate(trade.open.date, to, calcTradeCurVal(trade), trade.open.riskVal)

# After closed
rawTradePnl1(trade) = trade.open.neto + trade.close[].netc
rawTradePnlAll(trade) = trade.multiple * (rawTradePnl1(trade))
getTradeRateVal(trade) = trade.rateVal[]

isClosed(trade) = isassigned(trade.close)

netOpen(x::Union{LegMetaOpen,Coll{LegMetaOpen}}, r=0.1) = bap(x, r)
netClose(x::Union{LegMetaClose,Coll{LegMetaClose}}, r=0.0) = bap(x, r)
netOpen(oq::OptionQuote, side::Side.T, r=0.1)::Currency = bap(QuoteTypes.newQuote(getQuote(oq), DirSQA(side, 1.0, Action.open)), r)
netOpen(oq1::OptionQuote, side1::Side.T, oq2::OptionQuote, side2::Side.T, r=0.1)::Currency = netOpen(oq1, side1, r) + netOpen(oq2, side2, r)
#endregion

#region Process
run(f, fday, params, months::Int; maxSeconds=10) = run(f, fday, params, (Date(2022,9)-Month(months-1)):Month(1):Date(2022,9); maxSeconds)
function run(f, fday, params, months; maxSeconds=10)
    LogUtil.resetLog(:backtest)

    vix = Dict{Date,Currency}()
    vix1 = HD.dailyDict(first(months), last(months)+Month(1), "VIX")
    for (k, v) in vix1
        vix[k] = v.open
    end
    daily = HD.dataDaily()

    dateStart = nextTradingDay(first(months))
    acct = MutableNamedTuple(
        # Immutable
        dateStart = dateStart,
        params = params,
        start = time(),
        # Containers
        trades = Vector{TradeType8}(),
        poss = Vector{TradeType8}(),
        todayOpens = Vector{TradeType8}(),
        todayCloses = Vector{TradeType8}(),
        rates = Vector{Float64}(),
        bals = Vector{Tuple{Date,Currency}}(),
        realBals = Vector{Tuple{DateTime,Currency}}(),
        # Mutable
        ts = DATETIME_ZERO,
        date = DATE_ZERO,
        lastTradeId = 0,
        bal = C(params.balInit),
        real = CZ,
        margin = marginZero(),
        marginDay = marginZero(),
        marginMax = marginZero(),
        openMax = 0,
        realBal = CZ,

        curp = CZ,
        vix = 0.0,
        extrema = (hi=CZ, lo=CZ),
        xpirs = Vector{Date}(),

        xoqss = Dict{Date, Styles{Sides{Vector{ChainTypes.OptionQuote}}}}(),
        xoqssAll = Dict{Date, Styles{Sides{Vector{ChainTypes.OptionQuote}}}}(),
        search = Dict{Date,ChainUtil.ChainSearchS2}(),
        lup = flookup(Dict{Date, Styles{Dict{Currency, ChainTypes.OptionQuote}}}()),
    )
    global ac = acct

    for m in months
        # @show m
        procMonth(f, fday, year(m), month(m), params, maxSeconds, acct, daily, vix) || break
    end
    lyze()
    showResult()
end

function showResult()
    acct = ac
    rpnl = acct.real
    unreal = urpnl(acct.poss)
    total = rpnl + unreal
    days = bdays(acct.dateStart, acct.date)
    dateEnd = acct.date
    # We're going to pretend we closed everything at the last time, so don't need to add risk.
    # acct.riskDays += getRisk(acct.poss) # model it for end of last day, so only * 1 day
    # rate = calcRate(dateStart, dateEnd, total, acct.riskDays / days)
    rateMean = StatsBase.mean(acct.rates)
    rr = rpnl < 0.0 ? rpnl : (1 + rpnl / acct.params.balInit) ^ (1 / (days / DateUtil.bdaysPerYear())) - 1
    log("Summary $(acct.dateStart) - $(dateEnd) (ran $(days) bdays):")
    log("  bal = $(acct.bal), rpnl = $(rpnl), urpnl = $(rond(unreal))")
    log("  Total: $(rond(total))")
    log("  overall realized rate: $(rond(rr))")
    log("  trade rate mean: $(rond(rateMean))")
    log("  openMax: $(acct.openMax)")
    log("  marginMax: $(acct.marginMax)")

    println("Summary $(acct.dateStart) - $(dateEnd) (ran $(days) bdays):")
    println("  bal = $(acct.bal), rpnl = $(rpnl), urpnl = $(unreal)")
    println("  Total: $(total)")
    println("  overall realized rate: $(rond(rr))")
    println("  trade rate mean: $(rond(rateMean))")
    println("  openMax: $(acct.openMax)")
    println("  marginMax: $(acct.marginMax)")
end

function procMonth(f, fday, y, m, params, maxSeconds, acct, daily, vix)
    data = SS.load(y, m)
    log("Loaded $(y) $(m)")
    # Don't run on first and last (other if inside loop) ts of the day
    tss = filter!(SS.getTss(data)) do x
        # For consistency, just do every 30 minutes because that's what we have data for 10 years
        return minute(x) != 15 && minute(x) != 45 && Time(x) != Time(getMarketOpen(Date(x))) + Second(10)
    end

    datePrev = Date(0)
    curp = C(0)
    for ts in tss
        log("Looping ts=$(ts)")
        date = toDateMarket(ts)
        acct.vix = vix[date] / 100

        if date != datePrev
            # tind = 1
            # while tind <= length(acct.poss)
            #     trade = acct.poss[tind]
            loopPoss(acct) do trade
                if getExpir(trade) < date
                    lmsc = trade.lmsTrack[]
                    netc = OptionUtil.netExpired(lmsc, curp)
                    if abs(curp - getStrike(lmsc[1])) < 5.0 || abs(curp - getStrike(lmsc[end])) < 5.0
                        netc -= 0.02 # adjust to pay to close and not just let expire
                    end
                    log("ERROR: Found expired trade: ", trade)
                    closeTrade(acct, trade, lmsc, netc, "(missed) expired $(getQuote(trade.lmsTrack[]))")
                    return true
                else
                    return false
                end
            end

            curp = SS.getUnder(data, ts).under

            fday(acct)
            rate = StatsBase.mean(acct.rates)
            log("End $(datePrev): bal:$(rond(acct.bal)) margin:$(acct.margin) rpnl:$(rond(acct.real)) urpnl:$(rond(urpnl(acct.poss))) #open:$(length(acct.poss)) rateAvg:$(rond(rate))")

            # TODO: closing and opening on same day might affect this a little, but I think buying power allows this so maybe it's ok
            # acct.riskDays += (date - datePrev).value * getRisk(acct.poss)
            empty!(acct.todayOpens)
            empty!(acct.todayCloses)
            acct.marginDay = marginZero()
            acct.extrema = HD.extrema(daily, bdaysBefore(date, params.ExtremaBdays), date-Day(1), curp, curp)
            datePrev = date
        else
            curp = SS.getUnder(data, ts).under
            updateExtrema(acct, curp)
        end

        log("Start $(formatLocal(ts)) SPY: $(curp)") # TODO: , VIX: $(mkt.vix)")
        acct.ts = ts
        acct.date = date
        acct.curp = curp
        xpirs = SS.getExpirs(data, ts)
        acct.xpirs = sort!(collect(xpirs))
        posLegs = collect(flatmap(x -> x[:legs], acct.poss))
        all, search, entry, xoqssAll = getChains(data, ts, xpirs, curp, posLegs)
        acct.xoqss = entry
        acct.xoqssAll = xoqssAll
        acct.search = search
        # lup = (xpir, args...) -> flookup(all) # @coalesce ChainUtil.lup(all[xpir], args...) log("ERROR: Backtest could not quote $(xpir), $(style), $(strike)")
        lup = flookup(all)
        acct.lup = lup
        updatePossLmsTrack(lup, acct.poss)
        # acct.greeks = calcGreeks(acct.poss) # isempty(acct.poss) ? GreeksZero : getGreeks(acct.poss)
        # acct.greeksExpirs = isempty(acct.poss) ? Dict{Date,GreeksType}() : greeksForExpirs(lup, acct.poss)

        # Run strat for this time

        if ts == getMarketClose(date)
            # tind = 1
            # while tind <= length(acct.poss)
            #     trade = acct.poss[tind]
            loopPoss(acct) do trade
                xpir = getExpir(trade)
                if ts == getMarketClose(xpir)
                    lmsc = trade.lmsTrack[]
                    netc = OptionUtil.netExpired(lmsc, curp)
                    if abs(curp - getStrike(lmsc[1])) < 5.0 || abs(curp - getStrike(lmsc[end])) < 5.0
                        # average pay .02 per short to buy back, assuming balanced long/short positions
                        netc -= .01 * sum(getQuantity, lmsc)
                    end
                    closeTrade(acct, trade, lmsc, netc, "expired $(getQuote(trade.lmsTrack))")
                    return true
                else
                    return false
                end
            end
        else
            f(acct)
        end
        push!(acct.bals, (acct.date, acct.bal))

        if time() > acct.start + maxSeconds
            return false
        end
    end
    return true
end

function getChains(data, ts::DateTime, xpirs, curp::Currency, legsCheck)
    all = Dict{Date,ChainUtil.ChainLookup}()
    entry = Dict{Date,Oqss}()
    oqssAll = Dict{Date,Oqss}()
    search = Dict{Date,ChainUtil.ChainSearchS2}()
    for xpir in xpirs
        oqs = SS.getOqs(data, ts, xpir)
        all[xpir] = ChainUtil.tolup(oqs)
        search[xpir] = ChainUtil.toSearch(oqs)
        entry[xpir] = ChainUtil.oqssEntry(oqs, curp, legsCheck)
        oqssAll[xpir] = ChainUtil.oqssAll(oqs)
    end
    return (;all, search, entry, oqssAll)
end

function updatePossLmsTrack(lup, trades)
    i = 1
    for trade in trades
        try
            trade.lmsTrack[] = tosLMC(trade.legs, lup)
            # trade.lmsTrack = map(leg -> LegMetaClose(leg, lup(getOption(leg)), trade.legs))
        catch e
            log("Could not quote trade $(string(trade))")
            # rethrow(e)
            typeof(e) == InterruptException && rethrow(e)
        end
        i += 1
    end
end

function updateExtrema(acct, curp)
    x = acct.extrema
    if curp > x.hi || curp < x.lo
        acct.extrema = (;hi=max(x.hi, curp), lo=min(x.lo, curp))
    end
end

nextTradeId(acct) = acct.lastTradeId += 1

function check(fs, args...)
    ss = filter(!isnothing, map(x -> x(args...), fs))
    return isempty(ss) ? nothing : join(ss, " ; ")
end

function loopPoss(f, acct)
    # Trades might have been opened during this process so we have to be careful to loop through only the trades existing at the beginning of this method execution.

    # I tested and eachindex is only eachindex at the start, so adding to the vector during this loop doesn't change it.
    # for i in eachindex(acct.poss)
    #     f(acct.poss[i])
    # end
    poss = acct.poss
    dels = [f(poss[i]) for i in eachindex(poss)]
    diff = length(acct.poss) - length(dels)
    if diff != 0
        append!(dels, fill(false, diff))
    end
    try
        deleteat!(acct.poss, dels)
        acct.marginDay = recalcMargin(acct.todayOpens) # - recalcMargin(acct.todayCloses)
        acct.margin = recalcMargin(acct.poss)
    catch e
        println("dels: $(typeof(dels))")
        rethrow(e)
    end
end
# function testLoopPoss()
#     poss = [1,2,3]
#     loopPoss((;poss)) do trade
#         push!(poss, -trade)
#         return true
#     end
# end

function checkClose(fs, acct, params, args...)
    date = acct.date
    origCount = length(acct.poss)
    checkedCount = 0
    numDelete = 0
    loopPoss(acct) do trade
        checkedCount += 1
        dateOpen = toDateMarket(trade.open.ts)
        # don't close today's entries
        dateOpen < date || return false
        label = check(fs, acct, trade, getTradeStyle(trade) == Style.put ? params.Put : params.Call, args...)
        if !isnothing(label)
            lms = trade.lmsTrack[]
            closeTrade(acct, trade, lms, calcTradeCurClose(trade), label)
            numDelete += 1
            return true
        end
        return false
    end
    @assert checkedCount == origCount
    @assert length(acct.poss) >= (origCount - numDelete) # gte because could have opened trades
    # println("checkClose $(origCount) - $(numDelete) = $(length(acct.poss))")
end
#endregion

#region Actions
function openTrade(acct, lms::Coll{LegMetaOpen}, multiple, basisAll, label)
    @assert getQuantity(lms[1]) == getQuantity(lms[2])
    neto = bap(lms, .1)
    spreadWidth = getSpreadWidth(lms) # abs(getStrike(lms[1]) - getStrike(lms[2]))
    netVal, riskVal = calcVal(multiple, neto, spreadWidth, basisAll)
    @assert netVal > 0.0
    #  (netVal = 11.003, riskVal = -116.003, multiple = 12, neto = 9.670, basisAll = -105.000, spreadWidth = 16.000)
    @assert riskVal > 0.0 "risk was < 0.0: $((;netVal, riskVal, multiple, neto, basisAll, spreadWidth))\n$(lms)"
    lup = acct.lup
    lmsTrack = tosLLMC(lms, lup)
    targetDate = minimum(getExpiration, lms)
    rate = calcRate(acct.date, targetDate, neto, riskVal)
    trade = (;
        # Immutable
        id = nextTradeId(acct),
        legs = map(getLeg, lms),
        open = (;label, ts=acct.ts, date=acct.date, neto, basisAll, netVal, riskVal, rate, quotes=map(getQuote, lms), metas=map(getOptionMeta, lms)),
        targetDate = targetDate,
        # riskVal = riskVal,
        multiple = multiple,
        curp = acct.curp,
        # Mutable
        lmsTrack = Ref(lmsTrack),
        close = Ref{NamedTuple{(:label, :ts, :netc, :quotes, :metas), Tuple{String, DateTime, Currency,
                    Tuple{Quote, Quote}, Tuple{OptionMeta, OptionMeta}}}}(),
        pnl = Ref{Currency}(),
        pnlAll = Ref{Currency}(),
        rateVal = Ref{Float64}(),
        rat = Ref{Float64}(),
    )
    trade.rat[] = calcTradeRat(trade)
    push!(acct.trades, trade)
    push!(acct.poss, trade)
    push!(acct.todayOpens, trade)
    acct.bal += neto * multiple
    acct.marginDay = recalcMargin(acct.todayOpens) # - recalcMargin(acct.todayCloses)
    margin = recalcMargin(acct.poss)
    acct.margin = margin
    marginRat = margin.risk / acct.bal
    if marginRat > acct.marginMax.risk
        acct.marginMax = (;risk=marginRat, margin.call, margin.put)
    end
    openCount = length(acct.poss)
    if openCount > acct.openMax
        acct.openMax = openCount
    end
    # acct.greeks = updateGreeks(acct, targetDate, getGreeks(lms))
    log("Open #$(trade.id) $(Shorthand.tosh(lms, acct.xpirs)): $((;multiple, neto, basisAll, netVal, riskVal)) '$(label)'")
    return trade
end
function closeTrade(acct, trade, lms::Coll{LegMetaClose}, netc, label)
    multiple = trade.multiple
    neto = trade.open.neto
    pnlVal = trade.open.netVal + netc
    pnl = neto + netc
    pnlAll = pnl * multiple
    trade.close[] = (;label, ts=acct.ts, netc, quotes=map(getQuote, lms), metas=map(getOptionMeta, lms))
    trade.pnl[] = pnl
    trade.pnlAll[] = pnlAll
    trade.rateVal[] = calcTradeCurRateVal(trade, acct.date)
    push!(acct.todayCloses, trade)
    acct.bal += netc * multiple
    acct.real += pnlAll
    push!(acct.rates, getTradeRateVal(trade))
    realBal = acct.realBal + pnlAll
    acct.realBal = realBal
    push!(acct.realBals, (acct.ts, realBal))
    log("Close #$(trade.id) $(Shorthand.tosh(lms, acct.xpirs)): multiple:$(multiple) neto:$(neto) netc:$(netc) pnl:$(pnl) pnlVal:$(pnlVal) riskVal:$(getTradeRiskVal(trade)) rateVal:$(getTradeRateVal(trade)) '$(label)' ; $(openDur(trade)) days open, $(bdaysLeft(acct.date, trade)) days left")
    return
end
#endregion

#region GetData
urpnl(trades::Coll) = sum(x -> urpnl(x), trades; init=0.0)
urpnl(trade) = calcTradeCurVal(trade) * trade.multiple

function flookup(data)
    function lup(xpir::Date, style::Style.T, strike::Currency) # ::Union{OptionQuote,Nothing}
        @coalesce ChainUtil.lup(data[xpir], style, strike) log("ERROR: Backtest could not quote $(xpir), $(style), $(strike)")
    end
    function lup(opt::Option) # ::Union{OptionQuote,Nothing}
        @coalesce ChainUtil.lup(data[getExpiration(opt)], opt) log("ERROR: Backtest could not quote $(opt)")
    end
    return lup
end

# function lookup(xoqss)
#     function lup(xpir::Date, style::Style.T, strike::Currency) # ::Union{Nothing,OptionQuote}
#         try
#             res = find(x -> getStrike(x) == strike, getfield(xoqss[xpir].all, Symbol(style)).long)
#             !isnothing(res) || log("ERROR: Backtest could not quote $(xpir), $(style), $(strike)")
#             return res
#         catch e
#             @show xpir style strike
#             rethrow(e)
#         end
#     end
#     function lup(o::Option)
#         try
#             xpir = getExpiration(o)
#             style = getStyle(o)
#             strike = getStrike(o)
#             res = find(x -> getStrike(x) == strike, getfield(xoqss[xpir].all, Symbol(style)).long)
#             !isnothing(res) || log("ERROR: Backtest could not quote $(xpir), $(style), $(strike)")
#             return res
#         catch e
#             @show o
#             rethrow(e)
#         end
#     end
#     return lup
# end

function calcGreeks(trades)
    d = Dict{Date,Greeks}()
    total = GreeksZero
    for trade in trades
        date = getExpir(trade) # [:targetDate]
        gks = getGreeks(trade.lmsTrack[])
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
#     total, xpirs = acct.greeks
#     total = addGreeks(total, gks)
#     if haskey(xpirs, targetDate)
#         xpirs[targetDate] = addGreeks(xpirs[targetDate], gks)
#     else
#         xpirs[targetDate] = gks
#     end
#     return (;total, xpirs)
# end

# function tradeInfo(trade, date)
#     op = trade.open
#     xpir = getExpir(trade)
#     daysLeft = bdays(date, xpir)
#     dateOpen = toDateMarket(op.ts)
#     # daysTotal = bdays(dateOpen, xpir)
#     # xpirRatio = (1 + daysLeft) / daysTotal
#     lmsc = trade.lmsTrack
#     qty = getQuantity(lmsc[1])
#     qt = getQuote(lmsc)
#     neto1 = op.neto1
#     netc1 = bap1(qt, .0)
#     curVal = neto1 + netc1
#     tmult = DateUtil.timult(dateOpen, date)
#     mn = qty * min(OptionUtil.legsExtrema(neto/qty, trade.legs...)...)
#     @assert mn < 0 "mn:$(mn) qty:$(qty)"
#     rate = tmult * curVal / (-mn)
#     # return (; xpir, daysLeft, daysTotal, xpirRatio, neto, lmsc, qt, netc, curVal, timt, mn, rate, qty, trade)
#     return (; id=trade.id, style=getStyle(lmsc[1]), dateOpen, xpir, daysLeft, netc, curVal, tmult, mn, rate, trade, lmsc)
# end
#endregion

#region Utils
rond(x::Real) = round(x; digits=4)
# usePrice(q) = bap(q, .2) # getBid(q)
# netOpen(lms) = sum(usePrice, lms)
# priceOpen(trade) = usePrice(quoter(trade.lms, Action.open))
# netClose(trade) = usePrice(quoter(trade.lms, Action.close))
getExpir(trade) = trade.targetDate # minimum(getExpiration, lms)
openDur(trade) = bdays(toDateMarket(trade.open.ts), toDateMarket(trade.close[].ts)) + 1
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
    legsPos = cu.mapflatmap(getLeg, x -> x[:lms], acct.poss)
    legsOpens = cu.mapflatmap(getLeg, x -> x[:lms], acct.todayOpens)
    legsCloses = cu.mapflatmap(x -> LegTypes.switchSide(getLeg(x)), x -> x[:lms], acct.todayCloses)
    d = Dict{Option,Int}()
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

function recalcMargin(poss)
    # Brokerage might consider margin offsets only for the same expiration, but for me, the risk is across all expirations, so I'll count it that way.
    len = length(poss)
    risks = Vector{Currency}(undef, len)
    callCount = 0
    callRisk = CZ
    putRisk = CZ
    for i in eachindex(poss)
        trade = poss[i]
        risk = calcTradeMarginAll(trade)
        risks[i] = risk
        if getTradeStyle(trade) == Style.call
            callCount += 1
            callRisk += risk
        else
            putRisk += risk
        end
    end
    sort!(risks; rev=true)
    keep = max(callCount, len-callCount)
    resize!(risks, keep)
    risk = sum(risks)
    # println("Calced new margin: ", margin)
    return (;risk, call=(;risk=callRisk, count=callCount), put=(;risk=putRisk, count=len-callCount))
end
marginZero() = (;risk=CZ, call=(;risk=CZ, count=0), put=(;risk=CZ, count=0))

using LogUtil
# log(args...) = LogUtil.logit(:backtest, args...)
log(args...) = LogUtil.logit(:backtest, args...)
#endregion

#region MaybeMove
toLMC(leg, lup) = LegMetaClose(leg, lup(getOption(leg)))
# toLMC(leg::Leg, lup) = LegMetaClose(leg, lup(getOption(leg)))
toLMC(lm::LegMetaOpen, lup) = toLMC(getLeg(lm), lup)
tosLLMC(lms, lup) = map(lm -> toLMC(getLeg(lm), lup), lms)
tosLMC(legs, lup) = map(leg -> toLMC(leg, lup), legs)
#endregion

end