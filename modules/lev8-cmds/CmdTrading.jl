module CmdTrading
using Dates, IterTools
using Globals, BaseTypes, SH, SmallTypes, StatusTypes, TradeTypes, LegTradeTypes, LegMetaTypes
using DateUtil, LogUtil, CollUtil, Pricing
using Trading
using Calendars, Markets, Expirations, Chains, StoreTrade
using OptionUtil
using OutputUtil

export ot, otr, cancel, todo, ct, ctr, drpos, tot, drx, toc, todup
export cleg, clegr

#region OpenTrades
using Trading
ot(lms::Coll{LegMetaOpen}; kws...) = _ot(lms; kws...)
otr(lms::Coll{LegMetaOpen}; kws...) = _ot(lms; kws..., pre=false)
function _ot(lms::Coll{LegMetaOpen}; pre=true, skip_confirm=false)
    canTrade(pre)
    Globals.set(:soRunLast, now(UTC))

    if ( exp = minimum(getExpir.(lms)) ; exp == market().startDay || exp == today() )
        msg = string("Attempted to enter a trade that expires the same day ", exp)
        pre ? (@error msg) : error(msg)
    end

    # TODO: Show risk and basic rate
    println("Opening trade with $(length(lms)) legs for $(Pricing.price(lms)) ($(join(Pricing.price.(lms), ",")))")
    if !skip_confirm && !confirm()
        println("Aborted.")
        return -1
    end
    # TODO: last check: requote to make sure it isn't very different from expected

    tid = Trading.open_trade(market(), lms; pre)
    return pre ? 0 : tid
end
#endregion

#region CloseTrades
ct(tid::Int; kws...) = ct(ST.getTradeOpen(tid); kws...)
ct(trad::Trade{<:Closeable}; kws...) = _ct(trad; kws..., pre=true)

ctr(tid::Int; kws...) = ctr(ST.getTradeOpen(tid); kws...)
ctr(trad::Trade{<:Closeable}; kws...) = _ct(trad; kws..., pre=false)

function _ct(trad::Trade{<:Closeable}; pre=true, skip_confirm=false, leginds=nothing, kws...)
    tid = getId(trad)
    canTrade(pre)
    if !intest()
        if ( openDate = toDateMarket(tsFilled(trad)) ; openDate == market().startDay || openDate == today() )
            @info "dupe in test" intest()
            msg = string("Attempted to close trade that was opened the same day ", tid, ' ', openDate)
            pre ? (@error msg) : error(msg)
        end
    end
    Globals.set(:ctRunLast, now(UTC))

    # TODO: put in Between?
    lms = tos(LegMetaClose, getLegs(trad), Chains.chainLookup)
    isnothing(leginds) || ( lms = lms[leginds] )

    println("Closing trade $(tid) with $(length(lms)) legs for $(Pricing.price(Action.close, lms)) ($(join(Pricing.price.(Action.close, lms), ",")))")
    # if !pre && skip_confirm && !confirm()
    if !skip_confirm && !confirm()
        println("Aborted.")
        return -1
    end

    Trading.close_trade(market(), tid, lms; kws..., pre)
end
cleg(lid::Int, p=P(0.01); isMkt=false) = ( canTrade(true) ; closeLeg(ST.getLegTrade(lid), P(p); pre=true, isMkt) )
clegr(lid::Int, p=P(0.01); isMkt=false) = ( canTrade(false) ; closeLeg(ST.getLegTrade(lid), P(p); pre=false, isMkt) )
#endregion

#region todo
import ProcOrder
function toc(rateMin=0.0) # findTradesToClose
    @assert isnothing(snap()) "toc() doesn't work when snapped"
    trades = sort!(StoreTrade.tradesOpen(); by=getTargetDate)
    # ords = filter!(x->!SH.isStatus(x, Deleted), tos(Order, ta.tradierOrders()))
    tords = filter!(x -> tierIsLive(x), ta.tradierOrders())
    println("tords ", length(tords))
    tids = ProcOrder.extractTid.(tords)
    todayDate = market().startDay
    for trade in trades
        trade isa Trade{Filled} || continue
        ts = tsFilled(trade)
        ts < DateTime(today()) || continue
        getTargetDate(trade) > today() || continue
        neto = getNetOpen(trade)
        qt = quoter(trade, Action.close)
        netc = bap(qt)
        curVal = neto + netc
        # if curVal > 0.0
            # tex = calcTex(ts, today() + Day(1))
            # timult = 1 / Calendars.texToYear(tex)
            dur = DateUtil.durRisk(toDateMarket(ts), todayDate)
            timt = DateUtil.timult(toDateMarket(ts), todayDate)
            mn = max(Pricing.calcMargin((getLegs(trade)...,))) # min(OptionUtil.legsExtrema(neto, getLegs(trade)...)...)
            rate = timt * curVal / mn
            # @show rate timt curVal (-mn)
            if rate > rateMin
                tid = getId(trade)
                expr = xp.whichExpir(getTargetDate(trade))
                println(expr, ": Trade ", tid, " (", strShort(Date(ts)), " - ", strShort(getTargetDate(trade)), "): ", map(x -> sho(x), (;curVal, neto, netc, rate, mn, dur, timt)))
                ind = findfirst(x -> x == tid, tids)
                isnothing(ind) || println("    Ord: ", ordinfo(tords[ind]))
            end
        # end
    end
end
ordinfo(tord) = "oid:$(tord["id"]) status:$(tord["status"]) price:$(tord["price"])"

tradeRate(trade::Trade{Filled}, to::Date, netc) = calcRate(getDateFilled(trade), to, getNetOpen(trade) + netc, tradeRisk(trade))
tradeRate(trade::Trade{Closed}) = calcRate(getDateFilled(trade), getDateClosed(trade), getPnl(trade), tradeRisk(trade))
# tradeRisk(trade) = -min(OptionUtil.legsExtrema(getNetOpen(trade), getLegs(trade)...)...)
# SH.getRisk(tradeOpen::Trade{Filled}) = -Pricing.calcCommit(LL.toSegments(Tuple(getLegs(tradeOpen))))
import LinesLeg as LL
function SH.getRisk(trade::Trade{Closed})
    legs = getLegs(trade)
    if length(legs) < 2
        println("WARN: not impl, risk for single leg")
        return 100.0
    end
    -Pricing.calcCommit(LL.toSegments(Tuple(legs)))
end
tradeRisk(trade) = getRisk(trade)
#endregion

#region CurrentPosition
using CmdUtil
function todo(ex=0)
    Globals.set(:todoRunLast, now(UTC))
    legOvers = queryLeftovers()
    if !isempty(legOvers)
        println("Leftovers:")
        pretyble(to.(NamedTuple, legOvers))
    end
    @info "Current price: $(market().curp)"
    return collect(Iterators.filter(x -> getTargetDate(x) == expir(ex), tradesOpen()))
    # trades = tradesToClose(ex)
    # return trades
end

# using RetTypes, Between, DrawStrat
# toRet(trades, exp)::Ret = combineTo(Ret, trades, exp, market().curp, Globals.get(:vtyRatio)) # TODO: choose diff start price?
# # toLms(trades, exp)::Ret = combineTo(LegMeta, trades, exp, market().curp, Globals.get(:vtyRatio)) # TODO: choose diff start price?
# toRet(trades)::Ret = combineTo(Ret, trades, market().curp) # TODO: choose diff start price?
# toLms(trades)::Vector{LegMeta} = combineTo(Vector{LegMeta}, trades) # TODO: choose diff start price?
# # TODO: change so matches todo and expirs and all: 0 for today, 1 for non-today next exp, and default is 0
# # drpos(exp=expir(0)) = drawRet(toRet(tradesToClose(exp), exp); prob=prob(), curp=market().curp, label="pos")
# export drt, adrt
# # drt(i::Int, ex=1) = drawRet(toRet([tradesFor(ex)[i]], ex), prob(), market().curp, "t$(i)")
# # adrt(i::Int, ex=1) = drawRet!(toRet([tradesFor(ex)[i]], ex), "t$(i)")
# drt(tid::Int) = ( trade = cacheTrades[tid] ; drawRet(toRet([trade], getTargetDate(trade)), prob(), market().curp, "t$(tid)") )
# adrt(tid::Int) = ( trade = cacheTrades[tid] ; drawRet!(toRet([trade], getTargetDate(trade)), "t$(tid)") )
# drt(trade::Trade) = drawRet(toRet([trade], getTargetDate(trade)); probs=probsFor(getTargetDate(trade)), curp=market().curp, label="t$(getId(trade))")
# adrt(trade::Trade) = drawRet!(toRet([trade], getTargetDate(trade)); label="t$(getId(trade))")

# function drx(ex=0)
#     # TODO: read from cache
#     tod = tradesToClose(ex)
#     drt(tod[1])
#     for i in 2:length(tod)
#         adrt(tod[i])
#     end
#     drawRet!(toRet(tod, expir(ex)); label="all")
# end

# # TODO: moved to CmdPos as xdr
# function drx(ex, lms)
#     # TODO: read from cache
#     tod = tradesToClose(ex)
#     drt(tod[1])
#     for i in 2:length(tod)
#         adrt(tod[i])
#     end
#     curp = market().curp
#     drawRet!(combineTo(Ret, lms, curp); label="add")
#     posLms = toLms(tod)
#     drawRet!(combineTo(Ret, vcat(posLms, lms), curp); label="all")
# end

# drawRet(tradesToRets(todo(ex)))
# calcPosStrat(today(), market().startPrice, Globals.get(:vtyRatio))
#endregion

tierIsLive(tord) = !(tord["status"] in ["canceled", "rejected"])
findTord(tords, tid) = find(x -> parse(Int, SubString(x["tag"], 3)) == tid, tords)

using TradierAccount, OrderTypes
function todup()
    # TODO: need to show filled ones so we can cancel pending duplicates
    tords = filter!(x -> tierIsLive(x) && startswith(get(x, "tag", ""), "op"), ta.tradierOrders())
    # tt = findTradeEntered(today())
    tt = tradesOpenEntered(today())
    lup = Dict(getId(trade) => findTord(tords, getId(trade)) for trade in tt)
    res = [(;xpr=xp.whichExpir(getTargetDate(trade)), tid=getId(trade), status=typeof(trade).parameters[1], oid=lup[getId(trade)]["id"]) for trade in cu.findDupes!(tt; by=getTargetDate)]
    sort!(res; by=x -> 10000*x.xpr + x.tid)
    pretyble(res)
end

export todord
todord() = sort(xp.whichExpir.(getTargetDate.(tradesOpenEntered(today()))))

import TradierOrder
function cancel(tid)
    tords = filter!(x -> tierIsLive(x) && startswith(x["tag"], "op"), ta.tradierOrders())
    # tt = findTradeEntered(today())
    tt = tradesOpenEntered(today())
    lup = Dict(getId(trade) => findTord(tords, getId(trade)) for trade in tt)
    oid = lup[tid]["id"]
    println("Cancelling oid ", oid)
    TradierOrder.cancelOrder(oid)
end

#region Experiment
# TODO: use this to examine once I have iv data for specific legs
# getMeta.(getLegs(todo()[3]))
# I could look up the ivs in the old db
# using LegTypes, OptionMetaTypes, ChainTypes, Rets
# lmToRet(lm::LegMeta, om::OptionMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = makeRet(getLeg(lm), om, bap(lm), forDate, sp, vtyRatio)
# tradesToRet(trades::AVec{<:Trade}, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret =
#     combineRets([lmToRet(lm, getMeta(optQuoter(lm)), forDate, sp, vtyRatio) for lm in collect(mapFlattenTo(getLegs, LegMeta, trades))])
# toRet2(trades, ex)::Ret = tradesToRet(trades, expir(ex), market().startPrice, 1.0) # TODO: choose diff start price?
# drpos2(ex=1) = drawRet!(toRet2(todo(ex), ex), "pos2")#, prob(), market().curp, "pos2")
#endregion

export pnls
function pnls(since=bdaysBefore(today(), 20))
    trades = ST.tradesClosed(;age=Minute(0), since) # findTrades(Closed)
    sort!(trades; by=tsClosed)
    bal = PriceT(0.0)
    tbl = map(trades) do trade
        pnl = getPnl(trade)
        bal += pnl
        opened = getDateFilled(trade)
        closed = getDateClosed(trade)
        dur = DateUtil.durRisk(opened, closed)
        (; expir=getTargetDate(trade), opened, closed, dur, tid=getId(trade), rate=tradeRate(trade), pnl, risk=tradeRisk(trade), bal)
    end
    pretyble(tbl)
end

#region Local
function canTrade(pre::Bool)
    snapped = !isnothing(snap())
    pre && intest() && snapped && return true
    pre && dev() && return true
    snapped && error("Don't trade when snapped")
    # NOTE: maybe allow anything if tenv() === :paper
    # These checks will also fail if snapped
    isMarketOpen() || error("Only make trades within market hours")
    Markets.canTrade()
    Chains.canTrade()
    !dev() || error("Don't make trades in devMode")
end
#endregion

using TradierAccount
function tradeSize(kelly::Float64, kellyRatio::Float64 = 0.5)
    bal = tradierBalances()["total_cash"]
    println("Starting bal ", bal)
    trades = sort!(collect(values(tradesCached())); by=x -> tsCreated(x))
    rows = NamedTuple[]
    # TODO: groupby expiration and get worst case for all trades together in expiration
    for trade in trades
        # lms = to(Vector{LegMeta}, trade)
        legs = getLegs(trade)
        if length(legs) == 4
            mn = min(OptionUtil.legs4Extrema(legs)...)
        elseif length(legs) == 2
            mn = min(OptionUtil.legs2Extrema(legs)...)
        else
            error("not supported single leg calc yet")
        end
        bal += 100 * mn
        push!(rows, (;mn, bal))
    end
    pretyble(rows)
    println("$(kellyRatio) kelly trade size: ", kelly * kellyRatio * bal)
end

function tradeSize2(kelly::Float64, kellyRatio::Float64 = 0.5)
    bal = tradierBalances()["total_cash"]
    println("Starting bal ", bal)
    trades = sort!(collect(values(tradesCached())); by=getTargetDate)
    rows = NamedTuple[]
    for extrades in groupby(t -> getTargetDate(t), trades)
        # legs = Iterators.flatten(map(getLegs, extrades))
        date = getTargetDate(extrades[1])
        ret = toRet(extrades, date)
        mn = minimum(getVals(ret))
        bal += 100 * mn
        push!(rows, (;date, mn, bal))
    end
    pretyble(rows)
    println("$(kellyRatio) kelly trade size: ", kelly * kellyRatio * bal)
end

end