module Trading
using Dates
using SH, Globals, DateUtil, LogUtil
using BaseTypes, SmallTypes, OptionTypes, LegTypes, StatusTypes, TradeTypes, LegTradeTypes
using TradierOrder, TradierAccount
import Pricing
using Positions # TODO: just being used to grab prices for TradeMeta, and verify positions before close
import StoreTrade:newTrade,getTradeOpen

export price, submitPreview, submitLive, closeTrade, closeLeg

using OutputUtil
import Chains, Between, Pricing

#region Public
function open_trade(mkt, legsin, mineto::PT; pre=false, minextra::PT=P(0.02), kws...)
    TradierAccount.ensure_listening()

    legs = update_legs(Action.open, legsin, mkt.curp)
    extra = validate_legs(Action.open, legs, mineto, minextra)
    !isnothing(extra) || return

    tid = pre ? 0 : newTrade(P(Pricing.price(Action.open, legs)), legs, getBid(mkt.curQuot), getAsk(mkt.curQuot))
    println("Created trade:$(tid)")

    fills = pos_list_do(Action.open, tid, legs, mineto, extra; pre, kws...)
    return fills
end

using Markets
function close_trade(tid, minetc::PT; pre=false, minextra::PT=P(0.02), curp=market().curp, kws...)
    trade = getTradeOpen(tid)
    legsin = getLegs(trade)

    veri = verifyPoss(legsin)
    if false in veri
        @warn "close_trade: missing positions for legs" veri
        legsin = map(first, Iterators.filter(t -> t[2], zip(legsin, veri)))
    end

    TradierAccount.ensure_listening()

    legs = update_legs(Action.close, legsin, curp)
    extra = validate_legs(Action.close, legs, minetc, minextra)
    !isnothing(extra) || return

    fills = pos_list_do(Action.close, tid, legs, minetc, extra; pre, kws...)
    return fills
end

function pos_list_cont(inds, minet=kminet)
    legs = klegs[inds]
    validate_legs(kaction, legs, minet, kminextra)
    pos_list_do(kaction, ktid, legs, minet, kextra)
end

function pos_list_do(action::Action.T, tid, legs, minet::PT, extra::PT=PZ; pre=false, saveforeach::PT=P(0.01), kws...)
    global kaction = action
    global ktid = tid
    global klegs = legs
    global kminet = minet
    global kextra = extra
    TradierAccount.ensure_listening()
    fills = Vector{PT}()
    try
        tosavefor = length(legs)
        for leg in legs
            # price_tgt = Pricing.price(Action.open, leg)
            # price_min = getBid(leg) + tosavefor * saveforeach
            # println("Dist1: $(dist1) leaving min Dist23: $(dist23d), $(dist23pm)")
            price_tgt = ceil(PT, Pricing.price(action, leg))
            println("Used for extra: $(extra / tosavefor)")
            price_min = P(getBid(leg)) - floor(PT, extra / tosavefor)
            res = pos_do(action, tid, leg, price_tgt, price_min; pre, kws...)
            (;oid, status, fill, note) = res
            if status == "filled"
                push!(fills, fill)
                minet -= fill
                extra = fill - price_min
                global kminet = minet
                global kextra = extra
            else
                println("Aborting: not filled leg:$(leg), $(res)")
                break
            end
            tosavefor -= 1
        end
    finally
        println("## Filled prices:$(fills), remaining minet:$(minet) ##")
    end
    return fills
end
#endregion

#region LocalPosDo
function validate_legs(action::Action.T, legs, minet, minextra, minextraleg=CZ)
    global kminextra = minextra
    bid = P(sum(getBid.(legs)))
    ask = P(sum(getAsk.(legs)))

    rat = (minet - bid) / (ask - bid)
    if rat > 0.4
        println("TODO SHOULD HAVE Aborted: neto:$(minet) has rat:$(rd3(rat)) too high with bid:$(bid) and ask:$(ask)")
        # return
    end
    # println("Target neto:$(mineto) is $(rd3(100 * rat))% between bid:$(bid) and ask:$(ask)")

    dist = P(Pricing.price(action, legs) - minet)
    if dist <= 0.0
        println("TODO SHOULD HAVEAborted: no leeway in meeting target neto, dist:$(dist)")
        # return
    end
    if abs(dist) > 0.2
        println("TODO SHOULD HAVEAborted: too far away from current price, check price, dist:$(dist)")
        # return
    end

    for leg in legs[1:end]
        extra = (Pricing.price(action, leg) - getBid(leg))
        if extra < minextraleg
            println("Aborting: Insufficient extra:$(extra) < minextra:$(minextraleg) on leg:$(getLeg(leg))")
            return
        end
    end

    price_tgt = P(Pricing.price(action, legs))
    extra = price_tgt - minet
    extra >= minextra || ( println("Aborting: no extra:$(extra) from $(price_tgt) - $(minet)") ; return )

    return extra
end

function pos_do(action::Action.T, tid, leg, price_tgt::PT, price_min::PT; pre=false, timeout=2.0)
    println("## $(action) $(getSide(leg)) $(getOption(leg)) between $(price_tgt) and $(price_min)")
    price = price_tgt
    oid, resp = pos_new_order(action, tid, leg, price, pre, timeout)
    while !pre && isnothing(resp) && price > price_min
        price -= P(0.01)
        resp = pos_modify_order(action, tid, oid, price, timeout)
    end
    status = isnothing(resp) ? nothing : get(resp, "status", nothing)
    if pre && oid == 0
        status = "filled"
        note = "filled for preview"
    elseif isnothing(status)
        # waiting timed out on all tries down to price_min, so cancel and abort
        resp = TradierOrder.cancelOrder(oid)
        status = get(resp, "status", nothing)
        if status != "ok"
            # check status in case it filled after timeout
            resp = TradierAccount.tradierOrder(oid)
            status = get(resp, "status") do; error("No status in response to tradierOrder: ", resp) end
            if (status == "filled")
                price = parse(PT, get(resp, "avg_fill_price") do; error("No avg_fill_price for filled order $(oid): ", resp) end)
                note = "Filled after timeout at $(price)"
            else
                status = "ERROR"
                note = "Cancel failed but order not filled: " * resp
            end
        else
            note = "Cancelled due to timeout"
        end
    elseif status in STATUS_ERROR
        status = "failed"
        note = "Status in error: " * resp
    elseif status == "filled"
        price = P(get(resp, "avg_fill_price") do; error("No avg_fill_price for filled order $(oid): ", resp) end)
        note = "Filled at $(price)"
    else
        error("Unknown status:$(status) in resp:$(resp)")
    end
    fill = isLong(leg) ? toneg(price) : abs(price)
    return (;oid, status, fill, note)
end

# returns (oid, tord) if filled within timeout, otherwise (oid, nothing)
# there is a chance it could fill after the timeout.
function pos_new_order(action::Action.T, tid, leg, price::PT, pre, timeout)
    cond = Threads.Condition()
    tag = TradierOrder.maketag(tid, action)
    listener = watch_for(cond, tag, timeout)
    TradierAccount.register_listener(listener)

    oid = new_order(action, tid, leg, price; pre)
    oid > 0 || return (0, nothing)
    resp = get_wait(cond)
    println("## $(action) resp from get_wait: $(resp)")

    TradierAccount.unregister_listener(listener)
    return (oid, resp)
end

# returns tord if filled within timeout, otherwise nothing
# there is a chance it could fill after the timeout.
function pos_modify_order(action::Action.T, tid, oid, price::PT, timeout)
    cond = Threads.Condition()
    tag = TradierOrder.maketag(tid, action)
    listener = watch_for(cond, tag, timeout)
    TradierAccount.register_listener(listener)

    modify_order(tid, oid, price)
    resp = get_wait(cond)
    println("## modify resp from get_wait: $(resp)")

    TradierAccount.unregister_listener(listener)
    return resp
end

const STATUS_TERMINAL = ["filled", "expired", "canceled", "rejected", "error"]
const STATUS_ERROR = ["expired", "canceled", "rejected", "error"]

function watch_for(cond, tag, timeout=2.0)
    return function(resp)
        println("watch_for received: $(resp)")
        tg = get(resp, "tag", nothing)
        finished = Threads.Atomic{Bool}(false)
        if tg == tag
            status = get(resp, "status", nothing)
            if status in STATUS_TERMINAL
                println("Order terminal status received: $(status)")
                Threads.atomic_xchg!(finished, true)
                send_notify(cond, resp)
            elseif status == "open"
                println("Order is open, waiting timeout:$(timeout) seconds for fill")
                Threads.@spawn Timer(timeout) do t
                    if !finished[]
                        println("Timed out waiting for fill")
                        send_notify(cond, nothing)
                    end
                end
            else
                println("watch_for received other status:$(status)")
            end
        end
    end
end
#endregion

#region Wrappers
function new_order(action, tid, leg, price; pre)
    payload = TradierOrder.make_payload(action, tid, leg, price; pre)
    println("## new order $(action) pos tid:$(tid) - $(payload)")
    resp = submitOrder(payload)
    oid = get(resp, "id") do; pre ? 0 : error("no id in new_order resp:$(resp)") end
    println("new order $(action) response has oid:$(oid)")
    return oid
end

function modify_order(tid, oid, price)
    println("## modify tid:$(tid) order:$(oid) price:$(price)")
    resp = TradierOrder.modifyOrder(oid, abs(price))

    println("modify pos response $(resp)")
    return resp
end
#endregion

#region Util
function update_legs(action::Action.T, legs, curp)
    xpir = getExpir(legs)
    oqs = Chains.loadChain("SPY", xpir).chain
    lms1 = Between.requote(action, legs; lup=o -> Chains.chainLookup(oqs, o))
    return sortoto(action, curp, lms1)
end

const NEED_SIDE_OPEN = [isLong, isShort]
const NEED_SIDE_CLOSE = [isShort, isLong]

import StatsBase, CollUtil
function sortoto(action::Action.T, curp, legsin::Coll{L}) where L
    style_counts = StatsBase.countmap(map(getStyle, legsin))
    if length(style_counts) > 1
        # It has both styles so process one of them first
        style = CollUtil.maxkey(style_counts)
        legs1 = _sortoto(action, curp, filter(leg -> style == getStyle(leg), legsin))
        legs2 = _sortoto(action, curp, filter(leg -> style != getStyle(leg), legsin))
        return vcat(legs1, legs2)
    else
        return _sortoto(action, curp, legsin)
    end
end

function _sortoto(action::Action.T, curp, legsin::Coll{L}) where L
    fside = action == Action.open ? NEED_SIDE_OPEN : NEED_SIDE_CLOSE
    legs = sort!(collect(legsin); by=l -> -getQuantity(l) + abs(curp - getStrike(l)) / 1024)
    res = Vector{L}()
    posside = 0
    while !isempty(legs)
        i = 0
        # @show i posside
        if posside == 0
            j = findfirst(fside[1], legs)
            if isnothing(j)
                # No more offsides, so just continue pulling onsides
                i = findfirst(fside[2], legs)
                !isnothing(i) || error("sortoto is very confused: ", legs)
                posside -= getQuantity(legs[i])
            else
                i = j
                posside += getQuantity(legs[i])
            end
        else
            j = findfirst(l -> fside[2](l) && getQuantity(l) <= posside, legs)
            if isnothing(j)
                # No shorts that are small enough, take another long (or opposite for closing)
                i = findfirst(fside[1], legs)
                !isnothing(i) || error("Tried to sortoto invalid lms: imbalanced sides 2: ", legs)
                posside += getQuantity(legs[i])
            else
                i = j
                posside -= getQuantity(legs[i])
            end
        end
        # @show legs[i]
        push!(res, legs[i])
        # posside += Int(getSide(leg)) * getQuantity(leg)
        deleteat!(legs, i)
    end
    return res
end

toneg(x) = -abs(x)

# TODO: move to threadutil
function get_wait(cond)
    lock(cond)
    try
        resp = wait(cond)
        return resp
    finally
        unlock(cond)
    end
end

function send_notify(cond, resp)
    lock(cond)
    try
        notify(cond, resp)
    finally
        unlock(cond)
    end
end
#endregion


#region Old
# function open_trade(mkt, legs; pre=true, kws...)
#     # TODO: filter so don't enter opposite side same options on same day
#     # checkTradeLegs(inLegs)

#     tid = pre ? 0 : newTrade(P(Pricing.price(legs)), legs, getBid(mkt.curQuot), getAsk(mkt.curQuot))
#     submit_orders(mkt.curp, Action.open, tid, legs; pre)
#     return tid
# end

# function close_trade(mkt, tid, legs; pre=true)
#     veri = verifyPoss(legs)
#     if false in veri
#         @warn "closeTrade: missing positions for legs" veri
#         legs = map(first, Iterators.filter(t -> t[2], zip(legs, veri)))
#     end
#     # TODO: filter so don't enter opposite side same options on same day
#     # checkTradeLegs(inLegs)

#     return submit_orders(mkt.curp, Action.close, tid, legs; pre)
# end

# function submit_orders(curp, action, tid, legs; pre=true)
#     res = []
#     legs = sortoto(action, curp, legs)
#     for i in 1:2:(length(legs)-1)
#         leg1 = legs[i]
#         leg2 = legs[i+1]
#         pd1 = P(Pricing.price(action, leg1))
#         pd2 = P(Pricing.price(action, leg2))
#         payload = TradierOrder.make_payload_oto(action, tid, leg1, pd1, leg2, pd2; pre)
#         @info "$(action)_trade_oto" payload
#         resp = submitOrder(payload)
#         @info "$(action)_trade_oto" tid resp
#         push!(res, resp)
#     end
#     if isodd(length(legs))
#         legend = legs[end]
#         payload = TradierOrder.make_payload(action, tid, legend, P(Pricing.price(action, legend)); pre)
#         @info "$(action)_trade_1" payload
#         resp = submitOrder(payload)
#         @info "$(action)_trade_1" tid resp
#         push!(res, resp)
#     end

#     pre || ProcOrder.startWatching()
#     return res
# end

# function closeTrade(optQuoter, trade::Trade{<:Closeable}, primitDir::PriceT; pre=true, legs=nothing, skipMin=false)::Union{Nothing,Dict{String,Any}}
#     inLegs = getLegs(trade)
#     inLegs = isnothing(legs) ? inLegs : inLegs[legs]
#     veri = verifyPoss(inLegs)
#     checkTradeLegs(inLegs)
#     if !intest() && false in veri # TODO: enhance test to set positions properly to test this part?
#         @warn "closeTrade: missing positions for legs" veri
#         useLegs = map(first, Iterators.filter(t -> t[2], zip(inLegs, veri)))
#     else
#         useLegs = copy(inLegs)
#     end
#     tid = getId(trade)
#     exp = getTargetDate(trade)
#     if !skipMin
#         # TODO: consider something else because not closing future calendar longs
#         # Close legs which are worthless
#         shortCount = closeTinyLegs!(useLegs, tid, optQuoter, 4, Side.short, exp; pre)
#         if (shortCount > 0)
#             closeTinyLegs!(useLegs, tid, optQuoter, shortCount, Side.long, exp; pre)
#         end
#     end

#     if !skipMin && abs(primitDir) <= 0.01
#         @warn "Resulting order has price <= 0.01 so not submitting. Suggest use closeLegMarket on remaining legs" filter(l->!isLegClosed(getId(l),pre), useLegs)
#         return nothing
#     else
#         @info "closing legs" useLegs
#         payload = length(useLegs) == 1 ? toPayloadCloseSingle(tid, useLegs[1], primitDir; pre) : toPayloadClose(tid, useLegs, primitDir; pre)
#         result = submitOrder(payload)
#         !pre && ProcOrder.startWatching()
#         return result
#     end
# end

# function closeLeg(leg::LegTrade, primitDir::PriceT; pre=true, isMkt=false)::Dict{String,Any}
#     verifyPos(leg) || error("Position not found for closeLeg")
#     @info "Closing leg" leg
#     payload = toPayloadCloseSingle(getTid(leg), leg, primitDir; pre, isMkt)
#     result = submitOrder(payload)
#     !pre && ProcOrder.startWatching()
#     return result
# end

# #region Local
# const PreClosedLegs = Set{Int}()

# # function confirmInside(trad, qt)
# #     x = sum(quot.(optQuote.(getLegs(trad)[1]))) # TODO: should this go requote the leg?
# #     !(getBid(qt) <= x <= getAsk(qt)) && @warn "price is not inside current quote !($(getBid(qt)) < $(x) < $(getAsk(qt)))"
# # end

# # TODO: test
# using CmdUtil
# function checkTradeLegs(legs)
#     check = entryFilterLeg()
#     for leg in legs
#         check(leg)
#     end
#     # legsDb = queryLegsEntered(today())
#     # for leg in legs, legDb in legsDb
#     #     isConflict(leg, legDb) && @logerr "Cannot trade opposite side on same day" leg legDb
#     # end
#     return true
# end

# function closeTinyLegs!(legs, tid, optQuoter, maxClose::Int, sid::Side.T, exp::Date; pre)
#     cnt = 0
#     for leg in filter(l->getSide(l) == sid, legs)
#         cnt < maxClose || break
#         lid = getId(leg)
#         # We're no longer closing worthless future legs. TODO: add test for this case
#         if !isLegClosed(tid, lid, pre) || (getSide(leg) == Side.long && getExpir(leg) > exp)
#             q = optQuoter(leg)
#             qty = getQuantity(leg)
#             # @info "closeTinyLegs" getBid(q) getAsk(q) abs(getBid(q)/qty)
#             if abs(getBid(q)/qty) < 0.02 && abs(getAsk(q)/qty) < 0.02
#                 res = closeLegMarket(tid, leg, pre)
#                 # If an error occured, an exception is thrown. We can assume it succeeded here.
#                 @info "closeLegMarket live resp" res
#                 pre ? markLegClosedPre(lid) : markLegClosed(lid, res["id"], sid)
#                 deleteLeg!(legs, leg)
#                 cnt += 1
#             end
#         else
#             cnt += 1
#             deleteLeg!(legs, leg)
#         end
#     end
#     return cnt
# end

# isLegClosed(tid::Int, lid::Int, pre::Bool) = pre ? lid in PreClosedLegs : ST.getLegStatus(tid, lid) === Closed

# using ProcOrder
# markLegClosedPre(lid::Int) = push!(PreClosedLegs, lid)
# function markLegClosed(lid::Int, oid::Int, side::Side.T)::Bool
#     for i in 1:10 # check for about 5 seconds
#         tord = tradierOrder(oid)
#         status = tord["status"]
#         if status == "filled"
#             procOrder(tord)
#             # @assert isLegClosed(lid, false) "Leg close order completed but leg not closed"
#             return true
#         elseif status in ("open", "partially_filled", "pending")
#             i === 5 && (@info "Waiting for market close leg order processing")
#             sleep(0.5)
#         else
#             error("Leg close order error "*lid*", "*oid*", "*status)
#         end
#     end
#     side == Side.long || error("Short small leg not closed, cannot continue $(lid) $(oid) $(side)")
#     return false
#     # TODO: warn if not found and handle longs that don't fill
# end

# # TODO: cleanup
# function deleteLeg!(arr::Vector, o)
#     ff = findfirst(x -> x === o, arr)
#     if isnothing(ff)
#         @error o.raw["id"] (x->x["id"]).(arr)
#         throw("not found for delete!")
#     else
#         deleteat!(arr, ff)
#     end
# end
# #endregion

    # println("Current price:$(Pricing.price(lms1)) is $(dist) away from target neto:$(neto)")

    # println("Dist: $(dist) (price:$(Pricing.price(lms1)) - mineto:$(mineto))")

    # lmshortind = findfirst(isShort, lms1)
    # lmshort = lms1[lmshortind]

    # _, lmcheapind = findmax(lm -> isLong(lm) ? Pricing.price(Action.open, lm) : -Inf, lms1)
    # lmcheap = lms1[lmcheapind]
    # lmsrest = lms1[1:3 .!= lmcheapind]
    # p1 = P(Pricing.price(Action.open, lmcheap))
    # dist1 = P(dist / 3)

    # # at current price:$(p1) buying the cheapest:$(lmcheapind) $(getOption(lms1[lmcheapind]))")
    # println("#### Step 1: buy cheapest")
    # # bid2 = sum(getBid.(lmsrest))
    # # ask2 = sum(getAsk.(lmsrest))
    # # rat2 = (neto2 - bid2) / (ask2 - bid2)
    # p1_tgt = p1
    # p1_min = p1 - dist1
    # mineto23 = mineto - p1_min
    # p23 = Pricing.price(lmsrest)
    # dist23d = dist - dist1
    # dist23pm = p23 - mineto23

    # println("If we get it at worst:$(p1_worst) that would leave $(neto2) to open remaining 2 with current price2:$(p2) at rat2:$(rat2)")

#endregion

end