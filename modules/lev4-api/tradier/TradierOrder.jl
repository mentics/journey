module TradierOrder
using SH, BaseTypes, SmallTypes, DateUtil, TradierConfig, TradierBase, TradierUtil, PayloadUtil

export submitOrder # , cancelOrder, modifyOrder
export toPayloadOpen, toPayloadClose, toPayloadCloseSingle, closeLegMarket

function submitOrder(payload::String)
    res = tradierPost("/accounts/$(getAccountId())/orders", payload, Call(nameof(var"#self#")))
    if !haskey(res, "order")
        error("No order key in submitOrder response ", res)
    else
        return res["order"]
    end
end

# function cancelOrder(orderId::Int, accountId::AbstractString=config.accountId)
#     config.orderListener[] = OrderEventCancelPre(Events.cancel, orderId)
#     result = tradierDelete(config, "/accounts/$(accountId)/orders/$(orderId)")
#     if haskey(result, "errors")
#         config.orderListener[] = OrderEventCancelFailed(Events.cancelFailure, orderId, result)
#     else
#         config.orderListener[] = OrderEventCancelled(Events.cancel, orderId, result)
#     end
#     return result
# end

# function modifyOrder(config::TradierConfig, accountId::AbstractString, orderId::Int, newPrice::Currency)
#     config.orderListener[] = OrderEvent(Events.modify, orderId)
#     payload = "price=$(s(newPrice))"
#     @info "Modify payload: $(payload)"
#     result = tradierPut(config, "/accounts/$(accountId)/orders/$(orderId)", payload)
#     if haskey(result, "errors")
#         config.orderListener[] = OrderEvent(Events.modifyFailure, orderId)
#     end
#     return result
# end

# legToPayload(symUnder::AbstractString, i::Int, act, leg) = legToPayload(i, occSymbol(getOption(leg), symUnder), act, getSide(leg), getQuantity(leg))
function legToPayload(i::Int, sym, act, sid, qty)
    tradSide = tier.toTierSide(act, sid)
    return "option_symbol[$(i)]=$(sym)&side[$(i)]=$(tradSide)&quantity[$(i)]=$(round(Int, qty))"
end

function toPayloadOpen(tid::Int, inLegs, primitDir::PriceT; pre=true)
    legs = deepcopy(collect(inLegs)) # TODO: messy
    mergeLegs!(legs)
    legsStr = join([legToPayload(i-1, tier.optToOcc(getOption(leg)), Action.open, getSide(leg), getQuantity(leg)) for (i, leg) in enumerate(legs)], '&')
    payload = "tag=op$(tid)&class=multileg&symbol=$(getDefaultSymbol())&type=$(tier.orderType(primitDir))&duration=day&price=$(abs(primitDir))&$(legsStr)"
    if pre
        payload *= "&preview=true"
    end
    return payload
end

function toPayloadClose(tid, inLegs, primitDir::PriceT; pre=true)
    legs = deepcopy(collect(inLegs)) # TODO: messy
    mergeLegs!(legs)
    # TODO: if it goes to length 1, but the primitdir I entered was for all legs, so might need to divide by qty
    length(legs) == 1 && return toPayloadCloseSingle(tid, legs[1], primitDir; pre)
    ordType = tier.orderType(primitDir)
    primit = abs(primitDir)
    legsStr = join([legToPayload(i-1, tier.optToOcc(getOption(leg)), Action.close, getSide(leg), getQuantity(leg)) for (i, leg) in enumerate(legs)], '&')
    payload = "tag=cl$(tid)&class=multileg&symbol=$(getDefaultSymbol())&type=$(ordType)&duration=day&price=$(primit)&$(legsStr)"
    if pre
        payload *= "&preview=true"
    end
    return payload
end

function toPayloadCloseSingle(tid::Int, leg, primitDir::PriceT; pre=true, isMkt=false)
    ordType = isMkt ? "market" : "limit"
    tierSide = tier.toTierSide(Action.close, getSide(leg))
    # TODO: remove primitDir if market?
    payload = "tag=cl$(tid)&class=option&symbol=$(getDefaultSymbol())&option_symbol=$(tier.optToOcc(getOption(leg)))&side=$(tierSide)&quantity=$(getQuantity(leg))&type=$(ordType)&duration=day&price=$(abs(primitDir))$(pre ? "&preview=true" : "")"
    # occursin("preview", payload) || error("only preview right now")
    return payload
end

function closeLegMarket(tid::Int, leg, pre=true)
    side = getSide(leg)
    primitDir = PriceT(Int(side) * 0.01)
    payload = toPayloadCloseSingle(tid, leg, primitDir; pre, isMkt=(side == Side.long))
    return submitOrder(payload)
end

end