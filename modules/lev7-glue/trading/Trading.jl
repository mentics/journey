module Trading
using Dates
using SH, Globals, DateUtil
using BaseTypes, SmallTypes, OptionTypes, StatusTypes, TradeTypes, LegTradeTypes
using StoreTrade
using TradierOrder, TradierAccount
using LegTradeInfo, TradeInfo # TODO: needed? getting calcQuote
using Positions # TODO: just being used to grab prices for TradeMeta, and verify positions before close

export price, submitPreview, submitLive, closeTrade, closeLeg

#=
TODO: To handle > 4 legs, we'll need to find the optimal combination of 2 or 4 legged parts. So, loop through the combinations, calculate their stats, and choose the best.
=#

# submitPreview(lgs, primitDir) = (submitOrder(toPayloadOpen(lgs, PriceT(primitDir); pre=true)) ; return 0)
function submitPreview(legs, primitDir)
    checkTradeLegs(legs)
    payload = toPayloadOpen(0, legs, PriceT(primitDir); pre=true)
    @info "submitPreview" payload
    resp = submitOrder(payload)
    @info "submitPreview" resp
    return 0
end

function submitLive(legs, primitDir, mktqt)
    checkTradeLegs(legs)
    length(legs) > 4 && error("Trades with > 4 legs not supported yet")
    tid = newTrade(primitDir, legs, getBid(mktqt), getAsk(mktqt))
    Globals.set(:tidOpenLast, tid) # for override
    payload = toPayloadOpen(tid, legs, PriceT(primitDir); pre=false)
    @info "submitLive submitOrder payload" payload
    resp = submitOrder(payload)
    @info "submitLive submitOrder response" resp tid
    ProcOrder.startWatching()
    return tid
end

function closeTrade(optQuoter, trade::Trade{<:Closeable}, primitDir::PriceT; pre=true, legsInd=nothing)::Union{Nothing,Dict{String,Any}}
    inLegs = getLegs(trade)
    inLegs = isnothing(legsInd) ? inLegs : inLegs[legsInd]
    veri = verifyPoss(inLegs)
    checkTradeLegs(legs)
    if !intest() && false in veri # TODO: enhance test to set positions properly to test this part?
        @warn "closeTrade: missing positions for legs" veri
        useLegs = map(first, Iterators.filter(t -> t[2], zip(inLegs, veri)))
    else
        useLegs = copy(inLegs)
    end
    tid = getId(trade)
    exp = getTargetDate(trade)
    # TODO: consider something else because not closing future calendar longs
    # Close legs which are worthless
    shortCount = closeTinyLegs!(useLegs, tid, optQuoter, 4, Side.short, exp; pre)
    if (shortCount > 0)
        closeTinyLegs!(useLegs, tid, optQuoter, shortCount, Side.long, exp; pre)
    end

    if abs(primitDir) <= 0.01
        @warn "Resulting order has price <= 0.01 so not submitting. Suggest use closeLegMarket on remaining legs" filter(l->!isLegClosed(getId(l),pre), useLegs)
        return nothing
    else
        @info "closing legs" useLegs
        payload = length(useLegs) == 1 ? toPayloadCloseSingle(tid, useLegs[1], primitDir; pre) : toPayloadClose(tid, useLegs, primitDir; pre)
        result = submitOrder(payload)
        !pre && ProcOrder.startWatching()
        return result
    end
end

function closeLeg(leg::LegTrade, primitDir::PriceT; pre=true, isMkt=false)::Dict{String,Any}
    verifyPos(leg) || error("Position not found for closeLeg")
    @info "Closing leg" leg
    payload = toPayloadCloseSingle(getTid(leg), leg, primitDir; pre, isMkt)
    result = submitOrder(payload)
    !pre && ProcOrder.startWatching()
    return result
end

#region Local
const PreClosedLegs = Set{Int}()

# function confirmInside(trad, qt)
#     x = sum(quot.(optQuote.(getLegs(trad)[1]))) # TODO: should this go requote the leg?
#     !(getBid(qt) <= x <= getAsk(qt)) && @warn "price is not inside current quote !($(getBid(qt)) < $(x) < $(getAsk(qt)))"
# end

# TODO: test
function checkTradeLegs(legs)
    legsDb = queryLegsEntered(today())
    for leg in legs, legDb in legsDb
        isConflict(leg, legDb) && @logerr "Cannot trade opposite side on same day" leg legDb
    end
    return true
end

function closeTinyLegs!(legs, tid, optQuoter, maxClose::Int, sid::Side.T, exp::Date; pre)
    cnt = 0
    for leg in filter(l->getSide(l) == sid, legs)
        cnt < maxClose || break
        lid = getId(leg)
        # We're no longer closing worthless future legs. TODO: add test for this case
        if !isLegClosed(lid, pre) || (getSide(leg) == Side.long && getExpiration(leg) > exp)
            q = optQuoter(leg)
            qty = getQuantity(leg)
            # @info "closeTinyLegs" getBid(q) getAsk(q) abs(getBid(q)/qty)
            if abs(getBid(q)/qty) < 0.02 && abs(getAsk(q)/qty) < 0.02
                res = closeLegMarket(tid, leg, pre)
                # If an error occured, an exception is thrown. We can assume it succeeded here.
                @info "closeLegMarket live resp" res
                pre ? markLegClosedPre(lid) : markLegClosed(lid, res["id"], sid)
                deleteLeg!(legs, leg)
                cnt += 1
            end
        else
            cnt += 1
            deleteLeg!(legs, leg)
        end
    end
    return cnt
end

isLegClosed(lid::Int, pre::Bool) = pre ? lid in PreClosedLegs : queryLegStatus(lid) === Closed

using ProcOrder
markLegClosedPre(lid::Int) = push!(PreClosedLegs, lid)
function markLegClosed(lid::Int, oid::Int, side::Side.T)::Bool
    for i in 1:10 # check for about 5 seconds
        tord = tradierOrder(oid)
        status = tord["status"]
        if status == "filled"
            procOrder(tord)
            @assert isLegClosed(lid, false) "Leg close order completed but leg not closed"
            return true
        elseif status in ("open", "partially_filled", "pending")
            i === 5 && (@info "Waiting for market close leg order processing")
            sleep(0.5)
        else
            error("Leg close order error "*lid*", "*oid*", "*status)
        end
    end
    side == Side.long || error("Short small leg not closed, cannot continue $(lid) $(oid) $(side)")
    return false
    # TODO: warn if not found and handle longs that don't fill
end

# TODO: cleanup
function deleteLeg!(arr::Vector, o)
    ff = findfirst(x -> x === o, arr)
    if isnothing(ff)
        @error o.raw["id"] (x->x["id"]).(arr)
        throw("not found for delete!")
    else
        deleteat!(arr, ff)
    end
end
#endregion

end