module ProcOrder
using Dates
using SH, Globals, BaseTypes, SmallTypes, OrderTypes, StatusTypes
using BaseUtil, LogUtil, StoreUtil, TradierOrderConvert, ThreadUtil
using Store, StoreTrade, StoreOrder, Backups, OrderWatching, TradierAccount

export procOrders, procOrder, matchOrders

function procOrders()::Bool
    isnothing(snap()) || error("Do not procOrders when snapped")
    tords = tradierOrders()
    @log debug "procOrders" length(tords)
    length(tords) > 0 || return false
    res = false
    for tord in tords
        res |= procOrder(tord)
    end
    Store.dbChecks()
    return res
end

# https://documentation.tradier.com/brokerage-api/reference/response/orders
# Order states: open, partially_filled, filled, expired, canceled, pending, rejected, error
# Both scheduled, and live order processing can enter here, so synchronize.
# TODO: scheduled will probably run in a separate process, so we might need a different technique for locking.
reprocOrder(oid::Int)::Bool = procOrder(loadOrderLogged(oid))
procOrder(tord::Dict{String,Any})::Bool = runSync(procOrderRaw, LOCK, tord)
function procOrderRaw(tord::Dict{String,Any})::Bool
    isnothing(snap()) || error("Do not procOrders when snapped")
    logOrderRaw(tord)
    if tord["class"] == "combo"
        @error "TODO: Combo order still not handled, skipping"
        return false
    end
    status = tord["status"]
    @log debug "procOrder: proc status" status
    status == "error" && error("Order with error status ", tord)
    status in ("pending", "open", "partially_filled") && return true

    oid = tord["id"]
    if doesExistOrd(oid)
        @log debug "procOrder: Skipping already exists" oid
        return false
    end

    tid = extractTid(tord)
    if status in ("expired", "canceled", "rejected")
        # TODO: Need to do something different when we support > 4 legged trades
        tnumLegs = queryNumLegs(tid)
        isnothing(tnumLegs) && (@log debug "procOrder: Skipping already deleted trade" oid tid ; return false)
        act = TradierOrderConvert.tierActionOrder(tord)
        onumLegs = TradierOrderConvert.tierNumLegs(tord)
        if act == 1 && (onumLegs == tnumLegs || onumLegs == tnumLegs - 1)
            StoreTrade.deleteTrade(tid)
            return true
        else
            @log error "Canceled order for trade doesn't match" oid tid act onumLegs tnumLegs
            return false
        end
    elseif status != "filled"
        @log error "procOrder: unknown status" status tord
    end

    ord = to(Order, tord)
    procOrder(tid, ord)
    return true
end

function matchOrders(tid::Int)
    # NOTE: The order by in the query assumes lid correlates to time (always increasing)
    updateCnt = 0
    while !isempty((res = select("select * from VFindMatchesUnused where tid=? order by lid asc limit 1", tid) ; res))
        row = res[1]
        try
            update("insert into LegUsed (lid, olid, act, quantity) values (?, ?, ?, ?)", row.lid, row.olid, row.act, row.quantity)
        catch e
            @error "error inserting into LegUsed" row
            rethrow(e)
        end
        updateCnt += 1
    end
    if updateCnt > 0
        update("update Trade set status = vt.status from VTrade vt where vt.tid=? and Trade.tid=?", tid, tid)
    end
end

#region Local
const LOCK = ReentrantLock()

procOrder(tid::Int, ::Order{<:Union{Canceled,Rejected}}) = return false # do nothing for these
function procOrder(tid::Int, ord::Order{Accepted})::Bool
    leg1 = getLegs(ord)[1]
    if getOrderType(ord) == OrderType.market && getSide(leg1) == Side.short && getAction(leg1) == Action.close
        @log debug "procOrder: Found sell to close market order still open" getId(ord)
        # Probably a market order for worthless expiring option that auto canceled after the day
        # TODO: if after expiration, process it as an expired/fill: or just do nothing and let batch processor handle it
    else
        @log debug "procOrder: Found Accepted order, still live and/or wait for batch?" getId(ord)
        # do nothing? These might be live orders, but could check if we're after hours
        # @error "procOrder: Accepted order needs special handling" getId(ord)
    end
    return false
end
function procOrder(tid::Int, ord::Order{Filled})::Bool
    @log debug "procOrder: storing order" ord
    storeOrder(ord)
    matchOrders(tid)
    return true # TODO: ???
end

# intended to be manually called
# example: tid: 70, lidAssigned: 274(452@4-22), lidCombo: 276(457@4-25)
function procAssigned(tid::Int, lidAssigned::Int, lidCombo::Int, tord::Dict{String,Any})
    isnothing(snap()) || error("Do not procOrders when snapped")
    leg1 = loadLegTrade(lidAssigned)
    leg2 = loadLegTrade(lidCombo)
    newOrder = TradierOrderConvert.fromAssigned(tord, leg1, leg2)
    return newOrder
    storeOrder(newOrder)
    matchOrders(tid)
end

function extractTid(tord::Dict{String,Any})::Int
    if !haskey(tord, "tag")
        # TODO: This happens when enter order via web site
        @error "No tag for order" tord
    end
    tag = tord["tag"]
    parse(Int, SubString(tag, 3)) # pull off the initial op/cl
end

const PERIOD_UPDATE = Second(444)
const JOB_NAME = "proc-orders"
using Sched, DataHelper
function startWatching()
    Sched.add(JOB_NAME, @__MODULE__, "updateProcOrders", "whenUpdate", true; repok=true)
end
function updateProcOrders()
    @log debug "update $(JOB_NAME)"
    procOrders() || Sched.remove(JOB_NAME)
end
whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = whenMarket(from, isMktOpen, nextMktChange, PERIOD_UPDATE)
#endregion

end