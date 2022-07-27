module ProcRe
using ProcOrder
using SH, BaseTypes, SmallTypes, StatusTypes, OrderTypes
using FileUtil, StoreUtil, LogUtil
using StoreTrade, StoreOrder

export reproc

function reproc()
    StoreUtil.dropAll()
    StoreUtil.useDbProd(;init=true)
    ordFiles = sort(filter(fn -> endswith(fn, ".json"), readdir(dirData("bak/orders"); join=true)))
    for of in ordFiles
        tord = loadJson(of)
        status = tord["status"]
        status in ("pending", "open", "partially_filled", "expired", "canceled", "rejected", "error") && continue
        ord = to(Order, tord)
        if getClass(ord) == OrderClass.multileg && getAction(getLegs(ord)[1]) == Action.open
            reprocOrderOpen(ord)
        end
    end
    matchOrders2()
    for of in ordFiles
        tord = loadJson(of)
        status = tord["status"]
        status in ("pending", "open", "partially_filled", "expired", "canceled", "rejected", "error") && continue
        if !doesExistOrd(tord["id"])
            ord = to(Order, tord)
            if !(getClass(ord) == OrderClass.multileg && getAction(getLegs(ord)[1]) == Action.open)
                procOrder2(ord)
            end
        end
    end
    procExpired()
end

reprocOrderOpen(::Order{<:Union{Canceled,Rejected}}) = return # do nothing for these
function reprocOrderOpen(ord::Order{Accepted})
    leg1 = getLegs(ord)[1]
    if getOrderType(ord) == OrderType.market && getSide(leg1) == Side.short && getAction(leg1) == Action.close
        # Probably a market order for worthless expiring option that auto canceled after the day
    else
        @error "reprocOrder: Accepted order needs special handling" getId(ord)
    end
end
function reprocOrderOpen(ord::Order{Filled}) where S
    @log debug "Reproc trade from order" getId(ord)
    startTradeFromOrder(ord)
end

function startTradeFromOrder(ord::Order{Filled})
    legs = map(lmFromLegOrd, getLegs(ord))
    inTransaction() do
        newTrade(getPrimitDir(ord), legs, C(0.0), C(0.0))
        storeOrder(ord)
    end
end

using QuoteTypes, OptionMetaTypes, LegMetaTypes
lmFromLegOrd(leg::LegOrder) = LegMeta(getLeg(leg), Quote(Action.open, 0.0, 0.0), OptionMeta(0.0))

# TODO: clean up this mess, these two *2 functions, was done in a rush
function matchOrders2(;us=false)
    # NOTE: The order by in the query assumes lid correlates to time (always increasing)
    updateCnt = 0
    while !isempty((res = select("select * from VFindMatchesUnused order by lid asc limit 1") ; res))
        row = res[1]
        try
            update("insert into LegUsed (lid, olid, act, quantity) values (?, ?, ?, ?)", row.lid, row.olid, row.act, row.quantity)
        catch e
            @error "error inserting into LegUsed" row
            rethrow(e)
        end
        updateCnt += 1
    end
    if updateCnt > 0 || us
        # update("update Trade set status = vt.status from VTrade vt where vt.tid=? and Trade.tid=?", tid, tid)
        rows = select("select tid from VTrade where status != 'Starting'")
        for row in rows
            tid = row.tid
            update("update Trade set status = vt.status from VTrade vt where vt.tid=? and Trade.tid=?", tid, tid)
            # update("update Trade set status = vt.status from VTrade vt where Trade.tid=vt.tid")
        end
    end
end

function procOrder2(ord::Order{Filled})::Bool
    @log debug "procOrder: storing order" ord
    storeOrder(ord)
    matchOrders2()
    return true # TODO: ???
end

end