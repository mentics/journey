module StoreOrder
using Dates
using SH, OrderTypes
using LogUtil, StoreUtil

export storeOrder, doesExistOrd, queryLegOrders

function storeOrder(ord::Order{S})::Nothing where S
    @log debug "storeOrder"
    inTransaction() do
        update("insert into Ord (oid, symbol, class, orderType, status, primitDir, prillDir, tsCreated, tsFilled) values (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                getId(ord), getSymbol(ord), getClass(ord), getOrderType(ord), S, getPrimitDir(ord), getPrillDir(ord), tsCreated(ord), noth(tsFilled(ord)))
        storeLegOrder.(getId(ord), getLegs(ord))
    end
    return
end

function storeLegOrder(oid::Int, leg::LegOrder)::Nothing
    update("insert into LegOrd (olid, oid, act, style, expiration, strike, side, quantity, prillDir, tsCreated, tsFilled) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
           getId(leg), oid, getAction(leg),
           getStyle(leg), getExpiration(leg), getStrike(leg), getSide(leg), getQuantity(leg),
           getPrillDir(leg), tsCreated(leg), noth(tsFilled(leg)))
    return
end

# TODO: this doesn't handle timezone correctly because it converts to date in UTC and it should use market time
queryLegOrders(d::Date)::Vector{LegOrder} = LegOrder.(select("select * from LegOrd where cast(tsFilled as date) = ?", d))

doesExistOrd(oid) = !isempty(select("select oid from Ord where oid = ?", (oid)))

noth(x) = ismissing(x) ? nothing : x

end