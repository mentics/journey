module OrderTypes
using EnumX, Dates
using SH, BaseTypes, SmallTypes, StatusTypes, LegTypes, StatusTypes

export Order
export LegOrder
export OrderType, OrderClass

# from: https://documentation.tradier.com/brokerage-api/reference/response/orders
@enumx OrderType market=1 debit credit even limit stop stop_limit other
@enumx OrderClass equity=1 option combo multileg oto

struct LegOrder
    id::Int
    status::Type{<:Status}
    action::Action.T
    prillDir::Union{Nothing,Currency}
    leg::Leg
    tsCreated::DateTime
    tsFilled::Union{Nothing,DateTime}
end
LegOrder(row::NamedTuple) = LegOrder(row.olid, Filled, Action.T(row.act), C(Float64(row.prilldir)), Leg(row), row.tscreated, row.tsfilled)
SH.getId(o::LegOrder) = o.id
SH.getAction(o::LegOrder) = o.action
SH.getStyle(o::LegOrder) = getStyle(o.leg)
SH.getExpir(o::LegOrder) = getExpir(o.leg)
SH.getStrike(o::LegOrder) = getStrike(o.leg)
SH.getSide(o::LegOrder) = getSide(o.leg)
SH.getQuantity(o::LegOrder) = getQuantity(o.leg)
SH.getPrillDir(o::LegOrder) = o.prillDir
SH.tsCreated(o::LegOrder) = o.tsCreated
SH.tsFilled(o::LegOrder) = o.tsFilled
SH.getLeg(o::LegOrder) = o.leg

struct Order{S}
    id::Int
    sym::String
    class::OrderClass.T
    orderType::OrderType.T
    primitDir::Union{Nothing,PriceT}
    prillDir::Union{Nothing,Currency}
    legs::Vector{LegOrder}
    tsCreated::DateTime
    tsFilled::Union{Nothing,DateTime}
end
SH.getId(o::Order) = o.id
SH.getSymbol(o::Order) = o.sym
SH.getClass(o::Order) = o.class
SH.getOrderType(o::Order) = o.orderType
SH.getPrimitDir(o::Order) = o.primitDir
SH.getPrillDir(o::Order) = o.prillDir
SH.getLegs(o::Order) = o.legs
SH.tsCreated(o::Order) = o.tsCreated
SH.tsFilled(o::Order) = o.tsFilled
# SH.isDeleted(o::Order{T}) where T <: st.Deleted = true
# SH.isDeleted(o::Order) = false
# isDone(o::Order{T}) where T <: st.OrderDone = true
# isDone(o::Order) = false
SH.isStatus(o::Order{T}, status) where T = T <: status

end