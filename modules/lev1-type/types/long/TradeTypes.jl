module TradeTypes
using Dates
using SH, BaseTypes, LegTradeTypes, StatusTypes

export Trade, TradeMeta, getTargetDate

struct TradeMeta
    bid::Currency
    ask::Currency
end

struct Trade{S}
    id::Int
    targetDate::Date
    primitDir::PriceT
    prillDirOpen::Union{Nothing,Currency}
    prillDirClose::Union{Nothing,Currency}
    legs::Vector{LegTrade}
    tsCreated::Int
    tsFilled::Union{Nothing,Int}
    tsClosed::Union{Nothing,Int}
    meta::TradeMeta
end
Trade(id::Int, ::Nothing, primitDir::PriceT, rargs...) = Trade{Starting}(id, minimum(getExpiration.(rargs[3])), primitDir, rargs...)
SH.getId(o::Trade) = o.id
getTargetDate(o::Trade) = o.targetDate # minimum(getExpiration.(getLegs(o)))
SH.getPrimitDir(o::Trade) = o.primitDir
SH.getPrillDirOpen(o::Trade) = o.prillDirOpen
SH.getPrillDirClose(o::Trade) = o.prillDirClose
SH.getLegs(o::Trade) = o.legs
SH.tsCreated(o::Trade) = o.tsCreated
SH.tsFilled(o::Trade) = o.tsFilled
SH.tsClosed(o::Trade) = o.tsClosed
SH.getNetOpen(o::Trade) = isnothing(o.prillDirOpen) ? NaN : ( s = sum(getNetOpen, o.legs) ; @assert s ≈ o.prillDirOpen ; s )
SH.getNetClose(o::Trade) = isnothing(o.prillDirClose) ? NaN : ( s = sum(getNetClose, o.legs) ; @assert s ≈ o.prillDirClose ; s )
SH.getPnl(o::Trade) = getPrillDirOpen(o) + getPrillDirClose(o)
SH.getMeta(o::Trade) = o.meta

end