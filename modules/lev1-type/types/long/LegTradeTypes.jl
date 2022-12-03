module LegTradeTypes
using Dates
using SH, BaseTypes, StatusTypes, LegTypes, OptionMetaTypes

export LegTrade, LegTradeMeta, getTid

struct LegTradeMeta
    bid::Currency
    ask::Currency
    iv::Float64
end
SH.getBid(ltm::LegTradeMeta) = ltm.bid
SH.getAsk(ltm::LegTradeMeta) = ltm.ask
SH.getIv(ltm::LegTradeMeta) = ltm.iv
# TODO: store all the greeks
SH.getOptionMeta(ltm::LegTradeMeta) = OptionMeta(;mid_iv=ltm.iv)

struct LegTrade
    id::Int
    tid::Int
    status::Type{<:Status}
    prillDirOpen::Union{Nothing,Currency}
    prillDirClose::Union{Nothing,Currency}
    leg::Leg
    tsCreated::DateTime
    tsFilled::Union{Nothing,DateTime}
    tsClosed::Union{Nothing,DateTime}
    meta::LegTradeMeta
end
LegTrade(lt::LegTrade; leg=lt.leg) =
    LegTrade(lt.id, lt.tid, lt.status, lt.prillDirOpen, lt.prillDirClose, leg, lt.tsCreated, lt.tsFilled, lt.tsClosed, lt.meta)
SH.getId(o::LegTrade) = o.id
getTid(o::LegTrade) = o.tid
SH.getLeg(o::LegTrade) = o.leg
SH.getOption(o::LegTrade) = getOption(o.leg)
SH.getStyle(o::LegTrade) = getStyle(o.leg)
SH.getExpiration(o::LegTrade) = getExpiration(o.leg)
SH.getStrike(o::LegTrade) = getStrike(o.leg)
SH.getSide(o::LegTrade) = getSide(o.leg)
SH.getQuantity(o::LegTrade) = getQuantity(o.leg)
SH.tsCreated(o::LegTrade) = o.tsCreated
SH.tsFilled(o::LegTrade) = o.tsFilled
SH.tsClosed(o::LegTrade) = o.tsClosed
SH.getIv(o::LegTrade) = getIv(o.meta)
SH.getPrillDirOpen(o::LegTrade) = o.prillDirOpen
SH.getPrillDirClose(o::LegTrade) = o.prillDirClose
SH.getMeta(o::LegTrade) = o.meta
SH.getNetOpen(o::LegTrade)::Currency = isnothing(o.prillDirOpen) ? missing : getQuantity(o) * o.prillDirOpen
SH.getNetClose(o::LegTrade) = isnothing(o.prillDirClose) ? missing : getQuantity(o) * o.prillDirClose
# TODO: store all the greeks
SH.getOptionMeta(lt::LegTrade) = getOptionMeta(lt.meta)

# TODO: whatever uses this, change the code to follow example in Store.findUnknownPositions
SH.addQuantity(lt::LegTrade, addend::Real) = LegTrade(lt; leg=Leg(lt.leg; quantity=(getQuantity(lt.leg) + addend)))

end