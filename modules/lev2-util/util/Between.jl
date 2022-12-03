module Between
using Dates
using SH, BaseTypes, SmallTypes, QuoteTypes, OptionMetaTypes, StratTypes, LegMetaTypes, RetTypes, StatusTypes, ChainTypes
using Rets, LegTypes, TradeTypes, LegTradeTypes, Pricing

#region ToRet
SH.to(::Type{Ret}, leg::Leg, curp::Currency, neto::Currency)::Ret = makeRet(leg, neto, curp)
# Can't do this because need one neto per leg: SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:Leg}}, legs, curp::Currency, neto::Currency)::Ret = combineRets(tos(Ret, legs, curp, neto))

SH.to(::Type{Ret}, lm::LegMetaOpen, curp::Currency)::Ret = to(Ret, getLeg(lm), curp, bap(lm))
SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegMetaOpen}}, lms, curp::Currency)::Ret = combineRets(map(lm -> to(Ret, lm, curp), lms)) # combineTo(Ret, tos(Leg, lms), curp, bap(lms))
# isempty(lms) ? Ret(curp) : combineRets(tos(Ret, lms, curp))
# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegMeta}}, lms, forDate::Date, curp::Currency, vtyRatio::Float64=1.0)::Ret = combineRets(tos(Ret, lms, forDate, curp, vtyRatio))

SH.to(::Type{Ret}, leg::LegTrade, curp::Currency)::Ret = to(Ret, getLeg(leg), curp, getNetOpen(leg))
SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegTrade}}, legs, curp::Currency)::Ret = combineRets(map(leg -> to(Ret, getLeg(leg), curp, getNetOpen(leg)), legs))
# combineTo(Ret, map(getLeg, legs), curp, getNetOpen(legs))

SH.to(::Type{Ret}, trade::Trade, curp::Currency)::Ret = combineTo(Ret, getLegs(trade), curp)
# combineRets(tos(Ret, getLegs(trade), combineTo(Ret, tos(LegMeta, getLegs(trade)), curp)
SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:Trade}}, trades, sp::Currency)::Ret = combineRets(tos(Ret, trades, sp))
#endregion

# SH.to(::Type{LegMeta}, leg::Leg, oqter)::LegMeta = ( (oq, side) = (oqter(leg), getSide(leg)) ; LegMeta(Leg(getOption(leg), getQuantity(leg), side), getQuote(oq, side), getMeta(oq)) )
# SH.to(::Type{LegMeta}, oq::OptionQuote, side::Side.T) = LegMeta(Leg(getOption(oq), 1.0, side), getQuote(oq, side), getMeta(oq))

# SH.to(::Type{Ret}, lg::Leg, forDate::Date, sp::Currency, vtyRatio::Float64) = to(Ret, to(LegMeta, lg), forDate, sp, vtyRatio)
# SH.to(::Type{Ret}, lm::LegMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = makeRet(getLeg(lm), getOptionMeta(lm), bap(lm), forDate, sp, vtyRatio)
# SH.to(::Type{LegRet}, lm::LegMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::LegRet = (lm, to(Ret, lm, forDate, sp, vtyRatio))


# SH.combineTo(::Type{Ret}, legs::Coll{Leg}, oqter, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, tos(LegMeta, legs, oqter), forDate, sp, vtyRatio))

# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegRet}}, lrs::Coll{LegRet})::Ret = combineRets(map(to(Ret), lrs))
# SH.combineTo(::Type{Vals}, ::Type{<:ElType{<:LegRet}}, lrs::Coll{LegRet})::Vals = combineRetVals(tos(Ret, lrs))

#region Trades
# SH.to(::Type{LegMetaOpen}, lg::LegTrade)::LegMeta = LegMetaOpen(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(getPrillDirOpen(lg)), getOptionMeta(lg))

# SH.combineTo(::Type{Ret}, trades::AVec{<:Trade}, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineTo(Ret, combineTo(Vector{LegMetaOpen}, trades), forDate, sp, vtyRatio)

# SH.to(::Type{Vector{LegMeta}}, trade::Trade)::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, [trade])) # TODO: cleanup, don't reuse wrongly
# SH.combineTo(::Type{Vector{LegMetaOpen}}, ::Type{<:ElType{<:Trade}}, trades)::Vector{LegMetaOpen} = collect(mapFlattenTo(getLegs, LegMetaOpen, trades))
#endregion

# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegTrade}}, legs, forDate::Date, curp::Currency, vtyRatio::Float64=1.0)::Ret = combineRets(tos(Ret, legs, forDate, curp, vtyRatio))
# function SH.to(::Type{Ret}, leg::LegTrade, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret
#     @assert getExpiration(leg) == forDate
#     makeRet(getLeg(leg), NaN, getNetOpen(leg), forDate, sp, vtyRatio)
# end



# SH.to(::Type{LegMeta}, lg::Leg, qt::Quote, met::OptionMeta)::LegMeta = ( side = getSide(side) ; LegMeta(Leg(getOption(lg), getQuantity(lg), side), qt, met) )
# SH.to(::Type{LegMeta}, lg::Leg, qt::OptionQuote)::LegMeta = ( side = getSide(side) ; LegMeta(Leg(getOption(lg), getQuantity(lg), side), qt, met) )

# SH.to(::Type{Ret}, lm::LegMeta, sp::Currency)::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), getExpiration(lm), sp, 1.0)

# SH.to(::Type{Ret}, lm::LegMeta, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), (forDate, sp, vtyRatio))
# SH.to(::Type{Ret}, lg::Leg, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = to(Ret, to(LegMeta, lg), (forDate, sp, vtyRatio))

# using Globals, Expirations, Markets
# SH.to(::Type{Ret}, lg::Leg, sp::Currency=market().startPrice) = toRet(toLegMeta(lg), expir(1), sp, Globals.get(:vtyRatio))

# SH.combineTo(::Type{Ret}, lms::Coll{LegMeta}, forDate::Date, sp::Currency, vtyRatio::Float64=1.0)::Ret = combineRets(tos(Ret, lms, forDate, sp, vtyRatio))
# SH.combineTo(::Type{Ret}, lms::Coll{LegMeta}, sp::Currency)::Ret = isempty(lms) ? Ret(sp) : combineRets(tos(Ret, lms, sp))

# SH.combineTo(::Type{Ret}, legs::Coll{Leg,4}, oqter, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, tos(LegMeta, legs, oqter), forDate, sp, vtyRatio))

# (SH.to(::Type{LegMeta}, lg::T)::LegMeta) where T<:LegTrade = LegMeta(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(Action.open, getNetOpen(lg)), OptionMeta(getIv(lg)))

# SH.tos(::Type{Vector{LegMeta}}, trades::AVec{<:Trade})::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))
# SH.tos(::Type{Vector{Vector{LegMeta}}}, trades::AVec{<:Trade})::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))

# SH.combineTo(::Type{Vector{LegMeta}}, ::Type{<:ElType{<:Trade}}, trades)::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))

end