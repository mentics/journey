module Between
using Dates
using SH, BaseTypes, SmallTypes, QuoteTypes, OptionMetaTypes, StratTypes, LegMetaTypes, RetTypes, StatusTypes
using Rets, LegTypes, TradeTypes, LegTradeTypes

# SH.to(::Type{LegMeta}, lg::Leg, qt::Quote, met::OptionMeta)::LegMeta = ( side = getSide(side) ; LegMeta(Leg(getOption(lg), getQuantity(lg), side), qt, met) )
# SH.to(::Type{LegMeta}, lg::Leg, qt::OptionQuote)::LegMeta = ( side = getSide(side) ; LegMeta(Leg(getOption(lg), getQuantity(lg), side), qt, met) )
SH.to(::Type{LegMeta}, leg::Leg, oqter)::LegMeta = ( (oq, side) = (oqter(leg), getSide(leg)) ; LegMeta(Leg(getOption(leg), getQuantity(leg), side), getQuote(oq, side), getMeta(oq)) )

using ChainTypes # TODO: fix when move bap?
SH.to(::Type{Ret}, lm::LegMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), forDate, sp, vtyRatio)
SH.to(::Type{Ret}, lg::Leg, forDate::Date, sp::Currency, vtyRatio::Float64) = to(Ret, to(LegMeta, lg), forDate, sp, vtyRatio)
# SH.to(::Type{Ret}, lm::LegMeta, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), (forDate, sp, vtyRatio))
# SH.to(::Type{Ret}, lg::Leg, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = to(Ret, to(LegMeta, lg), (forDate, sp, vtyRatio))
SH.to(::Type{LegRet}, lm::LegMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::LegRet = (lm, to(Ret, lm, forDate, sp, vtyRatio))

# using Globals, Expirations, Markets
# SH.to(::Type{Ret}, lg::Leg, sp::Currency=market().startPrice) = toRet(toLegMeta(lg), expir(1), sp, Globals.get(:vtyRatio))

SH.combineTo(::Type{Ret}, lms::AVec{LegMeta}, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, lms, forDate, sp, vtyRatio))
# SH.combineTo(::Type{Ret}, lms::Coll{LegMeta,4})::Ret = combineRets(tos(Ret, lms))

#region Trades
# TODO: convert directly to these objects (or related) from db?
# SH.toLegMetas(trads) = reduce(vcat, toLegMeta.(getLegs(trad)) for trad in trads; init=Vector{LegMeta}())

SH.to(::Type{LegMeta}, lg::LegTrade)::LegMeta = LegMeta(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(Action.open, getPrillDirOpen(lg)), OptionMeta(getIv(lg)))
# (SH.to(::Type{LegMeta}, lg::T)::LegMeta) where T<:LegTrade = LegMeta(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(Action.open, getNetOpen(lg)), OptionMeta(getIv(lg)))

# TODO: don't do collect, figure out dispatch for iterators
SH.combineTo(::Type{Ret}, trades::AVec{<:Trade}, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineTo(Ret, collect(mapFlattenTo(getLegs, LegMeta, trades)), forDate, sp, vtyRatio)
# SH.combineTo(::Type{Ret}, trade::Trade, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineTo(Ret, getLegs(trade), forDate, sp, vtyRatio)
SH.tos(::Type{Vector{LegMeta}}, trades::AVec{<:Trade})::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))
#endregion

end