module Between
using Dates
using SH, BaseTypes, SmallTypes, QuoteTypes, OptionMetaTypes, StratTypes, LegMetaTypes, RetTypes, StatusTypes
using Rets, LegTypes, TradeTypes, LegTradeTypes

# SH.to(::Type{LegMeta}, lg::Leg, qt::Quote, met::OptionMeta)::LegMeta = ( side = getSide(side) ; LegMeta(Leg(getOption(lg), getQuantity(lg), side), qt, met) )
# SH.to(::Type{LegMeta}, lg::Leg, qt::OptionQuote)::LegMeta = ( side = getSide(side) ; LegMeta(Leg(getOption(lg), getQuantity(lg), side), qt, met) )
SH.to(::Type{LegMeta}, leg::Leg, oqter)::LegMeta = ( (oq, side) = (oqter(leg), getSide(leg)) ; LegMeta(Leg(getOption(leg), getQuantity(leg), side), getQuote(oq, side), getMeta(oq)) )
SH.to(::Type{LegMeta}, oq, side) = LegMeta(Leg(getOption(oq), 1.0, side), getQuote(oq, side), getMeta(oq))

SH.to(::Type{Ret}, lm::LegMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), forDate, sp, vtyRatio)
# SH.to(::Type{Ret}, lm::LegMeta, sp::Currency)::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), getExpiration(lm), sp, 1.0)
SH.to(::Type{Ret}, lm::LegMeta, sp::Currency)::Ret = makeRet(getLeg(lm), bap(lm), sp)
SH.to(::Type{Ret}, lg::Leg, forDate::Date, sp::Currency, vtyRatio::Float64) = to(Ret, to(LegMeta, lg), forDate, sp, vtyRatio)
# SH.to(::Type{Ret}, lm::LegMeta, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), (forDate, sp, vtyRatio))
# SH.to(::Type{Ret}, lg::Leg, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = to(Ret, to(LegMeta, lg), (forDate, sp, vtyRatio))
SH.to(::Type{LegRet}, lm::LegMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::LegRet = (lm, to(Ret, lm, forDate, sp, vtyRatio))

# using Globals, Expirations, Markets
# SH.to(::Type{Ret}, lg::Leg, sp::Currency=market().startPrice) = toRet(toLegMeta(lg), expir(1), sp, Globals.get(:vtyRatio))

# SH.combineTo(::Type{Ret}, lms::AVec{LegMeta}, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, lms, forDate, sp, vtyRatio))
SH.combineTo(::Type{Ret}, lms::Coll{LegMeta}, forDate::Date, sp::Currency, vtyRatio::Float64=1.0)::Ret = combineRets(tos(Ret, lms, forDate, sp, vtyRatio))
SH.combineTo(::Type{Ret}, lms::Coll{LegMeta}, sp::Currency)::Ret = isempty(lms) ? Ret(sp) : combineRets(tos(Ret, lms, sp))

SH.combineTo(::Type{Ret}, legs::Coll{Leg}, oqter, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, tos(LegMeta, legs, oqter), forDate, sp, vtyRatio))
# SH.combineTo(::Type{Ret}, legs::Coll{Leg,4}, oqter, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, tos(LegMeta, legs, oqter), forDate, sp, vtyRatio))

#region Trades
# TODO: convert directly to these objects (or related) from db?
# SH.toLegMetas(trads) = reduce(vcat, toLegMeta.(getLegs(trad)) for trad in trads; init=Vector{LegMeta}())

SH.to(::Type{LegMeta}, lg::LegTrade)::LegMeta = LegMeta(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(Action.open, getPrillDirOpen(lg)), getOptionMeta(lg))
# (SH.to(::Type{LegMeta}, lg::T)::LegMeta) where T<:LegTrade = LegMeta(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(Action.open, getNetOpen(lg)), OptionMeta(getIv(lg)))

# TODO: don't do collect, figure out dispatch for iterators
# collect(mapFlattenTo(getLegs, LegMeta, trades))
SH.combineTo(::Type{Ret}, trades::AVec{<:Trade}, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineTo(Ret, combineTo(Vector{LegMeta}, trades), forDate, sp, vtyRatio)
SH.combineTo(::Type{Ret}, trades::AVec{<:Trade}, sp::Currency)::Ret = combineTo(Ret, combineTo(Vector{LegMeta}, trades), sp)
# SH.combineTo(::Type{Ret}, trade::Trade, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineTo(Ret, getLegs(trade), forDate, sp, vtyRatio)
# SH.tos(::Type{Vector{LegMeta}}, trades::AVec{<:Trade})::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))
# SH.tos(::Type{Vector{Vector{LegMeta}}}, trades::AVec{<:Trade})::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))
SH.to(::Type{Vector{LegMeta}}, trade::Trade)::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, [trade])) # TODO: cleanup, don't reuse wrongly
SH.combineTo(::Type{Vector{LegMeta}}, trades::AVec{<:Trade})::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))
#endregion

end