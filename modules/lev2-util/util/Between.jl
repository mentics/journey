module Between
using Dates
using SH, BaseTypes, SmallTypes, QuoteTypes, LegQuoteTypes, ChainTypes
using CollUtil, ChainUtil
using LegTypes, TradeTypes, LegTradeTypes, Pricing
import ProbMultiKde as pmk
# StratTypes, RetTypes, StatusTypes, OptionMetaTypes

#region Lines
import LinesLeg:LinesLeg as LL,Segments
# TODO: add a Pricing for Trades
# (LL.toSegments(legs::Coll{N,T})) where {N,T<:LegLike} = LL.toSegments(legs, map(P ∘ Pricing.price_open, legs))
(LL.toSegments(legs::CollT{T})) where {T<:LegLike} = LL.toSegments(legs, map(P ∘ Pricing.price_open, legs))
# (LL.toSegments(legs::NTuple{N,LegTrade})::Segments{N}) where N = LL.toSegments(legs, map(P ∘ getNetOpen, legs))

# function toSegmentsN(legs::NTuple{N,LegLike}) where N
#     netos = map(P ∘ Pricing.price_open, legs) # map(leg -> P(Pricing.price(Action.open, leg)), legs)
#     segs = LL.toSegments(legs, netos)
#     return (;segs, netos)
# end
# function LL.toSegments(legs::NTuple{N,LegLike}, adjustprices::Real) where N
#     netos = map(leg -> P(Pricing.price(Action.open, leg) + adjustprices), legs)
#     segs = LL.toSegments(legs, netos)
#     return (;segs, netos)
# end

# function LL.toSegments(legs::NTuple{N,LegLike}, netos)::Segments{N} where N
#     # netos = map(P ∘ Pricing.price, lms)
#     # # TODO: remove validation after checking
#     # for i in eachindex(lms)
#     #     q = getQuote(lms[i])
#     #     @assert q.bid < netos[i] < q.ask
#     # end
#     segs = LL.toSegments(legs, netos)
#     return segs
# end

LL.toSegmentsWithZeros(lms::NTuple{N,LegQuote}) where N = LL.toSegmentsWithZeros(lms, map(P ∘ Pricing.price, lms))
LL.toSections(lms::NTuple{N,LegQuote}) where N = LL.toSections(lms, map(P ∘ Pricing.price, lms))
# SH.toDraw(lms::NTuple{N,LegLike}; mn=100.0, mx=600.0) where N = collect(LL.toLineTuples(LL.toSegments(lms); mn, mx)) # collect because Makie can't handle tuples of coords
# SH.toDraw(lms::Vector{<:LegLike}; kws...) = SH.toDraw(Tuple(lms); kws...)
SH.toDraw(lms::CollT{<:LegLike}; mn=100.0, mx=600.0) = collect(LL.toLineTuples(LL.toSegments(lms); mn, mx)) # collect because Makie can't handle tuples of coords
SH.toDraw(lms::CollT{<:LegLike}, netos; mn=100.0, mx=600.0) = collect(LL.toLineTuples(LL.toSegments(lms, netos); mn, mx)) # collect because Makie can't handle tuples of coords
# SH.toDraw(lms::NTuple{N,LegQuote}) where N = collect(LL.toLineTuples(LL.toSegmentsWithZeros(lms))) # collect because Makie can't handle tuples of coords
# SH.toDraw(lms::NTuple{N,LegQuote}) where N = collect(LL.toLineTuples(LL.toSegmentsWithZeros(lms))) # collect because Makie can't handle tuples of coords
#end

#region Prob
# import DateUtil, Markets, ProbKde, HistData
# using ProbTypes
import LineTypes:SegmentsWithZeros
# import ProbUtil
# makeprob(hasexpir)::ProbWithMin = makeprob(getExpir(hasexpir), Markets.market().curp)
# makeprob(hasexpir, curp::Real)::ProbWithMin = makeprob(getExpir(hasexpir), curp)
# makeprob(xpir::Date, curp::Real)::ProbWithMin = toProbWithMin(ProbKde.probToClose(F(curp), F(Markets.market().vix), now(UTC), xpir))
# makeprob(ts::DateTime, hasexpir, curp::Real)::ProbWithMin = makeprob(ts, getExpir(hasexpir), curp)
# makeprob(ts::DateTime, xpir::Date, curp::Real)::ProbWithMin = toProbWithMin(ProbKde.probToClose(F(curp), F(HistData.vixOpen(DateUtil.lastTradingDay(ts))), ts, xpir))

# makeprob2(hasexpir) = makeprob2(hasexpir, Markets.market().curp)
# makeprob2(hasexpir, curp::Real) = makeprob2(getExpir(hasexpir), curp)
# makeprob2(xpir::Date, curp::Real) = ( prob = ProbKde.probToClose(F(curp), F(Markets.market().vix), now(UTC), xpir) ; (;prob, probwithmin=toProbWithMin(prob)) )
# # makeprob2(ts::DateTime, hasexpir, curp::Real) = makeprob(ts, getExpir(hasexpir), curp)
# # makeprob2(ts::DateTime, xpir::Date, curp::Real) = toProbWithMin(ProbKde.probToClose(F(curp), F(HistData.vixOpen(DateUtil.lastTradingDay(ts))), ts, xpir))

calcprobprofit(prob, lms) = calcprobprofit(prob, LL.toSegmentsWithZeros(lms))
calcprobprofit(prob, segs::Segments) = calcprobprofit(prob, LL.toSegmentsWithZeros(segs))
function calcprobprofit(prob, segsz::SegmentsWithZeros)
    p = 0.0
    s1 = first(segsz)
    if s1.left.y > 0 || s1.slope < 0
        p += pmk.cdf(prob, s1.left.x)
        # println("added1 ", (; s1, p))
    end

    lasts = s1
    for s in segsz
        if s.left.y > 0 || s.right.y > 0
            p += pmk.cdf(prob, s.right.x) - pmk.cdf(prob, s.left.x)
            # println("added2 ", (; s, p))
        end
        lasts = s
    end

    s = lasts
    if s.right.y > 0 || s.slope > 0
        p += 1 - pmk.cdf(prob, s.right.x)
        # println("added3 ", (; s, p))
    end

    return p
end
#endregion

#region ToLegQuote
import Chains
function requote(action::Action.T, legs; lup=Chains.chainLookup, pricer=Pricing.price)
    println("Using pricer: $(pricer)")
    isop = action == Action.open
    lmsnew = isop ? tos(LegQuoteOpen, legs, lup) : tos(LegQuoteClose, legs, lup)
    println("Requote price: $(isop ? string(pricer(action, legs)) : "closing") -> $(pricer(action, lmsnew))")
    return lmsnew
end
SH.to(::Type{LegQuoteOpen}, oq, side) = LegQuoteOpen(oq, side)
SH.to(::Type{LegQuoteOpen}, leg::Leg, lup) = LegQuoteOpen(lup(getOption(leg)), getSide(leg), getQuantity(leg))
SH.to(::Type{LegQuoteOpen}, lm::LegQuoteOpen, otoq) = ( leg = getLeg(lm) ; LegQuoteOpen(leg, ChainUtil.oToOq(otoq, getOption(leg))) )
SH.to(::Type{LegQuoteOpen}, lm::LegQuoteOpen, fotoq::Function) = ( leg = getLeg(lm) ; LegQuoteOpen(leg, fotoq(getOption(leg))) )
SH.to(::Type{LegQuoteClose}, lm::Union{LegQuoteOpen,LegTrade}, otoq) = ( leg = getLeg(lm) ; LegQuoteClose(leg, ChainUtil.oToOq(otoq, getOption(leg))) )
SH.to(::Type{LegQuoteClose}, lm::Union{LegQuoteOpen,LegTrade}, fotoq::Function) = ( leg = getLeg(lm) ; LegQuoteClose(leg, fotoq(getOption(lm))) )
#endregion

#region ToRet
# SH.to(::Type{Ret}, leg::Leg, curp::Currency, neto::Currency)::Ret = makeRet(leg, neto, curp)
# # Can't do this because need one neto per leg: SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:Leg}}, legs, curp::Currency, neto::Currency)::Ret = combineRets(tos(Ret, legs, curp, neto))

# SH.to(::Type{Ret}, lm::LegQuoteOpen, curp::Currency)::Ret = to(Ret, getLeg(lm), curp, bap(lm))
# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegQuoteOpen}}, lms, curp::Currency)::Ret = combineRets(map(lm -> to(Ret, lm, curp), lms)) # combineTo(Ret, tos(Leg, lms), curp, bap(lms))
# # isempty(lms) ? Ret(curp) : combineRets(tos(Ret, lms, curp))
# # SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegQuote}}, lms, forDate::Date, curp::Currency, vtyRatio::Float64=1.0)::Ret = combineRets(tos(Ret, lms, forDate, curp, vtyRatio))

# SH.to(::Type{Ret}, leg::LegTrade, curp::Currency)::Ret = to(Ret, getLeg(leg), curp, getNetOpen(leg))
# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegTrade}}, legs, curp::Currency)::Ret = combineRets(map(leg -> to(Ret, getLeg(leg), curp, getNetOpen(leg)), legs))
# # combineTo(Ret, map(getLeg, legs), curp, getNetOpen(legs))

# SH.to(::Type{Ret}, trade::Trade, curp::Currency)::Ret = combineTo(Ret, getLegs(trade), curp)
# # combineRets(tos(Ret, getLegs(trade), combineTo(Ret, tos(LegQuote, getLegs(trade)), curp)
# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:Trade}}, trades, sp::Currency)::Ret = combineRets(tos(Ret, trades, sp))

# # TODO: do this differently?
# SH.combineTo(::Type{Ret}, lms::Coll{LegQuote}, forDate::Date, sp::Currency, vtyRatio::Float64=1.0)::Ret = combineRets(tos(Ret, lms, forDate, sp, vtyRatio))
# SH.combineTo(::Type{Ret}, lms::Coll{LegQuote}, sp::Currency)::Ret = isempty(lms) ? Ret(sp) : combineRets(tos(Ret, lms, sp))
# SH.combineTo(::Type{Ret}, legs::Coll{Leg,4}, oqter, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, tos(LegQuote, legs, oqter), forDate, sp, vtyRatio))
#endregion

# export requote
function reqlm(lup, leg::Leg, act::Action.T)
    oq = lup(leg)
    return act == Action.open ? LegQuoteOpen(leg, oq) : LegQuoteClose(leg, oq)
end
function reqlms(lup, legs::CollT{Leg}, act::Action.T)
    map(x -> reqlm(lup, x, act), legs)
end
function reqlms(lup, legs::Coll, act::Action.T)
    map(x -> reqlm(lup, getLeg(x), act), legs)
end
function reqlms(lup, hasLegs, act::Action.T)
    map(x -> reqlm(lup, getLeg(x), act), getLegs(hasLegs))
end


# SH.to(::Type{LegQuote}, leg::Leg, oqter)::LegQuote = ( (oq, side) = (oqter(leg), getSide(leg)) ; LegQuote(Leg(getOption(leg), getQuantity(leg), side), getQuote(oq, side), getMeta(oq)) )
# SH.to(::Type{LegQuote}, oq::OptionQuote, side::Side.T) = LegQuote(Leg(getOption(oq), 1.0, side), getQuote(oq, side), getMeta(oq))

# SH.to(::Type{Ret}, lg::Leg, forDate::Date, sp::Currency, vtyRatio::Float64) = to(Ret, to(LegQuote, lg), forDate, sp, vtyRatio)
# SH.to(::Type{Ret}, lm::LegQuote, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = makeRet(getLeg(lm), getOptionMeta(lm), bap(lm), forDate, sp, vtyRatio)
# SH.to(::Type{LegRet}, lm::LegQuote, forDate::Date, sp::Currency, vtyRatio::Float64)::LegRet = (lm, to(Ret, lm, forDate, sp, vtyRatio))


# SH.combineTo(::Type{Ret}, legs::Coll{Leg}, oqter, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, tos(LegQuote, legs, oqter), forDate, sp, vtyRatio))

# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegRet}}, lrs::Coll{LegRet})::Ret = combineRets(map(to(Ret), lrs))
# SH.combineTo(::Type{Vals}, ::Type{<:ElType{<:LegRet}}, lrs::Coll{LegRet})::Vals = combineRetVals(tos(Ret, lrs))

#region Trades

# SH.combineTo(::Type{Ret}, trades::AVec{<:Trade}, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineTo(Ret, combineTo(Vector{LegQuoteOpen}, trades), forDate, sp, vtyRatio)

# SH.to(::Type{LegQuoteOpen}, lg::LegTrade)::LegQuote = LegQuoteOpen(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(getPrillDirOpen(lg)), getOptionMeta(lg))
legTradeToLMO(lg) = LegQuoteOpen(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(getPrillDirOpen(lg)), getOptionMeta(lg))

# SH.to(::Type{Vector{LegQuote}}, trade::Trade)::Vector{LegQuote} = collect(mapFlattenTo(getLegs, LegQuote, [trade])) # TODO: cleanup, don't reuse wrongly
# SH.combineTo(::Type{Vector{LegQuoteOpen}}, ::Type{<:ElType{<:Trade}}, trades)::Vector{LegQuoteOpen} = collect(mapFlattenTo(getLegs, LegQuoteOpen, trades))
tradesToLMOs(trades) = collect(mapflatmap(getLegs, legTradeToLMO, trades))
#endregion

# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegTrade}}, legs, forDate::Date, curp::Currency, vtyRatio::Float64=1.0)::Ret = combineRets(tos(Ret, legs, forDate, curp, vtyRatio))
# function SH.to(::Type{Ret}, leg::LegTrade, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret
#     @assert getExpir(leg) == forDate
#     makeRet(getLeg(leg), NaN, getNetOpen(leg), forDate, sp, vtyRatio)
# end



# SH.to(::Type{LegQuote}, lg::Leg, qt::Quote, met::OptionMeta)::LegQuote = ( side = getSide(side) ; LegQuote(Leg(getOption(lg), getQuantity(lg), side), qt, met) )
# SH.to(::Type{LegQuote}, lg::Leg, qt::OptionQuote)::LegQuote = ( side = getSide(side) ; LegQuote(Leg(getOption(lg), getQuantity(lg), side), qt, met) )

# SH.to(::Type{Ret}, lm::LegQuote, sp::Currency)::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), getExpir(lm), sp, 1.0)

# SH.to(::Type{Ret}, lm::LegQuote, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), (forDate, sp, vtyRatio))
# SH.to(::Type{Ret}, lg::Leg, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = to(Ret, to(LegQuote, lg), (forDate, sp, vtyRatio))

# using Globals, Expirations, Markets
# SH.to(::Type{Ret}, lg::Leg, sp::Currency=market().startPrice) = toRet(toLegQuote(lg), expir(1), sp, Globals.get(:vtyRatio))

# (SH.to(::Type{LegQuote}, lg::T)::LegQuote) where T<:LegTrade = LegQuote(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(Action.open, getNetOpen(lg)), OptionMeta(getIv(lg)))

# SH.tos(::Type{Vector{LegQuote}}, trades::AVec{<:Trade})::Vector{LegQuote} = collect(mapFlattenTo(getLegs, LegQuote, trades))
# SH.tos(::Type{Vector{Vector{LegQuote}}}, trades::AVec{<:Trade})::Vector{LegQuote} = collect(mapFlattenTo(getLegs, LegQuote, trades))

# SH.combineTo(::Type{Vector{LegQuote}}, ::Type{<:ElType{<:Trade}}, trades)::Vector{LegQuote} = collect(mapFlattenTo(getLegs, LegQuote, trades))

end