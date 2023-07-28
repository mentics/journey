module Between
using Dates
using SH, BaseTypes, SmallTypes, QuoteTypes, OptionMetaTypes, StratTypes, LegMetaTypes, RetTypes, StatusTypes, ChainTypes
using CollUtil, ChainUtil
using Rets, LegTypes, TradeTypes, LegTradeTypes, Pricing

#region Lines
import LinesLeg:LinesLeg as LL,Segments
# (LL.toSegments(lms::NTuple{N,LegMeta})::Segments{N}) where N = LL.toSegments(lms, map(P ∘ Pricing.price, lms))
(LL.toSegments(legs::NTuple{N,LegLike})::Segments{N}) where N = LL.toSegments(legs, map(P ∘ Pricing.price, legs))
(LL.toSegments(legs::NTuple{N,LegLike}, adjustprices::Real)::Segments{N}) where N = LL.toSegments(legs, map(leg -> P(Pricing.price(leg) + adjustprices), legs)) # P ∘ Pricing.price
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
(LL.toSegments(legs::NTuple{N,LegTrade})::Segments{N}) where N = LL.toSegments(legs, map(P ∘ getNetOpen, legs))

LL.toSegmentsWithZeros(lms::NTuple{N,LegMeta}) where N = LL.toSegmentsWithZeros(lms, map(P ∘ Pricing.price, lms))
LL.toSections(lms::NTuple{N,LegMeta}) where N = LL.toSections(lms, map(P ∘ Pricing.price, lms))
SH.toDraw(lms::NTuple{N,LegMeta}; mn=100.0, mx=600.0) where N = collect(LL.toLineTuples(LL.toSegments(lms); mn, mx)) # collect because Makie can't handle tuples of coords
# SH.toDraw(lms::NTuple{N,LegMeta}) where N = collect(LL.toLineTuples(LL.toSegmentsWithZeros(lms))) # collect because Makie can't handle tuples of coords
# SH.toDraw(lms::NTuple{N,LegMeta}) where N = collect(LL.toLineTuples(LL.toSegmentsWithZeros(lms))) # collect because Makie can't handle tuples of coords
#end

#region Prob
import DateUtil, Markets, ProbKde, HistData
using ProbTypes
import LineTypes:SegmentsWithZeros
import ProbUtil
makeprob(hasexpir, curp::Real)::ProbWithMin = makeprob(getExpir(hasexpir), curp)
makeprob(xpir::Date, curp::Real)::ProbWithMin = toProbWithMin(ProbKde.probToClose(F(curp), F(Markets.market().vix), now(UTC), xpir))
makeprob(ts::DateTime, hasexpir, curp::Real)::ProbWithMin = makeprob(ts, getExpir(hasexpir), curp)
makeprob(ts::DateTime, xpir::Date, curp::Real)::ProbWithMin = toProbWithMin(ProbKde.probToClose(F(curp), F(HistData.vixOpen(DateUtil.lastTradingDay(ts))), ts, xpir))
calcprobprofit(prob, lms) = calcprobprofit(prob, LL.toSegmentsWithZeros(lms))
function calcprobprofit(prob, segsz::SegmentsWithZeros)
    p = 0.0
    s1 = first(segsz)
    if s1.left.y > 0 || s1.slope < 0
        p += ProbUtil.cdf(prob, s1.left.x)
        # println("added1 ", (; s1, p))
    end

    lasts = s1
    for s in segsz
        if s.left.y > 0 || s.right.y > 0
            p += ProbUtil.cdf(prob, s.right.x) - ProbUtil.cdf(prob, s.left.x)
            # println("added2 ", (; s, p))
        end
        lasts = s
    end

    s = lasts
    if s.right.y > 0 || s.slope > 0
        p += 1 - ProbUtil.cdf(prob, s.right.x)
        # println("added3 ", (; s, p))
    end

    return p
end
#endregion

#region ToLegMeta
SH.to(::Type{LegMetaOpen}, oq, side) = LegMetaOpen(oq, side)
SH.to(::Type{LegMetaOpen}, leg::Leg, lup) = LegMetaOpen(lup(getOption(leg)), getSide(leg), getQuantity(leg))
SH.to(::Type{LegMetaOpen}, lm::LegMetaOpen, otoq) = ( leg = getLeg(lm) ; LegMetaOpen(leg, ChainUtil.oToOq(otoq, getOption(leg))) )
SH.to(::Type{LegMetaOpen}, lm::LegMetaOpen, fotoq::Function) = ( leg = getLeg(lm) ; LegMetaOpen(leg, fotoq(getOption(leg))) )
SH.to(::Type{LegMetaClose}, lm::Union{LegMetaOpen,LegTrade}, otoq) = ( leg = getLeg(lm) ; LegMetaClose(leg, ChainUtil.oToOq(otoq, getOption(leg))) )
SH.to(::Type{LegMetaClose}, lm::Union{LegMetaOpen,LegTrade}, fotoq::Function) = ( leg = getLeg(lm) ; LegMetaClose(leg, fotoq(getOption(lm))) )
#endregion

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

# export requote
function reqlm(lup, leg::Leg, act::Action.T)
    oq = lup(leg)
    return act == Action.open ? LegMetaOpen(leg, oq) : LegMetaClose(leg, oq)
end
function reqlms(lup, legs::Coll{Leg}, act::Action.T)
    map(x -> reqlm(lup, x, act), legs)
end
function reqlms(lup, legs::Coll, act::Action.T)
    map(x -> reqlm(lup, getLeg(x), act), legs)
end
function reqlms(lup, hasLegs, act::Action.T)
    map(x -> reqlm(lup, getLeg(x), act), getLegs(hasLegs))
end


# SH.to(::Type{LegMeta}, leg::Leg, oqter)::LegMeta = ( (oq, side) = (oqter(leg), getSide(leg)) ; LegMeta(Leg(getOption(leg), getQuantity(leg), side), getQuote(oq, side), getMeta(oq)) )
# SH.to(::Type{LegMeta}, oq::OptionQuote, side::Side.T) = LegMeta(Leg(getOption(oq), 1.0, side), getQuote(oq, side), getMeta(oq))

# SH.to(::Type{Ret}, lg::Leg, forDate::Date, sp::Currency, vtyRatio::Float64) = to(Ret, to(LegMeta, lg), forDate, sp, vtyRatio)
# SH.to(::Type{Ret}, lm::LegMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = makeRet(getLeg(lm), getOptionMeta(lm), bap(lm), forDate, sp, vtyRatio)
# SH.to(::Type{LegRet}, lm::LegMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::LegRet = (lm, to(Ret, lm, forDate, sp, vtyRatio))


# SH.combineTo(::Type{Ret}, legs::Coll{Leg}, oqter, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, tos(LegMeta, legs, oqter), forDate, sp, vtyRatio))

# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegRet}}, lrs::Coll{LegRet})::Ret = combineRets(map(to(Ret), lrs))
# SH.combineTo(::Type{Vals}, ::Type{<:ElType{<:LegRet}}, lrs::Coll{LegRet})::Vals = combineRetVals(tos(Ret, lrs))

#region Trades

# SH.combineTo(::Type{Ret}, trades::AVec{<:Trade}, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineTo(Ret, combineTo(Vector{LegMetaOpen}, trades), forDate, sp, vtyRatio)

# SH.to(::Type{LegMetaOpen}, lg::LegTrade)::LegMeta = LegMetaOpen(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(getPrillDirOpen(lg)), getOptionMeta(lg))
legTradeToLMO(lg) = LegMetaOpen(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(getPrillDirOpen(lg)), getOptionMeta(lg))

# SH.to(::Type{Vector{LegMeta}}, trade::Trade)::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, [trade])) # TODO: cleanup, don't reuse wrongly
# SH.combineTo(::Type{Vector{LegMetaOpen}}, ::Type{<:ElType{<:Trade}}, trades)::Vector{LegMetaOpen} = collect(mapFlattenTo(getLegs, LegMetaOpen, trades))
tradesToLMOs(trades) = collect(mapflatmap(getLegs, legTradeToLMO, trades))
#endregion

# SH.combineTo(::Type{Ret}, ::Type{<:ElType{<:LegTrade}}, legs, forDate::Date, curp::Currency, vtyRatio::Float64=1.0)::Ret = combineRets(tos(Ret, legs, forDate, curp, vtyRatio))
# function SH.to(::Type{Ret}, leg::LegTrade, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret
#     @assert getExpir(leg) == forDate
#     makeRet(getLeg(leg), NaN, getNetOpen(leg), forDate, sp, vtyRatio)
# end



# SH.to(::Type{LegMeta}, lg::Leg, qt::Quote, met::OptionMeta)::LegMeta = ( side = getSide(side) ; LegMeta(Leg(getOption(lg), getQuantity(lg), side), qt, met) )
# SH.to(::Type{LegMeta}, lg::Leg, qt::OptionQuote)::LegMeta = ( side = getSide(side) ; LegMeta(Leg(getOption(lg), getQuantity(lg), side), qt, met) )

# SH.to(::Type{Ret}, lm::LegMeta, sp::Currency)::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), getExpir(lm), sp, 1.0)

# SH.to(::Type{Ret}, lm::LegMeta, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = makeRet(getLeg(lm), getMeta(lm), bap(lm), (forDate, sp, vtyRatio))
# SH.to(::Type{Ret}, lg::Leg, (forDate, sp, vtyRatio)::Tuple{Date,Currency,Float64})::Ret = to(Ret, to(LegMeta, lg), (forDate, sp, vtyRatio))

# using Globals, Expirations, Markets
# SH.to(::Type{Ret}, lg::Leg, sp::Currency=market().startPrice) = toRet(toLegMeta(lg), expir(1), sp, Globals.get(:vtyRatio))

# TODO: do this differently?
SH.combineTo(::Type{Ret}, lms::Coll{LegMeta}, forDate::Date, sp::Currency, vtyRatio::Float64=1.0)::Ret = combineRets(tos(Ret, lms, forDate, sp, vtyRatio))
SH.combineTo(::Type{Ret}, lms::Coll{LegMeta}, sp::Currency)::Ret = isempty(lms) ? Ret(sp) : combineRets(tos(Ret, lms, sp))
SH.combineTo(::Type{Ret}, legs::Coll{Leg,4}, oqter, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = combineRets(tos(Ret, tos(LegMeta, legs, oqter), forDate, sp, vtyRatio))

# (SH.to(::Type{LegMeta}, lg::T)::LegMeta) where T<:LegTrade = LegMeta(Leg(getOption(lg), getQuantity(lg), getSide(lg)), Quote(Action.open, getNetOpen(lg)), OptionMeta(getIv(lg)))

# SH.tos(::Type{Vector{LegMeta}}, trades::AVec{<:Trade})::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))
# SH.tos(::Type{Vector{Vector{LegMeta}}}, trades::AVec{<:Trade})::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))

# SH.combineTo(::Type{Vector{LegMeta}}, ::Type{<:ElType{<:Trade}}, trades)::Vector{LegMeta} = collect(mapFlattenTo(getLegs, LegMeta, trades))

end