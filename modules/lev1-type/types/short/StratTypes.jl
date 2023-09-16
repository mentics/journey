module StratTypes
using SH, BaseTypes, RetTypes, LegQuoteTypes

export LegRet, AllLegs4, Spread, Condor, Spreads2, Combi, Strat, getRets
export isCal, isCalShort, isCalLong, isVert, isSideSame, isStyleDiff

# const LegRet = Tuple{LegQuote,Ret}
const AllLegs4 = NTuple{4,Vector{LegRet}}
const Spread = Tuple{LegRet,LegRet}
const Condor = Tuple{Spread,Spread}
const Spreads2 = Tuple{Vector{Spread}, Vector{Spread}} # all call spreads and all put spreads
const Combi = NTuple{4,LegRet}
const Strat = Combi

# SH.to(::Type{LegQuote}, s::Strat) = Tuple(lr[1] for lr in s)
# getRets(s::Strat)::NTuple{4,Ret} = map(x->x[2], s)
# getRets(lrs) = map(x->x[2], lrs)
SH.to(::Type{LegQuote}, lr::LegRet)::LegQuote = first(lr)
SH.to(::Type{Ret}, lr::LegRet)::Ret = lr[2]

# SH.getValMid(lrs) = sum(getValMid, getRets(lrs))
# SH.getNetOpen(s::Strat) = sum(getNetOpen, tos(LegQuote, s)) # mapreduce(getNetOpen, +, tos(LegQuote, s))
SH.getLeg(lr::LegRet) = getLeg(lr[1])

using SmallTypes
isCal((lr1, lr2)) = getExpir(lr1) != getExpir(lr2)
isCalShort((lr1, lr2)) =
    (getExpir(lr1) > getExpir(lr2) && getSide(lr1) == Side.short) ||
    (getExpir(lr2) > getExpir(lr1) && getSide(lr2) == Side.short)
isCalLong((lr1, lr2)) =
    (getExpir(lr1) > getExpir(lr2) && getSide(lr1) == Side.long) ||
    (getExpir(lr2) > getExpir(lr1) && getSide(lr2) == Side.long)
isVert((lr1, lr2)) = getExpir(lr1) == getExpir(lr2)
isSideSame((lr1, lr2)) = getSide(lr1) == getSide(lr2)
isStyleDiff((lr1, lr2)) = getStyle(lr1) != getStyle(lr2)

SH.getStyle(spr::Spread) = ( s1 = getStyle(spr[1]) ; s2 = getStyle(spr[2])  ; s1 == s2 ? (return s1) : error("Spread styles don't match ", spr) )
SH.getSide(spr::Combi) = ( s1 = getSide(spr[1]) ; s2 = getSide(spr[4]) ; s1 == s2 ? (return s1) : error("Spread sides don't match ", spr) )
# SmallTypes.isCall(c::Combi) = isnothing(findfirst(isPut, map(getStyle, c)))
# SmallTypes.isPut(c::Combi) = isnothing(findfirst(isCall, map(getStyle, c)))
# SmallTypes.isLong(c::Combi) = getSide(c[1]) == Side.long && getSide(c[4]) == Side.long
# SmallTypes.isShort(c::Combi) = getSide(c[1]) == Side.short && getSide(c[4]) == Side.short

SH.getStrike(lr::LegRet) = getStrike(first(lr))

SH.getSide(lr::LegRet) = getSide(first(lr))
SH.getStyle(lr::LegRet) = getStyle(first(lr))
SH.getOption(lr::LegRet) = getOption(first(lr))
SH.getExpir(lr::LegRet) = getExpir(first(lr))

end