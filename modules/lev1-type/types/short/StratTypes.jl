module StratTypes
using SH, BaseTypes, RetTypes, LegMetaTypes

export LegRet, AllLegs4, Spread, Spreads2, Combi, Strat, getRets
export isCal, isCalShort, isCalLong, isVert, isSideSame, isStyleDiff

const LegRet = Tuple{LegMeta,Ret}
const AllLegs4 = NTuple{4,Vector{LegRet}}
const Spread = Tuple{LegRet,LegRet}
const Spreads2 = Tuple{Vector{Spread}, Vector{Spread}} # all call spreads and all put spreads
const Combi = NTuple{4,LegRet}
const Strat = Combi

# SH.to(::Type{LegMeta}, s::Strat) = Tuple(lr[1] for lr in s)
# getRets(s::Strat)::NTuple{4,Ret} = map(x->x[2], s)
# getRets(lrs) = map(x->x[2], lrs)
SH.to(::Type{LegMeta}, lr::LegRet)::LegMeta = first(lr)
SH.to(::Type{Ret}, lr::LegRet)::Ret = lr[2]

# SH.getValMid(lrs) = sum(getValMid, getRets(lrs))
SH.getNetOpen(s::Strat) = mapreduce(getNetOpen, +, tos(LegMeta, s))
SH.getLeg(lr::LegRet) = getLeg(lr[1])

using SmallTypes
isCal((lr1, lr2)) = getExpiration(lr1) != getExpiration(lr2)
isCalShort((lr1, lr2)) =
    (getExpiration(lr1) > getExpiration(lr2) && getSide(lr1) == Side.short) ||
    (getExpiration(lr2) > getExpiration(lr1) && getSide(lr2) == Side.short)
isCalLong((lr1, lr2)) =
    (getExpiration(lr1) > getExpiration(lr2) && getSide(lr1) == Side.long) ||
    (getExpiration(lr2) > getExpiration(lr1) && getSide(lr2) == Side.long)
isVert((lr1, lr2)) = getExpiration(lr1) == getExpiration(lr2)
isSideSame((lr1, lr2)) = getSide(lr1) == getSide(lr2)
isStyleDiff((lr1, lr2)) = getStyle(lr1) != getStyle(lr2)

end