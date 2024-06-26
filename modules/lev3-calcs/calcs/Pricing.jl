module Pricing
using SH, BaseTypes, SmallTypes, QuoteTypes, OptionQuoteTypes, OptionMetaTypes, LegTypes, LegQuoteTypes
import OptionUtil, CollUtil

# price(has)::Currency = price(getQuote(has))
# price(hass::Coll)::Currency = sum(price, hass)
# price(qt::Quote)::Currency = price(getBid(qt), getAsk(qt))

# priceOpp(has)::Currency = priceOpp(getQuote(has))
# priceOpp(hass::Coll)::Currency = sum(priceOpp, hass)
# priceOpp(qt::Quote)::Currency = price(-getAsk(qt), -getBid(qt))

#region Pricing
price_open(x) = price(Action.open, x)

function price(action::Action.T, legs::CollT{<:LegLike})
    psum = CZ
    for leg in legs
        p = price(action, leg)
        # !isnothing(p) || return nothing
        psum += p
    end
    return psum
end

function price(action::Action.T, leg::LegLike)
    closing = action == Action.close
    if (getSide(leg) == Side.long) ⊻ closing
        return price_long(leg, closing) # not multiplying by quantity because LegQuotes already have that in their quotes
    else
        return price_short(leg, closing)
    end
end

# function price(leg::LegQuoteOpen)
#     if getSide(leg) == Side.long
#         return price_long(leg)
#     else
#         return price_short(leg)
#     end
# end
# function price(leg::LegQuoteClose)
#     if getSide(leg) == Side.long
#         return price_short(leg) # opposite because closing
#     else
#         return price_long(leg)
#     end
# end

flip_quote(qt::Quote) = Quote(-getAsk(qt), -getBid(qt))
price_long_flip(qt, closing = false) = price_long(flip_quote(getQuote(qt)), closing)

price_long(has, closing::Bool = false) = (qt = getQuote(has) ; price_long(getBid(qt), getAsk(qt), closing) )
function price_long(bid::Currency, ask::Currency, closing::Bool = false) # ::Union{Nothing,Currency}
    if bid >= 0
        if closing
            return CZ
        else
            # println("WARN: Tried to price long for no bid ($bid, $ask)")
            throw(DomainError((bid, ask), "Tried to price long for no bid"))
            # return nothing
        end
    end
    return price_raw(bid, ask)
end

price_short(has, closing::Bool = false) = (qt = getQuote(has) ; price_short(getBid(qt), getAsk(qt), closing) )
function price_short(bid::Currency, ask::Currency, closing::Bool = false) # ::Union{Nothing,Currency}
    if bid <= 0
        if closing
            return CZ
        else
            # println("WARN: Tried to price short for no bid ($bid, $ask)")
            throw(DomainError((bid, ask), "Tried to price short for no bid"))
            # return nothing
        end
    end
    return price_raw(bid, ask)
end

function price_raw(bid::Currency, ask::Currency)::Currency
    if bid > ask
        # TODO: do something else?
        # println("WARN: Tried to price when bid > ask ", (;bid, ask))
        return min(bid, ask)
    end
    # if ask <= 0
    #     println("WARN: using bid price when ask $(ask) <= 0")
    #     return bid
    # end

    spread = ask - bid
    if spread <= 0.02
        return bid
    # elseif spread <= 0.4
    #     mult = round(Int, spread / 0.04, RoundUp)
    #     return bid + mult * 0.01
    elseif spread <= 0.5
        return bid + ceil(spread / 2 * 100)/100 - 0.01
    else
        return bid + 0.25
    end
    return max(p, C(0.01))
end
#endregion Pricing

#region CalcRisk
function calc_commit(lq::Union{LegQuote,Tuple{LegQuote}}, neto::PT)
    @assert neto < 0
    @assert getQuantity(lq) == 1.0
    return -neto
end
# function calc_commit(lqs::Coll{2,LegQuote}, neto::PT)
#     @assert getQuantity(lqs[1]) == 1.0 && getQuantity(lqs[2]) == 1.0
#     lq1 = lqs[1]
#     lq2 = lqs[2]
#     style1, side1, strike1 = getStyle(lq1), getSide(lq1), getStrike(lq1)
#     style2, side2, strike2 = getStyle(lq2), getSide(lq2), getStrike(lq2)
#     @assert style1 == style2
#     @assert side1 == side2
#     # if the quantity is different, it will just affect neto
#     return neto + (strike2 - strike1) * min(getQuantity(lq1), getQuantity(lq2))
# end
# function calc_commit(lqs::Coll{3,LegQuote}, neto::PT)
#     @assert count(isShort, lqs) == 1
#     # sides = getSide.(lqs)
#     ind_short = tup_find(getSide, Side.short, lqs)
#     ind_short == 3 && return neto + (getStrike(lqs[3]) - getStrike(lqs[2])) * minqty(lqs[3], lqs[2])
#     ind_short == 1 && return neto + (getStrike(lqs[2]) - getStrike(lqs[1])) * minqty(lqs[2], lqs[1])
#     width32 = getStrike(lqs[3]) - getStrike(lqs[2])
#     width21 = getStrike(lqs[2]) - getStrike(lqs[1])
#     return neto + min(width32, width21)
# end
# function calc_commit(lqs::Coll{4,LegQuote}, neto::PT)
#     return -neto
# end
@inline minqty(lq1, lq2) = min(getQuantity(lq1), getQuantity(lq2))

get_spread_width(lqs) = get_spread_width(tuple(lqs...))
get_spread_width(lqs::Tuple{<:LegLike}) = 0
get_spread_width(lqs::NTuple{2,<:LegLike}) = abs(getStrike(lqs[1]) - getStrike(lqs[2]))

function get_spread_width(lqs::NTuple{3,<:LegLike})
    ind_short = tup_find(getSide, Side.short, lqs)
    # @show ind_short
    ind_short == 3 && return getStrike(lqs[3]) - getStrike(lqs[2])
    ind_short == 1 && return getStrike(lqs[2]) - getStrike(lqs[1])
    width32 = getStrike(lqs[3]) - getStrike(lqs[2])
    width21 = getStrike(lqs[2]) - getStrike(lqs[1])
    return min(width32, width21)
end

# Assumes normal condor and not interleaved long/short
function get_spread_width(lqs::NTuple{4,<:LegLike})
    left = get_spread_width((lqs[1], lqs[2]))
    right = get_spread_width((lqs[3], lqs[4]))
    return max(left, right)
end
#endregion CalcRisk

function tup_find(f, y, tup::NTuple{3,T})::Int where T
    f(tup[1]) == y && return 1
    f(tup[2]) == y && return 2
    return 3
end
tup_sum((x1,x2)::NTuple{2,T}) where T = x1 + x2
tup_sum((x1,x2,x3)::NTuple{3,T}) where T = x1 + x2 + x3
tup_sum((x1,x2,x3,x4)::NTuple{4,T}) where T = x1 + x2 + x3 + x4

#region OldMarginSpreadStuff
# function priceSpread(hass::NTuple{4,T})::Currency where T
#     priceSpread(sumQuotes(getQuote.(hass)))
# end
# function priceSpread(qt::Quote)::Currency
#     b = getBid(qt)
#     a = getAsk(qt)
#     spread = a - b
#     if spread < 0.4
#         # mult = round(Int, spread / 0.04, RoundDown)
#         mult = unsafe_trunc(Int, spread / 0.04)
#         return b - a + 2 * mult * 0.01 # (b + mult * 0.01) + (-a + mult * 0.01)
#     else
#         return b - a + 0.2 # (b + 0.1) + (-a  + 0.1)
#     end
# end

# # priceSpread(has)::Currency = price(getQuote(has)) + priceOpp(getQuote(has))
# # priceSpread(hass::Coll)::Currency = sum(price, hass) + sum(priceOpp, hass)
# # function priceSpread(hass::NTuple{4,T})::Currency where T
# #     sum(price, hass) + sum(priceOpp, hass)
# # end

# bap(qt::Quote, r=.2)::Currency = improve(qt, r)
# bap(hasQuote, r=.2)::Currency = improve(getQuote(hasQuote), r)
# bap(hasQuotes::Coll, r=.2)::Currency = round(sum(bap.(getQuote.(hasQuotes), r)), RoundDown; digits=2)
# function (bap(hasQuotes::NTuple{3,T}, r=.2)::Currency) where T
#     s = bap(getQuote(hasQuotes[1]), r) +
#         bap(getQuote(hasQuotes[2]), r) +
#         bap(getQuote(hasQuotes[3]), r)
#     return round(s, RoundDown; digits=2)
# end

# bapFast(qt::Quote, r=.2)::Float64 = improveFast(qt, r)
# bapFast(hasQuote, r=.2)::Float64 = improveFast(getQuote(hasQuote), r)
# function (bapFast(hasQuotes::NTuple{3,T}, r=.2)::Float64) where T
#     return bapFast(getQuote(hasQuotes[1]), r) +
#         bapFast(getQuote(hasQuotes[2]), r) +
#         bapFast(getQuote(hasQuotes[3]), r)
# end

# function improve(q::Quote, r::Float64)::Currency
#     b = getBid(q)
#     a = getAsk(q)
#     a = min(a, (b >= 0.0 ? 4*b : b/4))
#     # @show q r b a
#     return round(b + r * (a - b), RoundDown; digits=2)
#     # if b < 0.0
#     #     a <= b/2 ? b + r * (a - b) : b * (1.0 - r)
#     # else
#     #     return a <= 2*b ? b + r * (a - b) : b * (1.0 + r)
#     # end
# end
# function improveFast(q::Quote, r::Float64)::Float64
#     b = F(getBid(q))
#     a = F(getAsk(q))
#     a = min(a, (b >= 0.0 ? 4*b : b/4))
#     return b + r * (a - b)
# end

# # TODO: remove this dep?
# import OptionUtil, ChainUtil

# # netExpired(lms, curp::Currency)::PT = sum(x -> netExpired1(x, curp), lms)
# # function netExpired1(lm::LegLike, curp::Currency)::PT
# #     return getQuantity(lm) * Int(getSide(lm)) * netExpired(getStyle(lm), getStrike(lm), curp)
# # end
# (netExpired(style::Style.T, strike::T, curp::T)::T) where T<:Real = (OptionUtil.extrin_sub_dist(style, strike, curp) ? abs(curp - strike) : zero(T))

# fallbackExpired(curp, otoq) = function(o)
#     res = ChainUtil.oToOq(otoq, o)
#     !isnothing(res) ? res : OptionQuote(o, Quote(C(Pricing.netExpired(getStyle(o), getStrike(o), curp))), OptionMeta())
# end

# # fallbackExpired(curp, otoq) = function(o)
# #     try
# #         return ChainUtil.oToOq(otoq, o)
# #     catch e
# #         if e isa KeyError
# #             println("WARN: Fell back to expired pricing for $(o)")
# #             return OptionQuote(o, Quote(C(Pricing.netExpired(getStyle(o), getStrike(o), curp))), OptionMeta())
# #         else
# #             rethrow(e)
# #         end
# #     end
# # end

# function calcMargin(lms::NTuple{2,T})::Sides{PT} where T
#     side1 = getSide(lms[1])
#     @assert side1 != getSide(lms[2])
#     width = calcWidth(lms[1], lms[2])
#     return side1 == Side.long ? Sides(P(width), PZ) : Sides(PZ, P(width))
# end

# # Assumes strike2 > strike1
# function calcMarginFloat(lms::NTuple{2,T})::Sides{Float64} where T
#     # side1 = getSide(lms[1])
#     # @assert side1 != getSide(lms[2])
#     width = F(calcWidth(lms[1], lms[2]))
#     return getSide(lms[1]) == Side.long ? Sides(width, 0.0) : Sides(0.0, width)
# end

# # # function calcMargin(lms::NTuple{3,LegQuoteOpen})::Sides{Currency}
# # #     # TODO: compare performance to conditionals approach
# # #     sides = Int.(getSide.(lms))
# # #     s1 = ((sides[1] - sides[2]) >> 1) * calcWidth(lms[1], lms[2])
# # #     # s2 = ((sides[1] - sides[2]) >> 1) * calcWidth(lms[1], lms[3]) # 1-3 wouldn't make sense: 1-2 or 2-3 would always be shorter because they're in strike order
# # #     s3 = ((sides[2] - sides[3]) >> 1) * calcWidth(lms[2], lms[3])
# # #     return s1 + s3 # s1 + s2 + s3
# # # end

# # This assumes two longs and a short and that the short is not in the middle
# function calcMargin(lms::NTuple{3,<:LegLike})::Sides{PT}
#     side1 = getSide(lms[1])
#     side3 = getSide(lms[3])
#     @assert side1 != side3
#     width = C(side1 == Side.long ? getQuantity(lms[3]) * calcWidth(lms[2], lms[3]) : getQuantity(lms[1]) * calcWidth(lms[1], lms[2]))
#     return side1 == Side.long ? Sides(P(width), PZ) : Sides(PZ, P(width))
# end

# # This assumes two longs and a short and that the short is not in the middle
# function calcMarginFloat(lms::NTuple{3,<:LegLike})::Sides{Float64}
#     side1 = getSide(lms[1])
#     side3 = getSide(lms[3])
#     @assert side1 != side3
#     width = side1 == Side.long ? F(getQuantity(lms[3])) * F(calcWidth(lms[2], lms[3])) : F(getQuantity(lms[1])) * F(calcWidth(lms[1], lms[2]))
#     return side1 == Side.long ? Sides(width, 0.0) : Sides(0.0, width)
# end

import LineTypes:Segments
import Lines:atsegs
function calcMarg(center, segs::Segments)::Sides{Float64}
    min_short = 0.0
    min_long = 0.0
    # i = 0
    for point in segs.points
        # i += 1
        y = point.y
        if point.x == center
            # slopeleft = segs.slopes[i]
            # sloperight = segs.slopes[i+1]
            if atsegs(segs, center * 0.5) < 0
                y >= min_short || (min_short = y)
            else
                y >= min_long || (min_long = y)
            end
        end
        if point.x < center && y < min_short
            min_short = y
        elseif point.x > center && y < min_long
            min_long = y
        end
    end
    return Sides(-min_short, -min_long)
end

# function calcMaxProfit(segs)::Float64
#     return maximum(segs) do seg
#         # TODO: optimize: don't have to check both sides
#         max(seg.left.y, seg.right.y)
#     end
# end

# function calcMarginFloat(center, lms::NTuple{3,<:LegLike}, sections)::Sides{Float64}
#     short = 0.0
#     long = 0.0
#     for section in sections
#         y = section.y
#         if y < 0
#             if section.x2 < center
#                 short += y
#             elseif section.x1 > center
#                 long += y
#             else
#                 short += y
#                 long += y
#             end
#         end
#     end
#     return Sides(-short, -long)
# end

# # TODO: don't be lazy
# function calcMargin(lms::NTuple{4,<:LegLike})::Sides{PT}
#     m = calcMarginFloat(lms)
#     return Sides(P(m.long), P(m.short))
# end
# function calcMarginFloat(lms::NTuple{4,<:LegLike})::Sides{Float64}
#     longs = filter(isLong, lms)
#     shorts = filter(isShort, lms)
#     mult = sum(getQuantity, shorts)
#     marg = mult * abs(getStrike(longs[1]) - getStrike(shorts[1]))
#     return Sides(marg, marg)
# end


# TODO: could do more efficient if sorted?
# Can't use, doesn't support quantities and would get complicated if did
# function calcMarginFloat(lms::NTuple{4,<:LegLike})::Sides{Float64}
#     longs = [-1,-1]
#     shorts = [-1,-1]
#     strikesShort = [0.0, 0.0]
#     widthsSum = 0.0
#     numShorts = 0
#     for i in eachindex(lms)
#         leg = lms[i]
#         side = getSide(leg)
#         if side == Side.short
#             numShorts += 1
#             shorts[numShorts] = i
#             sstrike = F(getStrike(leg))
#             strikesShort[numShorts] = sstrike
#             widthMin = 1e9
#             for j in eachindex(lms)
#                 (numShorts < 2 && j != longs[1]) || continue
#                 jleg = lms[j]
#                 jside = getSide(jleg)
#                 if jside == Side.long
#                     jstrike = F(getStrike(jleg))
#                     w = abs(jstrike - sstrike)
#                     if w < widthMin
#                         widthMin = w
#                         longs[numShorts] = j
#                     end
#                 end
#             end
#             widthsSum += widthMin
#         end
#     end
#     return Sides(widthsSum, widthsSum)
# end

# function calcMargin(lms::Coll{LegQuoteOpen})::Currency
#     # TODO: assert all legs have the same expiration? not sure how margin calced for diagonals
#     # strikesLong = filter(isLong, lms)
#     # strikesShort = filter(isShort, lms)
#     margin = CZ
#     longPrev = 0
#     for i in eachindex(lms)
#         println("Checking $(i)")
#         if isShort(lms[i])
#             println("Found short at $(i)")
#             # TODO: might be a way to make this loop slightly faster by skipping over i
#             # TODO: doesn't work for long long short because uses far one
#             for j in longPrev+1:lastindex(lms)
#                 if isLong(lms[j])
#                     println("Found long at $(j)")
#                     margin += calcWidth(lms[i], lms[j])
#                     longPrev = j
#                     break
#                 end
#             end
#         end
#     end
#     return margin
# end
#endregion OldMarginSpreadStuff

#region TestCommit

# at(p, x) = p.y + (
#   if x>p.x
#     ? iscall * side * q * (x - p.x)
#     : !iscall * side * (-q) * (x - p.x)
# )
function atp(lq, neto, x)
    strike = getStrike(lq)
    y = neto
    if x > strike
        # @show "x > s" isCall(lq) Int(getSide(lq)) getQuantity(lq) (x - strike)
        y += isCall(lq) ? Int(getSide(lq)) * getQuantity(lq) * (x - strike) : 0.0
    else
        # @show "x <= s" !isCall(lq) -Int(getSide(lq)) getQuantity(lq) (x - strike)
        y += !isCall(lq) ? -Int(getSide(lq)) * getQuantity(lq) * (x - strike) : 0.0
    end
    return y
end
function atp_test()
    for i in 1:10
        spread = random_spread()
        netos = toPT.(getBid.(spread))
        segs = LL.toSeg3.(spread, netos)
        x = 300.0 + rand(1:200)
        sy = Lines.at(segs[1], x)
        py = atp(spread[1], netos[1], x)
        if !(sy ≈ py)
            @show i sy py
            @show segs[1]
            return segs, spread, netos, x, sy, py
        end
    end
    return
end

function calc_commit(lqs::Coll{2,LegQuoteOpen}, netos)
    y11 = atp(lqs[1], netos[1], getStrike(lqs[1]))
    y12 = atp(lqs[1], netos[1], getStrike(lqs[2]))
    y21 = atp(lqs[2], netos[2], getStrike(lqs[1]))
    y22 = atp(lqs[2], netos[2], getStrike(lqs[2]))
    y1 = y11 + y12
    y2 = y21 + y22
    @show y1 y2 y11 y12 y21 y22
    return min(y1, y2)
end

function calc_commit2(lqs::Coll{2,LegQuoteOpen}, neto::PT)
    lq1, lq2 = lqs
    styleI = Int(getStyle(lq1))
    qty = getQuantity(styleI == 1 ? lq1 : lq2)
    width = getStrike(lq2) - getStrike(lq1)
    @assert width > 0 @str "expected width > 0" width
    side1I = Int(getSide(lq1))
    if -side1I * styleI == 1
        return neto
    end
    extra = qty * width * (-styleI) * side1I
    @show styleI qty width extra neto
    return neto + extra
end

min_seg_pt(segs)::Float64 = minimum(point -> point.y, segs.points)

using Lines
import LinesLeg as LL
function test_random()
    for _ in 1:10
        spread = random_spread()
        netos = toPT.(getBid.(spread))
        neto = sum(toPT.(getBid.(spread)))
        segs = LL.toSegments(spread, netos)
        mn = min_seg_pt(segs)

        commit = calc_commit(spread, netos)
        @show mn commit
        if !(mn ≈ commit)
            return segs, spread, neto, netos
        end
    end
    return
end
function random_spread()
    style = SH.random(Style.T)
    leg1 = random(LegQuoteOpen, style, Side.long)
    leg2 = random(LegQuoteOpen, style, Side.short)
    while getQuantity(leg1) < getQuantity(leg2)
        leg2 = random(LegQuoteOpen, style, Side.short)
    end
    return CollUtil.sortuple(getStrike, leg1, leg2)
end

#endregion TestCommit

end