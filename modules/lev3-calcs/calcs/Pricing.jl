module Pricing
using AbstractTypes, SH, BaseTypes, SmallTypes, QuoteTypes, OptionQuoteTypes, OptionMetaTypes, LegMetaTypes
import OptionUtil

export bap

bap(qt::Quote, r=.2)::Currency = improve(qt, r)
bap(hasQuote, r=.2)::Currency = improve(getQuote(hasQuote), r)
bap(hasQuotes::Coll, r=.2)::Currency = round(sum(bap.(getQuote.(hasQuotes), r)), RoundDown; digits=2)
function (bap(hasQuotes::NTuple{3,T}, r=.2)::Currency) where T
    s = bap(getQuote(hasQuotes[1]), r) +
        bap(getQuote(hasQuotes[2]), r) +
        bap(getQuote(hasQuotes[3]), r)
    return round(s, RoundDown; digits=2)
end

bapFast(qt::Quote, r=.2)::Float64 = improveFast(qt, r)
function (bapFast(hasQuotes::NTuple{3,T}, r=.2)::Float64) where T
    return bapFast(getQuote(hasQuotes[1]), r) +
        bapFast(getQuote(hasQuotes[2]), r) +
        bapFast(getQuote(hasQuotes[3]), r)
end

function improve(q::Quote, r::Float64)::Currency
    b = getBid(q)
    a = getAsk(q)
    a = min(a, (b >= 0.0 ? 4*b : b/4))
    # @show q r b a
    return round(b + r * (a - b), RoundDown; digits=2)
    # if b < 0.0
    #     a <= b/2 ? b + r * (a - b) : b * (1.0 - r)
    # else
    #     return a <= 2*b ? b + r * (a - b) : b * (1.0 + r)
    # end
end
function improveFast(q::Quote, r::Float64)::Float64
    b = F(getBid(q))
    a = F(getAsk(q))
    a = min(a, (b >= 0.0 ? 4*b : b/4))
    return b + r * (a - b)
end

# TODO: remove this dep?
import OptionUtil, ChainUtil

netExpired(lms, curp::Currency)::PT = sum(x -> netExpired1(x, curp), lms)
function netExpired1(lm::LegType, curp::Currency)::PT
    return getQuantity(lm) * Int(getSide(lm)) * netExpired(getStyle(lm), getStrike(lm), curp)
end
netExpired(style::Style.T, strike::Currency, curp::Currency)::PT = (OptionUtil.extrinSub(style, strike, curp) ? abs(curp - strike) : 0.0)

fallbackExpired(curp, otoq) = function(o)
    try
        return ChainUtil.oToOq(otoq, o)
    catch e
        if e isa KeyError
            return OptionQuote(o, Quote(C(Pricing.netExpired(getStyle(o), getStrike(o), curp))), OptionMeta())
        else
            rethrow(e)
        end
    end
end

# function calcMargin(lms::NTuple{2,LegMetaOpen})::Sides{Currency}
#     @assert getSide(lms[1]) != getSide(lms[2])
#     return calcWidth(lms[1], lms[2])
# end

function calcMargin(lms::NTuple{2,T})::Sides{Currency} where T
    side1 = getSide(lms[1])
    @assert side1 != getSide(lms[2])
    width = calcWidth(lms[1], lms[2])
    return side1 == Side.long ? Sides(width, CZ) : Sides(CZ, width)
end

function calcMarginFloat(lms::NTuple{2,T})::Sides{Float64} where T
    side1 = getSide(lms[1])
    @assert side1 != getSide(lms[2])
    width = F(calcWidth(lms[1], lms[2]))
    return side1 == Side.long ? Sides(width, 0.0) : Sides(0.0, width)
end

# function calcMargin(lms::NTuple{3,LegMetaOpen})::Sides{Currency}
#     # TODO: compare performance to conditionals approach
#     sides = Int.(getSide.(lms))
#     s1 = ((sides[1] - sides[2]) >> 1) * calcWidth(lms[1], lms[2])
#     # s2 = ((sides[1] - sides[2]) >> 1) * calcWidth(lms[1], lms[3]) # 1-3 wouldn't make sense: 1-2 or 2-3 would always be shorter because they're in strike order
#     s3 = ((sides[2] - sides[3]) >> 1) * calcWidth(lms[2], lms[3])
#     return s1 + s3 # s1 + s2 + s3
# end

function calcMargin(lms::NTuple{3,<:LegType})::Sides{Currency}
    side1 = getSide(lms[1])
    side2 = getSide(lms[2])
    if side1 == side2
        width = calcWidth(lms[2], lms[3])
        dir = toOther(side2)
    else
        width = calcWidth(lms[1], lms[2])
        dir = side2
    end
    return dir == Side.long ? Sides(width, CZ) : Sides(CZ, width)
end

function calcMarginFloat(lms::NTuple{3,<:LegType})::Sides{Float64}
    side1 = getSide(lms[1])
    side2 = getSide(lms[2])
    if side1 == side2
        width = F(calcWidth(lms[2], lms[3]))
        dir = toOther(side2)
    else
        width = F(calcWidth(lms[1], lms[2]))
        dir = side2
    end
    return dir == Side.long ? Sides(width, 0.0) : Sides(0.0, width)
end

# function calcMargin(lms::Coll{LegMetaOpen})::Currency
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

end