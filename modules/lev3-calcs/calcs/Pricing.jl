module Pricing
using SH, BaseTypes, SmallTypes, QuoteTypes, LegMetaTypes
import OptionUtil

export bap

bap(qt::Quote, r=.2)::Currency = improve(qt, r)
bap(hasQuote, r=.2)::Currency = improve(getQuote(hasQuote), r)
bap(hasQuotes::Coll, r=.2)::Currency = round(sum(bap.(getQuote.(hasQuotes), r)), RoundDown; digits=2)
function bap(hasQuotes::NTuple{3,T}, r=.2)::Currency where T
    s = bap(getQuote(hasQuotes[1]), r) +
        bap(getQuote(hasQuotes[2]), r) +
        bap(getQuote(hasQuotes[3]), r)
    return round(s, RoundDown; digits=2)
end

bapFast(qt::Quote, r=.2)::Currency = improveFast(qt, r)
function bapFast(hasQuotes::NTuple{3,T}, r=.2)::Currency where T
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
function improveFast(q::Quote, r::Float64)::Currency
    b = getBid(q)
    a = getAsk(q)
    a = min(a, (b >= 0.0 ? 4*b : b/4))
    return b + r * (a - b)
end

netExpired(lms, curp) = sum(x -> netExpired1(x, curp), lms)
function netExpired1(lm, curp)
    s = getStrike(lm)
    return getQuantity(lm) * (extrinSub(getStyle(lm), s, curp) ? Int(getSide(lm)) * abs(curp - s) : 0.0)
end

function calcMargin(lms::NTuple{2,LegMetaOpen})::Currency
    @assert getSide(lms[1]) != getSide(lms[2])
    return calcWidth(lms[1], lms[2])
end

function calcMargin(lms::NTuple{3,LegMetaOpen})::Currency
    # TODO: compare performance to conditionals approach
    sides = Int.(getSide.(lms))
    s1 = ((sides[1] - sides[2]) >> 1) * calcWidth(lms[1], lms[2])
    # s2 = ((sides[1] - sides[2]) >> 1) * calcWidth(lms[1], lms[3]) # 1-3 wouldn't make sense: 1-2 or 2-3 would always be shorter because they're in strike order
    s3 = ((sides[2] - sides[3]) >> 1) * calcWidth(lms[2], lms[3])
    return s1 + s3 # s1 + s2 + s3
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