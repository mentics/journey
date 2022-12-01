module Pricing
using SH, BaseTypes, QuoteTypes

export bap, improve

# SH.bap(lm)::Currency = getBid(lm)
# SH.bap(hasQuotes::Coll)::Currency = sum(getBid, hasQuotes)
# RAT2 = .2
bap(hasQuote, r=.2)::Currency = round(improve(getQuote(hasQuote), r), RoundDown; digits=2)
bap(qt::Quote, r=.2)::Currency = round(improve(qt, r), RoundDown; digits=2)
# SH.bap(hasQuotes::Coll, r=.2)::Currency = round(sum(bap.(getQuote.(hasQuotes), r)), RoundDown; digits=2)
bap(hasQuotes::Coll, r=.2)::Currency = round(bap(sumQuotes(getQuote.(hasQuotes)), r), RoundDown; digits=2)

# improve(q::Quote, r::Float64)::Currency = getBid(q) + r * (getAsk(q) - getBid(q))
function improve(q::Quote, r::Float64)::Currency
    b = getBid(q)
    a = getAsk(q)
    a = min(a, (b >= 0.0 ? 4*b : b/4))
    # @show q r b a
    return b + r * (a - b)
    # if b < 0.0
    #     a <= b/2 ? b + r * (a - b) : b * (1.0 - r)
    # else
    #     return a <= 2*b ? b + r * (a - b) : b * (1.0 + r)
    # end
end

end