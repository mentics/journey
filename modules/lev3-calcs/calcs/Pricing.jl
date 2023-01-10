module Pricing
using SH, BaseTypes, QuoteTypes

export bap, improve

bap(qt::Quote, r=.2)::Currency = round(improve(qt, r), RoundDown; digits=2)
bap(hasQuote, r=.2)::Currency = improve(getQuote(hasQuote), r)
bap(hasQuotes::Coll, r=.2)::Currency = round(sum(bap.(getQuote.(hasQuotes), r)), RoundDown; digits=2)

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

end