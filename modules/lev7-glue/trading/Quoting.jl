module Quoting
using SH, BaseTypes, SmallTypes, QuoteTypes, LegTypes, LegTradeTypes, LegMetaTypes, ChainTypes

# TODO: right place?
# actionSideQuote(act::Action.T, sid::Side.T, q::Quote)::Quote = (Int(act) * Int(sid) == 1) ? Quote(act, -q.ask, -q.bid) : q
# sideQuote(sid::Side.T, q::Quote)::Quote = ( act = getAction(q) ; (Int(act) * Int(sid) == 1) ? Quote(act, -q.ask, -q.bid) : q )

# function SH.calcOptQuote(lookup, leg::Leg, act::Action.T=Action.open)::Union{Nothing,OptionQuote}
#     oq = lookup(getExpiration(leg), getStyle(leg), getStrike(leg))
#     return isnothing(oq) ? nothing : OptionQuote(oq, act, getSide(leg))
# end

# function SH.calcOptQuote(lookup, lm::LegMeta, act::Action.T=Action.open)::Union{Nothing,OptionQuote}
#     oq = lookup(getExpiration(lm), getStyle(lm), getStrike(lm))
#     return isnothing(oq) ? nothing : OptionQuote(oq, act, getSide(lm))
# end

function SH.calcOptQuote(lookup, leg::Union{Leg,LegMeta,LegTrade}, act::Action.T=Action.open)::Union{Nothing,OptionQuote}
    oq = lookup(getExpiration(leg), getStyle(leg), getStrike(leg))
    return isnothing(oq) ? nothing : OptionQuote(oq, act, getSide(leg))
end

function SH.calcQuote(lookup, lms::Union{Coll{LegMeta},AVec{<:LegTrade}}, act::Action.T=Action.open)::Quote
    sumQuotes(getQuote(calcOptQuote(lookup, lm, act)) for lm in lms)
end

end
