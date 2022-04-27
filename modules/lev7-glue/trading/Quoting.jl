module Quoting
using SH, BaseTypes, SmallTypes, QuoteTypes, LegTypes, LegMetaTypes, ChainTypes

# TODO: right place?
# actionSideQuote(act::Action.T, sid::Side.T, q::Quote)::Quote = (Int(act) * Int(sid) == 1) ? Quote(act, -q.ask, -q.bid) : q
# sideQuote(sid::Side.T, q::Quote)::Quote = ( act = getAction(q) ; (Int(act) * Int(sid) == 1) ? Quote(act, -q.ask, -q.bid) : q )

# SH.calcQuote(lookup::Function, lms::AVec{Leg}, act::Action.T=Action.open)::Quote = sumQuotes(calcQuote(lookup, lm, act) for lm in lms)
SH.calcOptQuote(lookup, lm::Leg, act::Action.T=Action.open)::OptionQuote = OptionQuote(lookup(getExpiration(lm), getStyle(lm), getStrike(lm)), act, getSide(lm))
    # return Quote(getSide(lm), act, getBid(q), getAsk(q))


# SH.calcQuote(lookup::Function, lms::AVec{LegMeta}, act::Action.T=Action.open)::Quote = sumQuotes(calcQuote(lookup, lm, act) for lm in lms)
SH.calcOptQuote(lookup, lm::LegMeta, act::Action.T=Action.open)::OptionQuote = OptionQuote(lookup(getExpiration(lm), getStyle(lm), getStrike(lm)), act, getSide(lm))

SH.calcQuote(lookup, lms::Coll{LegMeta,4}, act::Action.T=Action.open)::Quote = sumQuotes(getQuote(calcOptQuote(lookup, lm, act)) for lm in lms)

end
