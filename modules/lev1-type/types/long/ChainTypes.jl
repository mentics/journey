module ChainTypes
using BaseTypes, SH, SmallTypes, OptionTypes, QuoteTypes, OptionMetaTypes

export OptionQuote, OptionChain

struct OptionQuote
    option::Option
    quot::Quote
    meta::OptionMeta
end
OptionQuote(;option=Option(), quot=Quote(), meta=OptionMeta()) = OptionQuote(option, quot, meta)
OptionQuote(oq::OptionQuote, action::Action.T, side::Side.T) = OptionQuote(getOption(oq), Quote(getQuote(oq), action, side), getMeta(oq))
SH.getOption(oq::OptionQuote) = oq.option
SH.getQuote(oq::OptionQuote) = oq.quot
SH.getQuote(oq::OptionQuote, side::Side.T) = Quote(oq.quot, side)
SH.getQuote(oq::OptionQuote, action::Action.T, side::Side.T) = Quote(oq.quot, action, side)
SH.getMeta(oq::OptionQuote) = oq.meta
SH.getAction(oq::OptionQuote) = getAction(oq.quot)

SH.getStyle(oq::OptionQuote) = getStyle(oq.option)
SH.getExpiration(oq::OptionQuote) = getExpiration(oq.option)
SH.getStrike(oq::OptionQuote) = getStrike(oq.option)
SH.getBid(oq::OptionQuote) = getBid(oq.quot)
SH.getAsk(oq::OptionQuote) = getAsk(oq.quot)

# TODO: Put this in the right place
export bap
bap(lms::Coll)::Currency = sum(lm -> bap(lm), lms)
bap(lm)::Currency = getBid(lm)

struct OptionChain
    chain::Vector{OptionQuote}
    ts::Int
end

end