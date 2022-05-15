module ChainTypes
using BaseTypes, SH, SmallTypes, OptionTypes, QuoteTypes, OptionMetaTypes

export OptionQuote, OptionChain, getExtrinsic

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
function getExtrinsic(oq::OptionQuote, curp::Currency)::Tuple{Currency,Currency}
    bid = getBid(oq)
    ask = getAsk(oq)
    s = getStrike(oq)
    dist = abs(curp - s)
    if Style.call == getStyle(oq)
        return s > curp ? (bid, ask) : (bid - dist, ask - dist)
    else
        return s < curp ? (bid, ask) : (bid - dist, ask - dist)
    end
end

struct OptionChain
    chain::Vector{OptionQuote}
    ts::Int
end

end