module ChainTypes
using BaseTypes, SH, SmallTypes, OptionTypes, QuoteTypes, OptionMetaTypes

export OptionQuote, OptionChain

struct OptionQuote
    option::Option
    quot::Quote
    meta::OptionMeta
end
OptionQuote(;option=Option(), quot=Quote(), meta=OptionMeta()) = OptionQuote(option, quot, meta)
OptionQuote(oq::OptionQuote; option=getOption(oq), quot=getQuote(oq), meta=getMeta(oq)) = OptionQuote(option, quot, meta)
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
SH.getIv(oq::OptionQuote) = oq.meta.iv
SH.isCall(oq::OptionQuote) = getStyle(oq.option) == Style.call
SH.isPut(oq::OptionQuote) = getStyle(oq.option) == Style.put

SH.isValid(configStrats::Dict{Symbol,Any}, curp::Currency) =
    oq ->
        isValid(configStrats, curp, getOption(oq)) &&
        getBid(oq) > 0.0 &&
        (abs(1.0 - curp / getStrike(oq)) <= .15)

struct OptionChain
    chain::Vector{OptionQuote}
    ts::Int
end

end