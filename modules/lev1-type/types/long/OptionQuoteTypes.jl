module OptionQuoteTypes
using SH, OptionTypes, QuoteTypes, OptionMetaTypes

export OptionQuote

struct OptionQuote
    option::Option
    quot::Quote
    meta::OptionMeta
end
SH.getOption(oq::OptionQuote) = oq.option
SH.getQuote(oq::OptionQuote) = oq.quot

SH.getStyle(oq::OptionQuote) = getStyle(oq.option)
SH.getExpir(oq::OptionQuote) = getExpir(oq.option)
SH.getStrike(oq::OptionQuote) = getStrike(oq.option)
SH.getBid(oq::OptionQuote) = getBid(oq.quot)
SH.getAsk(oq::OptionQuote) = getAsk(oq.quot)
SH.getIv(oq::OptionQuote) = oq.meta.iv
SH.getGreeks(oq::OptionQuote) = getGreeks(oq.meta)

end