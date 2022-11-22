module ChainTypes
import Dates
using BaseTypes, SH, SmallTypes, OptionTypes, QuoteTypes, OptionMetaTypes, Bins

export OptionQuote, OptionChain
export ChainsType, SymChainsType, Oqss

struct OptionQuote
    option::Option
    quot::Quote
    meta::OptionMeta
    src::Union{Nothing,Dict{String,Any}}
end
OptionQuote(;option=Option(), quot=Quote(), meta=OptionMeta(), src=nothing) = OptionQuote(option, quot, meta, src)
OptionQuote(oq::OptionQuote; option=getOption(oq), quot=getQuote(oq), meta=getMeta(oq)) = OptionQuote(option, quot, meta, nothing)
OptionQuote(oq::OptionQuote, action::Action.T, side::Side.T) = OptionQuote(getOption(oq), Quote(getQuote(oq), action, side), getMeta(oq), nothing)
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
OptionMetaTypes.getGreeks(oq::OptionQuote) = getGreeks(oq.meta)

SH.isValid(curp::Currency) =
    oq::OptionQuote ->
        getBid(oq) > 0.0 &&
        (abs(getStrike(oq) / curp - 1.0) <= Bins.SPAN/2)
# SH.isValid(oq::OptionQuote) = getBid(oq) > 0.0

struct OptionChain
    chain::Vector{OptionQuote}
    ts::Int
end

const ChainsType = Dict{Dates.Date,OptionChain}
const SymChainsType = Dict{String,Dict{Dates.Date,OptionChain}}
const Oqss = Styles{Sides{Vector{OptionQuote}}}

end