module ChainTypes
import Dates:Date
using BaseTypes, SmallTypes, OptionQuoteTypes

export OptionQuote, OptionChain
export ChainsType, SymChainsType, Oqss
export UnderTime, ChainInfo
export Chain, ChainSearch
export Otoq

const Otoq = Dict{Date,Styles{Dict{Currency,OptionQuote}}}

struct OptionChain
    chain::Vector{OptionQuote}
    ts::Int
end

const ChainsType = Dict{Date,OptionChain}
const SymChainsType = Dict{String,Dict{Date,OptionChain}}
const Oqss = Styles{Sides{Vector{OptionQuote}}}

struct UnderTime
    under::Currency
    open::Currency
    hi::Currency
    lo::Currency
end

struct ChainInfo # <: Chain
    xsoqs::Dict{Date,Styles{Vector{OptionQuote}}}
    under::UnderTime
    xpirs::Vector{Date}
end

abstract type Chain end
abstract type ChainSearch end

end