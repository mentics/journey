module ChainTypes
import Dates:Date
using SmallTypes, OptionQuoteTypes

export OptionQuote, OptionChain
export ChainsType, SymChainsType, Oqss

struct OptionChain
    chain::Vector{OptionQuote}
    ts::Int
end

const ChainsType = Dict{Date,OptionChain}
const SymChainsType = Dict{String,Dict{Date,OptionChain}}
const Oqss = Styles{Sides{Vector{OptionQuote}}}

end