module LegTradeInfo
using SH, BaseTypes, SmallTypes, LegTradeTypes
using Chains, Expirations

Base.show(io::IO, leg::LegTrade) = print(io, string(leg))

# TODO: change to use to(NamedTuple...)
function Base.string(leg::LegTrade)
    res = "Leg $(getId(leg)) $(getOption(leg)) $(getSide(leg)) $(getQuantity(leg))"
    pdo = getPrillDirOpen(leg)
    if !isnothing(pdo)
        res *= " op:$(pdo)"
        pdc = getPrillDirClose(leg)
        if isnothing(pdc)
            if getExpir(leg) in expirs()
                oq = optQuoter(leg, Action.close)
                res *= " cl($(getBid(oq)), $(getAsk(oq)))"
                res *= " urpnl:$(pdo + getBid(oq))"
            else
                res *= " NOQ(exp)"
            end
        else
            res *= " cl:$(pdc) rpnl:$(pdc + pdo)"
        end
    end
    return res
end

function SH.to(::Type{NamedTuple}, lt::LegTrade)
    open = getNetOpen(lt)
    netc = getNetClose(lt)
    if ismissing(netc)
        if getExpir(lt) in expirs()
            oq = optQuoter(lt, Action.close)
            close = getQuote(oq)
            pnl = open + getBid(oq)
        else
            close = "NOQ(exp)"
            pnl = "exp"
        end
    else
        close = netc
        pnl = pdc + pdo
    end
    (;lid=getId(lt), exp=getExpir(lt), strike=getStrike(lt), style=getStyle(lt), side=getSide(lt), qty=getQuantity(lt), open, close, pnl)
end

# TODO: this is identical to in LegQuoteTypes
# using SmallTypes, QuoteTypes, ChainTypes
# SH.calcQuote(lookup::Function, legs::AVec{<:LegTrade}, act::Action.T=Action.open)::Quote = sumQuotes(getQuote(calcOptQuote(lookup, leg, act)) for leg in legs)
# function SH.calcOptQuote(lookup::Function, leg::LegTrade, act::Action.T=Action.open)::Union{Nothing,OptionQuote}
#     OptionQuote(lookup(getExpir(leg), getStyle(leg), getStrike(leg)), act, getSide(leg))
# end

end