module LegTradeInfo
using SH, BaseTypes, LegTradeTypes
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
            if getExpiration(leg) in expirs(;td=true)
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

function SH.to(NamedTuple, lt::LegTrade)
    open = getNetOpen(lt)
    netc = getNetClose(lt)
    if ismissing(netc)
        if getExpiration(lt) in expirs(;td=true)
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
    (;lid=getId(lt), exp=getExpiration(lt), strike=getStrike(lt), style=getStyle(lt), side=getSide(lt), qty=getQuantity(lt), open, close, pnl)
end

# TODO: this is identical to in LegMetaTypes
using SmallTypes, QuoteTypes, ChainTypes
SH.calcQuote(lookup::Function, legs::AVec{<:LegTrade}, act::Action.T=Action.open)::Quote = sumQuotes(getQuote(calcOptQuote(lookup, leg, act)) for leg in legs)
SH.calcOptQuote(lookup::Function, leg::LegTrade, act::Action.T=Action.open)::OptionQuote = OptionQuote(lookup(getExpiration(leg), getStyle(leg), getStrike(leg)), act, getSide(leg))

end