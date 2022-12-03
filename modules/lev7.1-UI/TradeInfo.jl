module TradeInfo
using SH, BaseTypes, StatusTypes, TradeTypes
using DateUtil, OptionUtil
using Markets, Chains
using OutputUtil

using Globals, RetTypes
# minMaxPnl(trad::Trade)::Tuple{Currency,Currency} = C.(extrema(getVals(combineTo(Ret, [trad], getTargetDate(trad), market().startPrice, Globals.get(:vtyRatio)))))
minMaxPnl(trad::Trade)::Tuple{Currency,Currency} = C.(extrema(OptionUtil.legsExtrema(getNetOpen(trad), getLegs(trad)...)))

# getMaxClose(trad::Trade)::Currency = getMaxClose(getLegs(trad))
# findPriceOpen(trad) = (dt = Date(tsOpen(trad)); dt == today() ? market().open : priceOpen(dt))

# function strFilled(trad)
#     mkt = market()
#     no = getNetOpen(trad)
#     qt = quoter(trad, Action.close)
#     profit = no + qt.bid
#     maxl, maxp = Currency.(minMaxPnl(trad))
#     # TODO: clean up?
#     sat = C(valAtPrice(combineTo(Ret, [trad], getTargetDate(trad), mkt.startPrice, Globals.get(:vtyRatio)), Float64(mkt.curp)))
#     # sat = 123.123 # Currency(estAtPrice(segsetCheck(trad, findPriceOpen(trad)), Float64(mkt.curp)))
#     return "op:$(no) cl($(qt.bid),$(qt.ask))/$(getMaxClose(trad)) urpnl:$(PRI(string(profit))) @$(SEC(string(sat)))/($(maxl),$(maxp))"
# end

Base.show(io::IO, trade::Trade{S}) where S = println(io, string(trade))

canQuote(trade::Trade) = isnothing(findfirst(leg -> !CH.canQuoteXpr(getExpiration(leg)), getLegs(trade)))

# haskey(chains(), getTargetDate(trad))

function Base.string(trad::Trade{S}) where S
    # strBasic(trad::Trade) = "Trade $(getTid(trad)) $(shStatus(trad)) $(shortDate(dateOpen(trad), getTargetDate(trad)))"
    res = "Trade $(getId(trad)) $(shStatus(S)) $(strShort(toDateMarket(tsCreated(trad)), getTargetDate(trad)))"
    pdo = getPrillDirOpen(trad)
    if !isnothing(pdo)
        res *= " op:$(pdo)"
        pdc = getPrillDirClose(trad)
        # if isnothing(pdc)
        if S != Closed
            if canQuote(trad)
                qt = quoter(trad, Action.close)
                res *= " cl($(getBid(qt)), $(getAsk(qt)))/TODO:MAXCLOSE" # TODO: calc max close
                res *= " urpnl:$(PRI(pdo + getBid(qt)))"

                maxl, maxp = Currency.(minMaxPnl(trad))
                # TODO: clean up?
                mkt = market()
                # TODO: include this info in legs and original IV and current IV and prices based on those
                # sat = C(valAtPrice(combineTo(Ret, [trad], getTargetDate(trad), mkt.startPrice, Globals.get(:vtyRatio)), mkt.curp))
                sat = C(valAtPrice(combineTo(Ret, [trad], mkt.startPrice), mkt.curp))

                res *= " @$(SEC(sat))/($(maxl),$(maxp))"
            else
                res *= " oldtrad?"
            end
        else
            res *= " cl:$(pdc) rpnl:$(pdc + pdo)"
        end
    end
    res *= '\n' * indentedLegs(trad)
    return res
end

indentedLegs(trad::Trade) = join(["  $(string(leg))" for leg in getLegs(trad)], "\n")

(shStatus(::Type{S})::String) where S = uppercase(first(string(S), 3))

# TODO: move?
using SmallTypes, QuoteTypes
SH.calcQuote(lookup::Function, trade::Trade, act::Action.T=Action.open)::Quote = calcQuote(lookup, getLegs(trade), act)

export urpnl
function urpnl(trade::Trade)::Currency
    getTargetDate(trade) >= market().startDay || error("Tried to get urpnl of past trade")
    netOpen = getNetOpen(trade)
    prClose = getBid(quoter(trade, Action.close))
    return netOpen + prClose
end

end