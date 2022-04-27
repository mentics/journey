module Markets
using Dates
using DateUtil, DictUtil, LogUtil
using Globals, BaseTypes, QuoteTypes, Caches, TradierData
using Calendars, DataHelper

export Market, market, marketPrices, urp, urpon, urpoff, mktNumDays, mktTimeToExp

mutable struct Market
    startDay::Date
    curQuot::Quote
    curp::Currency
    startPrice::Currency
    open::Currency
    tsMarket::DateTime
    tsUpdate::DateTime
end

urpon() = urp(true) ; urpoff() = urp(false)
function urp(b::Bool)::Nothing
    Globals.set(USE_CURP, b)
    market(;up=true)
    return
end
urp() = Globals.get(USE_CURP)

function market(; up=false)::Market
    cache!(MARKET_TYPE, MARKET, tooOld(PERIOD_UPDATE); up) do
        up || @log error "Market not up to date"
        newVal()
    end
end
marketPrices() = (mkt = market() ; (mkt.startPrice, mkt.curp))

mktNumDays(d::Date)::Int = bdays(market().startDay, d)
mktTimeToExp(exp::Date)::Float64 = urp() ? timeToExpir(market().tsMarket, getMarketClose(exp)) : timeToExpir(getMarketOpen(market().startDay), getMarketClose(exp))

#region Local
const MARKET = :market
const MARKET_TYPE = Market
const USE_CURP = :useCurp
const PERIOD_UPDATE = Second(12)

function __init__()
    Globals.has(USE_CURP) || Globals.set(USE_CURP, false)
    return
end

whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = whenMarket(from, isMktOpen, nextMktChange, PERIOD_UPDATE)

canTrade() = ( delta = now(UTC) - market().tsUpdate ; delta <= PERIOD_UPDATE+Second(5) || error("Don't trade when market data out of date: ", delta, " seconds") )

function update()::Nothing
    @log debug "updateMarket"
    setCache!(MARKET, newVal())
    return
end
function newVal()::Market
    tq = tradierQuote()
    q = Quote(C(tryKey(tq, "bid", 0.0)), C(tryKey(tq, "ask", 0.0)))
    tsMarket = unix2datetime(max(tq["bid_date"], tq["ask_date"])/1000) # tq["trade_date"]
    startDay = nextTradingDay(toDateMarket(tsMarket))
    m = C(0.5 * (q.bid + q.ask))
    op = C(tryKey(tq, "open", m))
    sp = urp() ? m : op
    return Market(startDay, q, m, sp, op, tsMarket, now(UTC))
end
#endregion

end