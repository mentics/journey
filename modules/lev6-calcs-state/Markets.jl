module Markets
using Dates
using DateUtil, DictUtil, LogUtil
using Globals, BaseTypes, QuoteTypes, Caches, TradierData
using Calendars, DataHelper

export Market, market, marketPrices, urp, urpon, urpoff, mktNumDays

mutable struct Market
    startDay::Date
    curQuot::Quote
    vix::Currency
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

using ThreadUtil
const lock = ReentrantLock()
const MARKETS_SNAP = Dict{String,Market}()
marketSnap(snapName::String, revert=true) = useKey(MARKETS_SNAP, snapName) do
    runSync(lock) do
        back = snap()
        !isnothing(back) || error("Don't chainsSnap when not snapped")
        snap(snapName)
        res = market()
        !revert || snap(back)
        println("Cached market for ", snapName)
        return res
    end
end

function market(; up=false)::Market
    cache!(MARKET_TYPE, MARKET, tooOld(PERIOD_UPDATE, isMarketOpen()); up) do
        up || @log error "Market not up to date"
        newVal()
    end
end
marketPrices() = (mkt = market() ; (mkt.startPrice, mkt.curp))

mktNumDays(d::Date)::Int = bdays(market().startDay, d)
# mktTimeToExp(exp::Date)::Float64 = urp() ? timeToExpir(market().tsMarket, getMarketClose(exp)) : timeToExpir(getMarketOpen(market().startDay), getMarketClose(exp))

#region Local
const MARKET = :market
const MARKET_TYPE = Market
const USE_CURP = :useCurp
const PERIOD_UPDATE = Second(12)

# function __init__()
#     Globals.has(USE_CURP) || Globals.set(USE_CURP, false)
# end

whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = whenMarket(from, isMktOpen, nextMktChange, PERIOD_UPDATE)

canTrade() = ( delta = now(UTC) - market().tsUpdate ; delta <= 2 * PERIOD_UPDATE || error("Don't trade when market data out of date: ", delta, " seconds") )

function update()::Nothing
    @log debug "updateMarket"
    setCache!(MARKET, newVal())
    return
end
function newVal()::Market
    tqs = tradierQuotes(("SPY", "VIX"))["quote"]
    tq = tqs[1]
    tsMarket = unix2datetime(max(tq["bid_date"], tq["ask_date"])/1000)
    # TODO: clean this up
    if (!isnothing(snap()) && abs(snapTs() - tsMarket) > Minute(10))
        # println("Falling back because snap doesn't have quotes call")
        tq = tradierQuote()
        @assert tq["symbol"] == "SPY"
        q = Quote(C(tryKey(tq, "bid", 0.0)), C(tryKey(tq, "ask", 0.0)))
        tsMarket = unix2datetime(max(tq["bid_date"], tq["ask_date"])/1000)
        startDay = nextTradingDay(toDateMarket(tsMarket))
        vix = C(0.0)
        m = C(0.5 * (q.bid + q.ask))
        op = C(tryKey(tq, "open", m))
        sp = urp() ? m : op
        return Market(startDay, q, vix, m, sp, op, tsMarket, now(UTC))
    end
    @assert tqs[1]["symbol"] == "SPY"
    @assert tqs[2]["symbol"] == "VIX"
    q = Quote(C(tryKey(tq, "bid", 0.0)), C(tryKey(tq, "ask", 0.0)))
    vix = C(tqs[2]["last"]) # TODO: make sure this is the right field to use
    startDay = nextTradingDay(toDateMarket(tsMarket))
    m = C(0.5 * (q.bid + q.ask))
    op = C(tryKey(tq, "open", m))
    sp = urp() ? m : op
    return Market(startDay, q, vix, m, sp, op, tsMarket, now(UTC))
end
#endregion

end