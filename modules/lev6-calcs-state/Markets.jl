module Markets
using Dates
using DateUtil, DictUtil, LogUtil
using Globals, BaseTypes, QuoteTypes, Caches, TradierData
using Calendars, DataHelper

export Market, market # , marketPrices, urp, urpon, urpoff, mktNumDays

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
    market(;age=Second(0))
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

function market(; age=PERIOD_UPDATE2)::Market
    return cache!(MARKET_TYPE, MARKET, age) do
        return newVal()
    end
end
marketPrices() = (mkt = market() ; (mkt.startPrice, mkt.curp))

mktNumDays(d::Date)::Int = bdays(market().startDay, d)
# mktTimeToExp(exp::Date)::Float64 = urp() ? timeToExpir(market().tsMarket, getMarketClose(exp)) : timeToExpir(getMarketOpen(market().startDay), getMarketClose(exp))

#region Local
const MARKET = :market
const MARKET_TYPE = Market
const USE_CURP = :useCurp
# const PERIOD_UPDATE = Second(12)
const PERIOD_UPDATE2 = Minute(1)

# function __init__()
#     Globals.has(USE_CURP) || Globals.set(USE_CURP, false)
# end

whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = whenMarket(from, isMktOpen, nextMktChange, PERIOD_UPDATE2)

canTrade() = ( delta = now(UTC) - market().tsUpdate ; delta <= PERIOD_UPDATE2 + Second(10) || error("Don't trade when market data out of date: ", delta, " seconds") )

function update()::Nothing
    @log debug "updateMarket"
    setCache!(MARKET, newVal())
    return
end

using HistData
function newVal()::Market
    # t1 = @timed
    res = tradierQuotes(("SPY", "VIX"))
    if haskey(res, "quote")
        tqs = res["quote"]
        tq = tqs[1]
        tsMarket = unix2datetime(max(tq["bid_date"], tq["ask_date"])/1000)
    # elseif (!isnothing(snap()) && abs(snapTs() - tsMarket) > Minute(10))
    elseif !isnothing(snap())
        # TODO: clean this up
        # println("Falling back because snap doesn't have both quotes call")
        tq = tradierQuote()
        # println("Markets.newVal snap failover: ", tq)
        @assert tq["symbol"] == "SPY"
        q = Quote(C(get(tq, "bid", 0.0)), C(get(tq, "ask", 0.0)))
        tsMarket = unix2datetime(max(tq["bid_date"], tq["ask_date"])/1000)
        # @show snapTs() tsMarket tq
        if abs(snapTs() - tsMarket) > Minute(15)
            @show snapTs() tsMarket tq
            error("Failed to get snapped SPY quote for fallback")
        end
        startDay = nextTradingDay(toDateMarket(tsMarket))
        vix = dataDay(startDay, "VIX").open
        m = C(0.5 * (q.bid + q.ask))
        op = C(getnn(tq, "open", m))
        sp = urp() ? m : op
        return Market(startDay, q, vix, m, sp, op, tsMarket, now(UTC))
    else
        error("tradierQuotes invalid return and not snapped: ", res)
    end
    @assert tqs[1]["symbol"] == "SPY"
    @assert tqs[2]["symbol"] == "VIX"
    # q = Quote(C(tryKey(tq, "bid", 0.0)), C(tryKey(tq, "ask", 0.0)))
    q = Quote(C(get(tq, "bid", 0.0)), C(get(tq, "ask", 0.0)))
    vix = C(tqs[2]["last"]) # TODO: make sure this is the right field to use
    startDay = nextTradingDay(DateUtil.market_date(tsMarket))
    m = C(0.5 * (q.bid + q.ask))
    # op = C(tryKey(tq, "open", m))
    op = C(getnn(tq, "open", m))
    sp = urp() ? m : op
    # @log info "Loaded market" Threads.threadid() t1.time
    return Market(startDay, q, vix, m, sp, op, tsMarket, now(UTC))
end
#endregion

end