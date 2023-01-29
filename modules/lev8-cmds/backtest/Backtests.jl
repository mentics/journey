module Backtests
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
using LogUtil, DateUtil, ChainUtil, CollUtil
using SimpleStore
import SimpleStore as SS
import Pricing, Between, Shorthand
import OutputUtil:pp

#region Public
function run(strat::Strat, from::DateLike, to::DateLike; maxSeconds::Int=1)::Nothing
    LogUtil.resetLog(:backtest)

    params = strat.params
    global keepParams = params
    global tss = SS.getTss(from, to)
    global info = BacktestInfo(from, to, tss[1], tss[end], params)
    log("Starting backtest info ", info)
    # $(from) - $(to) params:\n", params
    acct = makeAccount(strat.acctTypes, params)
    global keepAcct = acct
    chainRef = Ref{Chain}()
    ops = makeOps(acct, chainRef)

    SS.run(from, to; maxSeconds) do tim, chain
        otoq = ChainUtil.toOtoq(chain)
        if !tim.atClose
            checkExits(strat, acct, tim, otoq)
            strat(ops, tim, chain)
        end
        if tim.lastOfDay
            handleExpirations(acct, tim, chain, otoq)
        end
    end
end
<<<<<<< Updated upstream
=======

function showResult(acct=keepAcct, params=keepParams)::Nothing
    # balReals = foldl(red.balReal, acct.closed; init=[params.balInit])
    # balReals = maps(red.balReal, acct.closed, params.balInit)
    balReals = mapPoint(pts.balReal, acct.closed, (acctTsStart(acct), params.balInit))

    # println(balReals)

    # balReal = params.balInit
    # for trade in acct.closed
    #     balReal += getPnl(trade)
    # end
    # println(pp((;balReals=balReals[end])))
    return
end

# mapPoint(f, trades, init) = maps(trades, init) do prev, trade
#     return (trad.tsClose(trade), f(prev[2], trade))
# end

# mapPoint2(f, trades, init) = accumulate(trades; init) do prev, trade
#     return (trad.tsClose(trade), f(prev[2], trade))
# end

# mapPoint!(f, buf, trades, init) = maps!(buf, trades; init) do prev, trade
#     return (trad.tsClose(trade), f(prev[2], trade))
# end

function mapPoint(f, trades, init)
    buf = Vector{typeof(init)}(undef, length(trades))
    mapPoint!(f, buf, trades, init)
    return buf
end

mapPoint!(f, buf, trades, init) = CollUtil.accum!(buf, trades; init) do prev, trade
    return (trad.tsClose(trade), f(prev[2], trade))
end

>>>>>>> Stashed changes
#endregion

#region LocalTypes
struct BacktestInfo{T}
    from::DateTime
    to::DateTime
    tsFrom::DateTime
    tsTo::DateTime
    params::T
end

# mutable struct Tracking
#     margins::Vector{MarginInfo}
#     marginMax::MarginInfo
#     bals::Vector{Currency}
#     realBals::Vector{Currency}
# end
#endregion

#region Local
function makeAccount(typeParams, params)
    return Account{typeParams...}(
        params.balInit,
        marginZero(),
        Vector{TradeBT{typeParams...}}(),
        Vector{TradeBT{typeParams[1]}}(),
        1,
    )
end

function makeOps(acct::Account, chain::Ref{Chain})
    return (;
        marginAvail = () -> marginAvail(acct),
        openTrade = (ts, lmso, neto, margin, multiple, label, extra) -> openTrade(acct, ts, lmso, neto, margin, multiple, label, extra),
        # closeTrade = (tradeOpen, lmsc, netc, label) -> closeTrade(acct, tradeOpen, lmsc, netc, label),
    )
end

function handleExpirations(acct::Account, tim::TimeInfo, chain::ChainInfo, otoq)::Nothing
    filter!(acct.open) do tradeOpen
        if tim.date == getExpir(tradeOpen)
            lmsc = tos(LegMetaClose, tradeOpen.lms, otoq)
            netc = Pricing.netExpired(lmsc, chain.under.under)
            # TODO: adjust netExpired to handle buyback for near strikes?
            closeTrade(acct, tradeOpen, tim.ts, lmsc, netc, "expired")
            return false
        end
        return true
    end
    acct.margin = calcMargin(acct.open, acct.bal)
    return
end

function calcMargin(trades::AbstractVector{<:TradeBTOpen}, bal::PT)::MarginInfo
    # Brokerage might consider margin offsets only for the same expiration, but for me, the risk is across all expirations, so I'll count it that way.
    countLong = 0
    countShort = 0
    marginLong = CZ
    marginShort = CZ
    for trade in trades
        long, short = calcMargin(trade)
        multiple = trade.multiple
        long > 0 && ( countLong += multiple )
        short > 0 && ( countShort += multiple )
        marginLong += long * multiple
        marginShort += short * multiple
    end
    marginTotal = max(marginLong, marginShort)
    return MarginInfo(marginTotal, F(marginTotal) / bal, MarginSide(marginLong, countLong), MarginSide(marginShort, countShort))
end

calcMargin(trade::TradeBTOpen) = Pricing.calcMargin(trade.lms)
#endregion

#region Trading
function checkExits(strat, acct::Account, tim, otoq)
    filter!(acct.open) do tradeOpen
        lmsc = tos(LegMetaClose, tradeOpen.lms, otoq)
        label = BackTypes.checkExit(strat, tradeOpen, tim, lmsc)
        if !isnothing(label)
            closeTrade(acct, tradeOpen, tim.ts, lmsc, BackTypes.pricingClose(strat, lmsc), label)
            return false
        end
        return true
    end
end

function openTrade(acct, ts, lmso, neto::PT, margin::Sides{PT}, multiple::Int, label::String, extra)::Nothing
    id = acct.nextTradeId
    acct.nextTradeId += 1
    tradeOpen = TradeBTOpen(id, ts, lmso, neto, margin, multiple, label, extra)
    push!(acct.open, tradeOpen)
    acct.bal += multiple * neto
    # log("Open #$(tradeOpen.id) $(pp((;neto, multiple, strikes=getStrike.(lmso)))), $(pp(extra))")
    log("Open #$(tradeOpen.id) $(Shorthand.sh(lmso)) '$(label)' $(pp((;neto, multiple))), $(pp(extra))")
    return
end

function closeTrade(acct, tradeOpen, ts, lmsc, netc, label)::Nothing
    tradeClose = TradeBTClose(ts, lmsc, netc, label)
    tradeFull = TradeBT(tradeOpen, tradeClose)
    push!(acct.closed, tradeFull)
    acct.bal += tradeOpen.multiple * netc
    pnl = tradeOpen.neto + netc
    log("Close #$(tradeOpen.id) $(Shorthand.sh(lmsc)) '$(label)' $(pp((;neto=tradeOpen.neto, netc, pnl, multiple=tradeOpen.multiple, rate=trad.rate(tradeFull))))")
    return
end
#endregion

#region AccessAndCalcs
module trad
using Dates
using BackTypes, DateUtil
tsClose(trade::TradeBT) = trade.close.ts
pnl(trade) = trade.open.neto + trade.close.netc
# rate(trade) = DateUtil.calcRate(Date(trade.open.ts), Date(trade.close.ts), pnl(trade), max(trade.open.margin))
function rate(trade)
    @show Date(trade.open.ts) Date(trade.close.ts) pnl(trade) max(trade.open.margin)
    DateUtil.calcRate(Date(trade.open.ts), Date(trade.close.ts), pnl(trade), max(trade.open.margin))
end
end

acctTsStart(acct)::DateTime =
    isempty(acct.open) ?
        (isempty(acct.closed) ? DATETIME_ZERO : acct.closed[1].open.ts) :
        (isempty(acct.closed) ? acct.open[1].ts : min(acct.open[1].ts, acct.closed[1].open.ts))

marginMax(acct::Account)::PT = acct.bal * .8 # TODO: make param?
marginAvail(acct)::Sides{PT} = ( mx = marginMax(acct) ; Sides(mx - acct.margin.long.margin, mx - acct.margin.short.margin) )
#endregion

#region Util
log(args...) = LogUtil.logit(:backtest, args...)
#endregion

end