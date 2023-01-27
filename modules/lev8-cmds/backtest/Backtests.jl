module Backtests
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
using LogUtil, DateUtil, ChainUtil, CollUtil
using SimpleStore
import SimpleStore as SS
using Between
import OutputUtil:pp

#region Public
# function run(strat::Strat, from::Int, to::Int; maxSeconds::Int=1)::Nothing
#     SS.getTss()
# end
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
            handleExpirations(acct, tim, chain)
        end
    end
    showResult()
end

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

module pts
import ..Currency, ..TradeBT, ..trad
balReal(prev::Currency, trade::TradeBT) = prev + trad.pnl(trade)
end # module pts

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
        Vector{TradeBT{typeParams...}}(),
        Vector{TradeBT{typeParams[1]}}(),
        1,
    )
end

function makeOps(acct::Account, chain::Ref{Chain})
    return (;
        openTrade = (lmso, ts, neto, multiple, label, extra) -> openTrade(acct, ts, lmso, neto, multiple, label, extra),
        # closeTrade = (tradeOpen, lmsc, netc, label) -> closeTrade(acct, tradeOpen, lmsc, netc, label),
    )
end

function handleExpirations(acct::Account, tim::TimeInfo, chain::ChainInfo)::Nothing
    filter!(acct.open) do tradeOpen
        if tim.date == getExpir(tradeOpen)
            lmsc = tos(LegMetaClose, tradeOpen.lms, optToOq)
            netc = OptionUtil.netExpired(lmsc, chain.under)
            # TODO: adjust netExpired to handle buyback for near strikes?
            closeTrade(acct, tradeOpen, tim.ts, lmsc, netc, "expired")
            return false
        end
        return true
    end
    recalcMargin(acct)
end

function recalcMargin(acct::Account)
end
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

import Shorthand

function openTrade(acct, ts, lmso, neto, multiple, label, extra)::Nothing
    id = acct.nextTradeId
    acct.nextTradeId += 1
    tradeOpen = TradeBTOpen(id, ts, lmso, neto, multiple, label, extra)
    push!(acct.open, tradeOpen)
    acct.bal += multiple * neto
    # log("Open #$(tradeOpen.id) $(pp((;neto, multiple, strikes=getStrike.(lmso)))), $(pp(extra))")
    log("Open #$(tradeOpen.id) $(Shorthand.sh(lmso)) $(pp((;neto, multiple))), $(pp(extra))")
    return
end

function closeTrade(acct, tradeOpen, ts, lmsc, netc, label)::Nothing
    tradeClose = TradeBTClose(ts, lmsc, netc, label)
    tradeFull = TradeBT(tradeOpen, tradeClose)
    push!(acct.closed, tradeFull)
    acct.bal += tradeOpen.multiple * netc
    pnl = tradeOpen.neto + netc
    log("Close #$(tradeOpen.id) $(Shorthand.sh(lmsc)) $(pp((;neto=tradeOpen.neto, netc, pnl, multiple=tradeOpen.multiple)))")
    return
end
#endregion

#region AccessAndCalcs
module trad
using BackTypes
tsClose(trade::TradeBT) = trade.close.ts
pnl(trade) = trade.open.neto + trade.close.netc
end

acctTsStart(acct)::DateTime =
    isempty(acct.open) ?
        (isempty(acct.closed) ? DATETIME_ZERO : acct.closed[1].open.ts) :
        (isempty(acct.closed) ? acct.open[1].ts : min(acct.open[1].ts, acct.closed[1].open.ts))

# acctTsStart2(acct) = acct.closed[1].open.ts
acctTsStart2(acct) = acct.open[1].ts

function acctTsStart3(acct)::DateTime
    @time a = acct.closed[1]
    @time b = a.open
    @time c = b.ts
    return c
end

    # isempty(acct.open) ?
    #     (isempty(acct.closed) ? DATETIME_ZERO : acct.closed[1].open.ts) :
    #     (isempty(acct.closed) ? acct.open[1].ts : min(acct.open[1].ts, acct.closed[1].open.ts))

#endregion

#region Util
log(args...) = LogUtil.logit(:backtest, args...)
#endregion

end