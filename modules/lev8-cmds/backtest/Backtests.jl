module Backtests
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
using LogUtil, DateUtil, ChainUtil
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
    acct = makeAccount(params)
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
    balReal = params.balInit
    for trade in acct.closed
        balReal += getPnl(trade)
    end
    println(pp((;balReal)))
    return
end

getPnl(trade) = trade.open.neto + trade.close.netc
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
function makeAccount(params)
    return Account(
        params.balInit,
        Vector{TradeBT}(),
        Vector{TradeBT}(),
        1,
    )
end

function makeOps(acct::Account, chain::Ref{Chain})
    return (;
        openTrade = (lmso, neto, multiple, label, extra) -> openTrade(acct, lmso, neto, multiple, label, extra),
        # closeTrade = (tradeOpen, lmsc, netc, label) -> closeTrade(acct, tradeOpen, lmsc, netc, label),
    )
end

function handleExpirations(acct::Account, tim::TimeInfo, chain::ChainInfo)::Nothing
    filter!(acct.open) do tradeOpen
        if tim.date == getExpir(tradeOpen)
            lmsc = tos(LegMetaClose, tradeOpen.lms, optToOq)
            netc = OptionUtil.netExpired(lmsc, chain.under)
            # TODO: adjust netExpired to handle buyback for near strikes?
            closeTrade(acct, tradeOpen, lmsc, netc, "expired")
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
            closeTrade(acct, tradeOpen, lmsc, BackTypes.pricingClose(strat, lmsc), label)
            return false
        end
        return true
    end
end

import Shorthand

function openTrade(acct, lmso, neto, multiple, label, extra)::Nothing
    id = acct.nextTradeId
    acct.nextTradeId += 1
    tradeOpen = TradeBTOpen(id, lmso, neto, multiple, label, extra)
    push!(acct.open, tradeOpen)
    acct.bal += multiple * neto
    # log("Open #$(tradeOpen.id) $(pp((;neto, multiple, strikes=getStrike.(lmso)))), $(pp(extra))")
    log("Open #$(tradeOpen.id) $(Shorthand.sh(lmso)) $(pp((;neto, multiple))), $(pp(extra))")
    return
end

function closeTrade(acct, tradeOpen, lmsc, netc, label)::Nothing
    tradeClose = TradeBTClose(lmsc, netc, label)
    tradeFull = TradeBT(tradeOpen, tradeClose)
    push!(acct.closed, tradeFull)
    acct.bal += tradeOpen.multiple * netc
    pnl = tradeOpen.neto + netc
    log("Close #$(tradeOpen.id) $(Shorthand.sh(lmsc)) $(pp((;neto=tradeOpen.neto, netc, pnl, multiple=tradeOpen.multiple)))")
    return
end
#endregion

#region Util
log(args...) = LogUtil.logit(:backtest, args...)
#endregion

end