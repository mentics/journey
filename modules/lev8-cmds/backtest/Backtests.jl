module Backtests
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
using LogUtil, DateUtil, ChainUtil
using SimpleStore
import SimpleStore as SS
using Between

#region Public
# function run(strat::Strat, from::Int, to::Int; maxSeconds::Int=1)::Nothing
#     SS.getTss()
# end
function run(strat::Strat, from::DateLike, to::DateLike; maxSeconds::Int=1)::Nothing
    LogUtil.resetLog(:backtest)

    params = strat.params
    global tss = SS.getTss(from, to)
    global info = BacktestInfo(from, to, tss[1], tss[end], params)
    log("Starting backtest info ", info)
    # $(from) - $(to) params:\n", params
    acct = makeAccount(params)
    global keepAcct = acct
    ops = makeOps(acct)

    SS.run(from, to; maxSeconds) do tim, chain
        if tim.lastOfDay
            handleExpirations(acct, tim, chain)
        else
            strat(ops, tim, chain)
        end
    end
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
function makeAccount(params)
    return Account(
        params.balInit,
        Vector{TradeBT}(),
        Vector{TradeBT}(),
        1,
    )
end

function makeOps(acct::Account)
    return (;
        openTrade = (lmso, neto, multiple, label) -> openTrade(acct, lmso, neto, multiple, label),
        # closeTrade = (tradeOpen, lmsc, netc, label) -> closeTrade(acct, tradeOpen, lmsc, netc, label),
        checkExit = (f, args...) -> checkExit(f, acct, args...),
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
function checkExit(f, acct::Account, optToOq, args...)
    filter!(acct.open) do tradeOpen
        label = f(tradeOpen, args...)
        if !isnothing(label)
            lmsc = tos(LegMetaClose, tradeOpen.lmso, optToOq)
            closeTrade(acct, tradeOpen, lmsc, pricingClose(lmsc), label)
        end
    end
end

function openTrade(acct, lmso, neto, multiple, label)
    id = acct.nextTradeId
    acct.nextTradeId += 1
    tradeOpen = TradeBTOpen(id, lmso, neto, label)
    push!(acct.open, tradeOpen)
    acct.bal += multiple * neto
    log("Open #$(trade.id) $(Shorthand.tosh(lms, acct.xpirs)): $((;multiple, neto, basisAll, netVal, riskVal)) '$(label)'")
end

function closeTrade(acct, tradeOpen, lmsc, netc, label)
    tradeClose = TradeBTClose(lmsc, netc, label)
    tradeFull = TradeBT(tradeOpen, tradeClose)
    push!(acct.trades, tradeFull)
    acct.bal += tradeOpen.multiple * netc
end
#endregion

#region Util
log(args...) = LogUtil.logit(:backtest, args...)
#endregion

end