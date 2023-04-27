module CmdTradeStrat
using Dates
using SH, BaseTypes, BackTypes
using LogUtil, DateUtil
using Expirations, Chains, Markets
import StratButter as stratb
import SimpleStore as sstor

# TODO: move ChainInfo to shared place for live and backtest?
# struct ChainInfo # <: Chain
#     xsoqs::Dict{Date,Styles{Vector{OptionQuote}}}
#     under::UnderTime
#     xpirs::Vector{Date}
# end

using ChainTypes, SmallTypes
function toXsoqs(cht::ChainsType)::Dict{Date,Styles{Vector{OptionQuote}}}
    xsoqs = Dict{Date,Styles{Vector{OptionQuote}}}()
    for (xpir, ch) in cht
        calls = filter(isCall, ch.chain)
        puts = filter(isPut, ch.chain)
        xsoqs[xpir] = Styles(calls, puts)
    end
    return xsoqs
end

function toChainInfo(cht::ChainsType, curp, xpirs)
    under = sstor.UnderTime(curp, curp, curp, curp)
    xsoqs = toXsoqs(cht)
    return sstor.ChainInfo(xsoqs, under, xpirs)
end

keepOpen = nothing

function run()
    LogUtil.resetLog(:backtest)
    keep = keepOpen
    global keepOpen = nothing

    mkt = market()
    strat = stratb.makeStrat()
    tim = makeTim()
    xpirs = stratb.filterXpirs(expirs(), tim.date, strat.params)
    ci = toChainInfo(chains(xpirs)["SPY"], mkt.curp, xpirs)
    BackTypes.resetStrat(strat)
    strat(makeOps(nothing), tim, ci, F(mkt.vix))
    if isnothing(keepOpen)
        global keepOpen = keep
        return nothing
    else
        return keepOpen.lmso
    end
end

using Globals
function makeTim()
    ts = dev() ? market().tsMarket : now(UTC)
    date = dev() ? market().startDay : Date(ts)
    return (;ts, date)
end

function makeOps(acct)
    return (;
        marginAvail = () -> Sides(1000, 1000), # ac.marginAvail(acct),
        bal = () -> C(1000), # acct.bal,
        tradesOpen = () -> acct.open,
        openTrade = (ts, lmso, neto, margin, multiple, label, extra) -> openTrade(acct, ts, lmso, neto, margin, multiple, label, extra),
        closeTrade = (tradeOpen, ts, lmsc, netc, label) -> ( closeTrade(acct, tradeOpen, ts, lmsc, netc, label) ; del!(acct.open, tradeOpen) )
    )
end

function openTrade(acct, ts, lmso, neto, margin, multiple, label, extra)
    global keepOpen = (;acct, ts, lmso, neto, margin, multiple, label, extra)
    println("openTrade: ", (;acct, ts, lmso, neto, margin, multiple, label, extra))
end

function closeTrade(acct, tradeOpen, ts, lmsc, netc, label)
    global keepClose = (;acct, ts, lmso, neto, margin, multiple, label, extra)
    println("closeTrade: ", (;acct, tradeOpen, ts, lmsc, netc, label))
end

import ProbKde, HistData
function checkScore(lms)
    strat = stratb.makeStrat()
    (;tsMarket, curp, vix) = market()
    prob = ProbKde.probToClose(F(curp), F(vix), tsMarket, getExpir(lms))
    tmult = timult(Date(tsMarket), getExpir(lms))
    return stratb.score(lms, strat.params, prob, curp, tmult)
end

function getProb(xpir)
    (;tsMarket, curp, vix) = market()
    return ProbKde.probToClose(F(curp), F(vix), tsMarket, xpir)
end

end