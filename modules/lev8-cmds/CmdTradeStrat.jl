module CmdTradeStrat
using Dates
using SH, BaseTypes, BackTypes, OptionTypes, TradeTypes, LegMetaTypes
using LogUtil, DateUtil, ChainUtil
using Expirations, Chains, Markets, StoreTrade
import StratButter as stratb
import TradierAccount, StoreTrade

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

    strat = stratb.makeStrat()

    mkt = market()
    tim = makeTim()
    xpirs = stratb.filterXpirs(expirs(), tim.date, strat.params)
    ci = toChainInfo(chains(xpirs)["SPY"], mkt.curp, xpirs)
    otoq = ChainUtil.toOtoq(ci.xsoqs)

    checkExits(strat, tim, otoq, mkt.curp)
    strat(makeOps(), tim, ci, otoq, F(mkt.vix))

    if isnothing(keepOpen)
        global keepOpen = keep
        return (nothing, nothing)
    else
        return keepOpen.lmso, keepOpen.neto
    end
end

function checkExits(strat, tim, otoq, curp)
    for tradeOpen in StoreTrade.tradesLive()
        lmsc = tosn(LegMetaClose, Tuple(getLegs(tradeOpen)), otoq)
        !isnothing(lmsc) || ( println("couldn't quote") ; return true ) # skip if can't quote
        label = BackTypes.checkExit(strat.params, tradeOpen, tim, lmsc, curp)
        if !isnothing(label)
            closeTrade(tradeOpen, tim.ts, lmsc, BackTypes.pricingClose(strat, lmsc), label)
        end
    end
end

using Globals
function makeTim()
    ts = dev() ? market().tsMarket : now(UTC)
    date = dev() ? market().startDay : Date(ts)
    return (;ts, date)
end

import Pricing
import LinesLeg as LL
# SH.getRisk(tradeOpen::Trade) = tradeOpen.extra.risk
# TODO: remove Tuple?
SH.getRisk(tradeOpen::Trade) = -Pricing.calcCommit(LL.toSegments(Tuple(getLegs(tradeOpen))))

function makeOps()
    tierBals = TradierAccount.tradierBalances()
    marginInfo = tierBals["margin"]
    if marginInfo["fed_call"] != 0 || marginInfo["maintenance_call"] != 0
        error("Margin call on account detected: fed: $(marginInfo["fed_call"]), maintenance: $(marginInfo["maintenance_call"])")
    end
    buyingPower = marginInfo["option_buying_power"] / 100
    return (;
        marginAvail = () -> Sides(buyingPower, buyingPower),
        bal = () -> C(tierBals["total_cash"] / 100),
        tradesOpen = () -> StoreTrade.tradesLive(),
        openTrade, # = (ts, lmso, neto, margin, multiple, label, extra) -> openTrade(acct, ts, lmso, neto, margin, multiple, label, extra),
        closeTrade, # = (tradeOpen, ts, lmsc, netc, label) -> ( closeTrade(acct, tradeOpen, ts, lmsc, netc, label) ; del!(acct.open, tradeOpen) ),
        canOpenPos,
    )
end

function openTrade(ts, lmso, neto, margin, multiple, label, extra)
    global keepOpen = (;ts, lmso, neto, margin, multiple, label, extra)
    println("openTrade: ", (;ts, lmso, neto, margin, multiple, label, extra))
end

function closeTrade(tradeOpen, ts, lmsc, netc, label)
    global keepClose = (;tradeOpen, ts, lmsc, netc, label)
    println("closeTrade: ", (;tradeOpen, ts, lmsc, netc, label))
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

import Positions, LegTypes
function canOpenPos(oq::Option, side::Side.T)
    # TODO: check current open orders also
    poss = Positions.positions()
    for pos in poss
        if LegTypes.isConflict(pos.leg, oq, side)
            # println("Found conflict:\n", pos.leg, "\n", side, ", ", oq)
            return false
        end
    end
    return true
end

end