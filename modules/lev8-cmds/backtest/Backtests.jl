module Backtests
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
using LogUtil, DateUtil, ChainUtil, CollUtil
using SimpleStore, BacktestUtil
import SimpleStore as SS
import Pricing, Between, Shorthand
import OutputUtil:pp

#region Public
function run(strat::Strat, from::DateLike, to::DateLike; maxSeconds::Int=1)::Nothing
    @time begin
    LogUtil.resetLog(:backtest)

    params = strat.params
    global keepParams = params
    global tss = SS.getTss(from, to)
    global info = BacktestInfo(from, to, tss[1], tss[end], params)
    @blog "Starting backtest info" info
    # $(from) - $(to) params:\n", params
    acct = makeAccount(strat.acctTypes, params)
    global keepAcct = acct
    # chainRef = Ref{Chain}()
    ops = makeOps(acct)
    BackTypes.resetStrat(strat)

    SS.run(from, to; maxSeconds) do tim, chain
        otoq = ChainUtil.toOtoq(chain)
        if !tim.atClose
            @blog "Strat running" ts=tim.ts curp=getCurp(chain) bal=acct.bal margin=acct.margin
            checkExits(strat, acct, tim, otoq, getCurp(chain))
            strat(ops, tim, chain, otoq)
        end
        if tim.lastOfDay
            handleExpirations(strat, acct, tim, chain, otoq)
            verifyMargin(acct)
        end
        yield()
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

function makeOps(acct::Account)
    return (;
        marginAvail = () -> ac.marginAvail(acct),
        bal = () -> acct.bal,
        tradesOpen = () -> acct.open,
        openTrade = (ts, lmso, neto, margin, multiple, label, extra) -> openTrade(acct, ts, lmso, neto, margin, multiple, label, extra),
        closeTrade = (tradeOpen, ts, lmsc, netc, label) -> ( closeTrade(acct, tradeOpen, ts, lmsc, netc, label) ; del!(acct.open, tradeOpen) )
    )
end

function handleExpirations(strat, acct::Account, tim::TimeInfo, chain::ChainInfo, otoq)::Nothing
    filter!(acct.open) do tradeOpen
        if tim.date == getExpir(tradeOpen)
            lmsc = tos(LegMetaClose, tradeOpen.lms, Pricing.fallbackExpired(getCurp(chain), otoq))
            # lmsc = tos(LegMetaClose, tradeOpen.lms, otoq)
            # if isnothing(lmsc)
            #             return OptionQuote(o, Quote(C(Pricing.netExpired(getStyle(o), getStrike(o), curp))), OptionMeta())
            # end

            if BackTypes.hasMultExpirs(strat)
                netc = BackTypes.pricingClose(strat, lmsc)
            else
                netc = Pricing.netExpired(lmsc, getCurp(chain))
            end

            # TODO: adjust netExpired to handle buyback for near strikes?
            closeTrade(acct, tradeOpen, tim.ts, lmsc, netc, "expired")
            return false
        end
        return true
    end
    return
end

function verifyMargin(acct::Account)
    # Recalc margin each day from scratch and make sure the intraday calcs were reasonable
    margin = calcMargin(acct.open, acct.bal)
    if abs(acct.margin.margin - margin.margin) > 0.01 || abs(acct.margin.marginBalRat - margin.marginBalRat > 0.01)
        println("margin didn't match")
        @blog "margin didn't match" margin acct.margin
    end
    acct.margin = margin
end

getCurp(chain::ChainInfo) = chain.under.under

function calcMargin(trades::AbstractVector{<:TradeBTOpen}, bal::PT)::MarginInfo
    # Brokerage might consider margin offsets only for the same expiration, but for me, the risk is across all expirations, so I'll count it that way.
    # countLong = 0
    # countShort = 0
    marginLong = CZ
    marginShort = CZ
    for trade in trades
        multiple = trade.multiple
        # margAdd = Pricing.calcMargin(trade.lms)
        margAdd = trade.margin
        marginLong += multiple * margAdd.long
        marginShort += multiple * margAdd.short
        # side = trad.side(trade)
        # if side == Side.long
        #     marginLong += margAdd
        #     countLong += multiple
        # else
        #     marginShort += margAdd
        #     countShort += multiple
        # end
    end
    marginTotal = max(marginLong, marginShort)
    return MarginInfo(marginTotal, F(marginTotal) / bal, MarginSide(marginLong, 0), MarginSide(marginShort, 0))
end

# calcMargin(trade::TradeBTOpen) = trade.multiple * Pricing.calcMargin(trade.lms)
#endregion

#region Trading
function checkExits(strat, acct::Account, tim, otoq, curp)
    filter!(acct.open) do tradeOpen
        lmsc = nothing
        # try
            lmsc = tosn(LegMetaClose, tradeOpen.lms, otoq)
            !isnothing(lmsc) || ( println("couldn't quote") ; return true ) # skip if can't quote
            label = BackTypes.checkExit(strat.params, tradeOpen, tim, lmsc, curp)
            if !isnothing(label)
                closeTrade(acct, tradeOpen, tim.ts, lmsc, BackTypes.pricingClose(strat, lmsc), label)
                return false
            end
        # catch e
        #     if e isa KeyError
        #         println("Could not quote")
        #     else
        #         rethrow(e)
        #     end
        # end
        return true
    end
end

function openTrade(acct, ts, lmso, neto::PT, margin::Sides{PT}, multiple::Int, label::String, extra)::Nothing
    id = acct.nextTradeId
    acct.nextTradeId += 1
    tradeOpen = TradeBTOpen(id, ts, lmso, neto, margin, multiple, label, extra)
    push!(acct.open, tradeOpen)
    acct.bal += multiple * neto
    acct.margin = ac.marginAdd(acct.bal, acct.margin, multiple, margin) # Pricing.calcMargin(tradeOpen.lms))
    # blog("Open #$(tradeOpen.id) $(pp((;neto, multiple, strikes=getStrike.(lmso)))), $(pp(extra))")
    @blog "Open $(label) #$(tradeOpen.id) $(Shorthand.sh(lmso))" neto multiple dmargin=(-max(margin)) extra
    return
end

function closeTrade(acct, tradeOpen, ts, lmsc, netc, label)::Nothing
    tradeClose = TradeBTClose(ts, lmsc, netc, label)
    trade = TradeBT(tradeOpen, tradeClose)
    push!(acct.closed, trade)
    acct.bal += tradeOpen.multiple * netc
    pnl = tradeOpen.neto + netc
    acct.margin = ac.marginAdd(acct.bal, acct.margin, -tradeOpen.multiple, tradeOpen.margin) # Pricing.calcMargin(tradeOpen.lms))
    @blog "Close $(label) #$(tradeOpen.id) $(Shorthand.sh(lmsc))" neto=tradeOpen.neto netc pnl multiple=tradeOpen.multiple rate=trad.rate(trade) dmargin=max(tradeOpen.margin) openDur=trad.openDur(trade) bdaysLeft=trad.bdaysLeft(Date(ts), trade)
    return
end
#endregion

BaseTypes.toPT(sides::Sides{Float64})::Sides{PT} = Sides(toPT(sides.long, RoundDown), round(toPT(sides.short, RoundDown)))

end