module CmdTrading
using Dates
using Globals, BaseTypes, SH, SmallTypes, StratTypes, StatusTypes, TradeTypes, LegTradeTypes, LegMetaTypes
using DateUtil, LogUtil
using Trading, Quoting
using Calendars, Markets, Expirations, Chains, StoreTrade
using CmdStrats

export so, sor, todo, ct, ctr, drpos, tot, drx
export cleg, clegr

#region NewTrades
using Trading
sor(i::Int, at::Real; kws...) = so(i; kws..., pre=false, at=PriceT(at))
sor(r::Strat, at::Real; kws...) = so(r; kws..., pre=false, at=PriceT(at))
so(i::Int; kws...) = so(tos(LegMeta, ar(i)); kws...)
so(r::Coll{LegRet,4}; kws...) = so(tos(LegMeta, r); kws...)
function so(lms::Coll{LegMeta,4}; ratio=nothing, at=nothing, pre=true, skipConfirm=false)::Int
    canTrade(pre)
    Globals.set(:soRunLast, now(UTC))

    if ( exp = minimum(getExpiration.(lms)) ; exp == market().startDay || exp == today() )
        msg = string("Attempted to enter a trade that expires the same day ", exp)
        pre ? (@error msg) : error(msg)
    end

    isnothing(at) && isnothing(ratio) && (ratio = 0.25)
    pr = priceUse(quoter(lms), sumQuotes(getQuote.(lms)); ratio, at)

    if !pre && !skipConfirm && !confirm()
        println("Aborted.")
        return -1
    end

    tid = pre ? submitPreview(lms, pr) : submitLive(lms, pr, market().curQuot)
    return pre ? 0 : tid
end
#endregion

#region CurrentPosition
using CmdUtil
# TODO: make it a const
cacheTrades = Dict{Int,Trade}()
function todo(ex=0)
    Globals.set(:todoRunLast, now(UTC))
    legOvers = queryLeftovers()
    # if !isnothing(findfirst(leg -> getSide(leg) != Side.long, legOvers))
    #     msg = "Found short leftovers" # $(saveObj(legOvers))"
    #     if Hour(Dates.now()) >= Hour(7)
    #         error(msg)
    #     else
    #         @log warn msg
    #     end
    # end
    println("Leftovers:")
    pretyble(to.(NamedTuple, legOvers))
    @info "Current price: $(market().curp)"
    trades = tradesToClose(ex)
    for trade in trades cacheTrades[getId(trade)] = trade end
    return trades
end

using RetTypes, Between, DrawStrat
toRet(trades, exp)::Ret = combineTo(Ret, trades, exp, market().startPrice, Globals.get(:vtyRatio)) # TODO: choose diff start price?
# TODO: change so matches todo and expirs and all: 0 for today, 1 for non-today next exp, and default is 0
drpos(exp=expir(0)) = drawRet(toRet(tradesToClose(exp), exp), probs(), market().curp, "pos")
export drt, adrt
# drt(i::Int, ex=1) = drawRet(toRet([tradesFor(ex)[i]], ex), probs(), market().curp, "t$(i)")
# adrt(i::Int, ex=1) = drawRet!(toRet([tradesFor(ex)[i]], ex), "t$(i)")
drt(tid::Int) = ( trade = cacheTrades[tid] ; drawRet(toRet([trade], getTargetDate(trade)), probs(), market().curp, "t$(tid)") )
adrt(tid::Int) = ( trade = cacheTrades[tid] ; drawRet!(toRet([trade], getTargetDate(trade)), "t$(tid)") )
drt(trade::Trade) = drawRet(toRet([trade], getTargetDate(trade)); probs=probs(), curp=market().curp, label="t$(getId(trade))")
adrt(trade::Trade) = drawRet!(toRet([trade], getTargetDate(trade)); label="t$(getId(trade))")

function drx(ex=0)
    # TODO: read from cache
    tod = tradesToClose(ex)
    drt(tod[1])
    for i in 2:length(tod)
        adrt(tod[i])
    end
    drawRet!(toRet(tod, expir(ex)); label="all")
end

# drawRet(tradesToRets(todo(ex)))
# calcPosStrat(today(), market().startPrice, Globals.get(:vtyRatio))
#endregion

tot() = findTradeEntered(today())

#region Experiment
# TODO: use this to examine once I have iv data for specific legs
# getMeta.(getLegs(todo()[3]))
# I could look up the ivs in the old db
using LegTypes, OptionMetaTypes, ChainTypes, Rets
lmToRet(lm::LegMeta, om::OptionMeta, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret = makeRet(getLeg(lm), om, bap(lm), forDate, sp, vtyRatio)
tradesToRet(trades::AVec{<:Trade}, forDate::Date, sp::Currency, vtyRatio::Float64)::Ret =
    combineRets([lmToRet(lm, getMeta(optQuoter(lm)), forDate, sp, vtyRatio) for lm in collect(mapFlattenTo(getLegs, LegMeta, trades))])
toRet2(trades, ex)::Ret = tradesToRet(trades, expir(ex), market().startPrice, 1.0) # TODO: choose diff start price?
drpos2(ex=1) = drawRet!(toRet2(todo(ex), ex), "pos2")#, probs(), market().curp, "pos2")
#endregion

ct(trad::Trade{<:Closeable}; kws...) = closePos(trad; kws..., pre=true)
ct(tid::Int; kws...) = ct(cacheTrades[tid]; kws...)
ctr(trad::Trade{<:Closeable}, primitDir::Real; kws...) = closePos(trad; kws..., pre=false, at=PriceT(primitDir))
ctr(tid::Int, primitDir::Real; kws...) = ctr(cacheTrades[tid], primitDir; kws...)
# ctrt(i::Int, at::Real; kws...) = closePos(trad; kws..., pre=false, at=PriceT(at))
# ctt(i::Int; kws...) = ct(todo()[i]); kws..., pre=true)
function closePos(trad::Trade{<:Closeable}; ratio=0.25, at=nothing, pre=true, kws...)
    canTrade(pre)
    if !intest()
        if ( openDate = toDateMarket(tsFilled(trad)) ; openDate == market().startDay || openDate == today() )
            @info "dupe in test" intest()
            msg = string("Attempted to close trade that was opened the same day ", getId(trad), ' ', openDate)
            pre ? (@error msg) : error(msg)
        end
    end
    Globals.set(:ctRunLast, now(UTC))
    qt = quoter(trad, Action.close)
    primit = priceUse(qt; ratio, at)
    @info "Closing $(getId(trad)): $(PRI(string(primit))) resulting in pnl:$(getPrillDirOpen(trad) + primit)"
    closeTrade(optQuoter, trad, primit; kws..., pre)
end
cleg(lid::Int, p=P(0.01); isMkt=false) = ( canTrade(true) ; closeLeg(loadLegTrade(lid), P(p); pre=true, isMkt) )
clegr(lid::Int, p=P(0.01); isMkt=false) = ( canTrade(false) ; closeLeg(loadLegTrade(lid), P(p); pre=false, isMkt) )

export pnls
function pnls()
    trades = findTrades(Closed)
    sort!(trades; by=getTargetDate)
    bal = PriceT(0.0)
    tbl = map(trades) do trade
        pnl = getPnl(trade)
        bal += pnl
        (;closed=getTargetDate(trade), tid=getId(trade), pnl, bal)
    end
    pretyble(tbl)
end

#region Local
function canTrade(pre::Bool)
    snapped = !isnothing(snap())
    intest() && snapped && return true
    snapped && error("Don't trade when snapped")
    # NOTE: maybe allow anything if tenv() === :paper
    # These checks will also fail if snapped
    isMarketOpen() || error("Only make trades within market hours")
    Markets.canTrade()
    Chains.canTrade()
    !dev() || error("Don't make trades in devMode")

end

using BaseTypes, QuoteTypes, OutputUtil
# TODO: separate output part from calcs and move calc elsewhere
function priceUse(qt, orig=nothing; ratio=nothing, at=nothing)
    if isnothing(at)
        # pr1 = improve(qt, ratio)
        pr1 = improve(orig, ratio)
    else
        pr1 = at
    end
    ratioActual = (pr1 - getBid(qt)) / (getAsk(qt) - getBid(qt))
    pr = round(PriceT, pr1)
    orig = isnothing(orig) ? "" : "(was: $(SEC(string(orig))))"
    @info "Using: $(PRI(string(pr))) = $(ratioActual) * $(qt) $(orig)"
    return pr
end
#endregion

end