module CmdTrading
using Dates
using Globals, BaseTypes, SH, SmallTypes, StratTypes, StatusTypes, TradeTypes, LegTradeTypes, LegMetaTypes
using DateUtil
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
using CmdUtil, CheckUtil
function todo(ex=1)
    Globals.set(:todoRunLast, now(UTC))
    legOvers = queryLeftovers()
    @assert isnothing(findfirst(leg -> getSide(leg) != Side.long, legOvers)) "Found short leftovers $(setObj(legOvers))"
    println("Leftovers:")
    pretyble(to.(NamedTuple, legOvers))
    @info "Current price: $(market().curp)"
    return tradesToClose(ex)
end

using RetTypes, Between, DrawStrat
toRet(trades, ex)::Ret = combineTo(Ret, trades, expir(ex; td=true), market().startPrice, Globals.get(:vtyRatio)) # TODO: choose diff start price?
# TODO: change so matches todo and expirs and all: 0 for today, 1 for non-today next exp, and default is 0
drpos(ex=1) = drawRet(toRet(tradesFor(ex), ex), probs(), market().curp, "pos")
export drt, adrt
drt(i::Int, ex=1) = drawRet(toRet([tradesFor(ex)[i]], ex), probs(), market().curp, "t$(i)")
drt(trade::Trade, ex=1, i=0) = drawRet(toRet([trade], ex), probs(), market().curp, "t$(i)")
adrt(i::Int, ex=1) = drawRet!(toRet([tradesFor(ex)[i]], ex), "t$(i)")
adrt(trade::Trade, ex=1, i=0) = drawRet!(toRet([trade], ex), "t$(i)")
function drx(ex=1)
    tod = tradesToClose(ex)
    drt(tod[1], ex, 1)
    for i in 2:length(tod)
        adrt(tod[i], ex, i)
    end
    drawRet!(toRet(tod, ex), "all")
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
toRet2(trades, ex)::Ret = tradesToRet(trades, expir(ex; td=true), market().startPrice, 1.0) # TODO: choose diff start price?
drpos2(ex=1) = drawRet!(toRet2(todo(ex), ex), "pos2")#, probs(), market().curp, "pos2")
#endregion

ctr(trad::Trade{Filled}, at::Real; kws...) = closePos(trad; kws..., pre=false, at=PriceT(at))
ct(trad::Trade{Filled}; kws...) = closePos(trad; kws..., pre=true)
# ctrt(i::Int, at::Real; kws...) = closePos(trad; kws..., pre=false, at=PriceT(at))
# ctt(i::Int; kws...) = ct(todo()[i]); kws..., pre=true)
function closePos(trad::Trade{Filled}; ratio=0.25, at=nothing, pre=true, kws...)
    canTrade(pre)
    if !intest()
        if ( openDate = toDateLocal(tsFilled(trad)) ; openDate == market().startDay || openDate == today() )
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

using FileUtil, TradierAccount
export backupOrders
function backupOrders()
    ords = [(d["id"], d) for d in tradierOrders()]
    foreach(ords) do (id, d)
        writeJson(dirData("bak/orders/$(id).json"), d)
    end
end

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
        pr1 = improve(qt, ratio)
    else
        pr1 = at
        ratio = (at - getBid(qt)) / (getAsk(qt) - getBid(qt))
    end
    pr = round(PriceT, pr1)
    orig = isnothing(orig) ? "" : "(was: $(SEC(string(orig))))"
    @info "Using: $(PRI(string(pr))) = $(ratio) * $(qt) $(orig)"
    return pr
end
#endregion

end