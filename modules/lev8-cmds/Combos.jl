module Combos
using Dates, IterTools, NamedTupleTools
using SH, BaseTypes, SmallTypes, ChainTypes
using Caches, DateUtil, DictUtil, OutputUtil, LogUtil
using TradierData
using Markets, Expirations, Chains
import SeekingAlpha

export c
const c = @__MODULE__

maxDate() = today() + Year(10)
minDate() = today() + Day(3)

csa(sym) = checkSymAll(x -> x.mov >= 0.05 && x.rate >= 0.5, sym)
checkSymAll(pred, sym) =  sort!([(;x.expr, x.mov, x.rate, x.strike, x.primitDir) for x in sort(filter(x-> x.sym == sym && pred(x), LookedRaw); by=x->x.rate)]; by=x->x.rate)

using ThreadPools
function lookAll(cands=SeekingAlpha.getCandidates(); ratio=.95)
    prout("Start proc")
    _, looked = logqbmap(cands) do sym
        try
            prout("Combos processing", sym)
            return look(sym; ratio)
        catch e
            prerr(e)
            return
        end
    end
    global LookedOrig = filter!(!isempty, looked)
    disp()
    return nothing
end
function disp()
    global Looked = copy(LookedOrig)
    prep(Looked)
    pretyble(delete.(Looked, :oq))
end

# disp(minMove=0.0) = pretyble(filter!(x -> x.mov > minMove, sort!(delete.(Looked, :oq); by=x -> x.rate)))
check(sym, minMove=0.0) = pretyble(filter!(x -> x.mov > minMove, sort!(delete.(look(sym; all=true), :oq); by=x -> x.rate)))

excludes(lll) = filter!(x -> !(x.sym in ActiveSyms) && !(x.sym in SeekingAlpha.BadPricing) && !(x.sym in TempIgnore) && !(x.sym in SeekingAlpha.Ignore) && x.strike <= 105.0, lll)
function clean(lll)
    # excludes(lll)
    global Looked = sort!(lll; rev=true, by=x->x.rate)
    # return sort!(map(g -> g[findmax(x -> x.rate, g)[2]], groupby(r -> r.sym, sort!(lll; by=x->x.sym))); rev=true, by=x->x.rate)
end

function prep(grouped)
    # global grouped = collect(groupby(r-> r.sym, res))
    # sort!.(grouped; rev=true, by=x->x.rate)
    for group in grouped
        sort!(group; rev=true, by=x->x.rate)
    end
    global trunced = first.(grouped, 4)
    sort!(trunced; by=x->bestRate(x))
    for trunc in trunced
        sort!(trunc; rev=false, by=x->x.rate)
    end
    global Looked = collect(Iterators.flatten(trunced))
    return Looked
end

bestRate(v) = findmax(x -> x.rate, v)

function getChains(sym::String; age=Minute(10))::ChainsType
    mnd, mxd = (minDate(), maxDate())
    xprs = filter(x -> mnd <= x <= mxd, expirs(sym))
    return CH.chains(xprs, (sym,); age)[sym]
end
function stock(sym::String; age=Minute(10))::Dict{String,Any}
    return cache!(Dict{String,Any}, Symbol("stock-", sym), age) do
        return tradierQuote(sym)
    end
end

const DF = dateformat"mm/dd/yyyy"
parseDate(seconds::Int) = unix2datetime(seconds)
parseDate(strDate::String) = strDate == "-" ? maxDate() : ( date = Date(strDate, DF) ; date >= today() ? date : maxDate() )
function parseDate(d::Dict, k::String)
    haskey(d, k) ? parseDate(d[k]) : return maxDate()
end

sa = SeekingAlpha
import TradierData:findEarnDate,findExDate
using Between
function look(sym; all=false, ratio)
    try
    res = []
    # about = SeekingAlpha.Cands[sym]
    # about = SeekingAlpha.Data[:metrics]
    # maxDate = min(parseDate(about[:earningsUpcomingAnnounceDate]), parseDate(about[Symbol("dividendsEx-DivDate")]))
    # maxDate = min(parseDate(about, "earning_announce_date"), parseDate(about, "div_pay_date"))
    # maxDate = min(findExDate(tryKey(sa.Dividends, sym)), findEarnDate(tryKey(sa.Earnings,sym)))
    # maxDate = maxDate() # min(findExDate(get(sa.Dividends, sym, nothing)), findEarnDate(get(sa.Earnings, sym, nothing)))
    chs = getChains(sym)
    # locUnder = Under[sym]
    locUnder = stock(sym)
    for xpir in sort!(collect(keys(chs)))
        # expr < maxDate || break # exprs sorted asc
        oqs = filter(isPut, chs[xpir].chain)
        timult = DateUtil.timult(xpir)
        underBid = locUnder["bid"]
        if all
            range = 1:length(oqs)
        else
            # TODO: use IV to figure out how far out to go?
            # or could maybe get 52 week range
            # ratio = .94
            # ratio = .96
            startI = findfirst(oq -> getStrike(oq) > ratio * underBid, oqs)
            !isnothing(startI) || continue
            # range = (startI-5):(startI-1)
            range = (startI-10):(startI-1)
        end
        for i in range
            i >= 1 || continue
            oq = oqs[i]
            getBid(oq) >= 0.05 || continue
            strike = getStrike(oq)
            primitDir = bap(oq)
            rate = timult * (primitDir - .0065) / strike # .0065 is the trade commission for fidelity
            # println("$(sym)[$(expr)]: $(underBid) -> $(strike) at $(primitDir) ($(getQuote(oq))) / $(strike) = $(rate)")
            if all || rate > 0.1
                push!(res, (;sym, xpir, mov=1.0 - strike/underBid, underBid, strike, primitDir, rate, div=getDividend(sym), oq))
            end
        end
    end
    return res
    catch e
        @error "look" sym
        rethrow(e)
    end
end

function getDividend(sym)
    haskey(sa.Dividends, sym) || return nothing
    div = sa.Dividends[sym]
    length(div) > 0 || return nothing
    d = filter(x -> Date(x["pay_date"]) >= today() - Year(1), values(sa.Dividends[sym]))
    length(d) > 0 || return nothing
    divTot = sum(x -> x["cash_amount"], d)
    last = sa.Quotes[sym]["last"]
    return divTot / last
end

function vol(sym)
    volTot = 0
    opintTot = 0
    for expr in keys(RawChainMap[sym])
        vol = 0
        opint = 0
        for tq in RawChainMap[sym][expr]
            vol += tq["average_volume"]
            opint += tq["open_interest"]
        end
        # println("$(sym) $(expr): $(vol) volume $(opint) opint")
        volTot += vol
        opintTot += opint
    end
    return opintTot
end

import Calendars
function findRoll(sym, at, cost=0.0, style=Style.put, improve=false)
    res = NamedTuple[]
    getChains(sym)
    # locUnder = Under[sym]
    locUnder = stock(sym)
    chs = getChains(sym)
    for expr in sort!(collect(keys(chs)))
        println("Checking ", expr)
        oqs = filter(x->getStyle(x) == style, chs[expr].chain)
        timult = DateUtil.timult(expr)
        underBid = locUnder["bid"]
        for oq in oqs
            strike = getStrike(oq)
            !improve || strike <= at || continue
            # strike == at || continue
            strikeDiff = abs(strike - at)
            strikeDiff <= 5.3 || continue
            getBid(oq) > 0.0 || ( println("skipping non-positive bid") ; continue )
            entry = bap(oq, .2)
            # reduct = style == Style.put ? max(0.0, strike - at) : max(0.0, strike - at)
            primitDir = entry - cost - max(0.0, Int(style) * (at - strike)) # (strike > at ? (strike - at) : 0.0)
            # @show primitDir cost
            rate = timult * (primitDir - .0065) / strike # .0065 is the trade commission for fidelity / 100
            # println("$(sym)[$(expr)]: $(underBid) -> $(strike) at $(primitDir) ($(getQuote(oq))) / $(strike) = $(rate)")
            # if rate > 0.0
                push!(res, (;sym, expr, mov=1.0 - strike/underBid, underBid, strike, primitDir, entry, rate, div=getDividend(sym)))
            # end
        end
    end
    # pretyble(filter!(x -> x.mov > minMove, sort!(delete.(res, :oq); by=x -> x.rate)))
    pretyble(sort!(res; by=x->x.rate); dateYear=true)
    # return res
    return
end

# using Combos
# c = Combos
# c.lookAll(keys(sa.Cands))

end