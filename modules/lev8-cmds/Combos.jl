module Combos
using Dates, IterTools, NamedTupleTools
using SH, BaseTypes, SmallTypes, ChainTypes
using DateUtil, DictUtil, OutputUtil
using TradierData
using Markets, Chains
import SeekingAlpha

function lookAll(cands=SeekingAlpha.getCandidates(); ratio=.96)
    global Looked = []
    for sym in cands
        append!(Looked, look(sym; ratio))
    end
    global LookedRaw = copy(Looked)
    disp()
    return nothing
end
function disp()
    prep(copy(LookedRaw))
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

function prep(res)
    global grouped = collect(groupby(r-> r.sym, res))
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

const ExpirMap = Dict{String,Vector{Date}}()
getExpirations(sym) = useKey(() -> tradierExpirations(sym), ExpirMap, sym)

const RawChainMap = Dict{String,Dict{Date,Vector{Dict{String,Any}}}}()
const ChainMap = Dict{String,ChainsType}()
const Under = Dict{String,Dict{String,Any}}()
function getChains(sym::String, maxDate::Date=DATE_FUTURE; up=false)
    up || !haskey(ChainMap, sym) || ( !isempty(ChainMap[sym]) && now(UTC) > unix2datetime(first(ChainMap[sym])[2].ts/1000) + Minute(1) ) || return # isempty(ChainMap[sym]) || return
    maxDate > today() || return # NOTE: If we ever trade today expr, would need to modify this
    println("Calling tradier for $(sym)")
    expAll = getExpirations(sym)

    ChainMap[sym] = ChainsType()
    RawChainMap[sym] = Dict{Date,Dict{String,Any}}()
    if isempty(expAll)
        Under[sym] = Dict()
        return
    end
    Under[sym] = tradierQuote(sym)

    # exprs = expAll[1] == today() ? expAll[2:min(11,length(expAll))] : expAll[1:min(10,length(expAll))]
    exprs = filter(x -> x > today() && x < maxDate, expAll)
    for expr in exprs
        raw = tradierOptionChain(expr, sym)
        RawChainMap[sym][expr] = raw # Dict(d["strike"] => d for d in raw)
        ChainMap[sym][expr] = Chains.procChain(expr, raw)
    end
    return
end

const DF = dateformat"mm/dd/yyyy"
parseDate(seconds::Int) = unix2datetime(seconds)
parseDate(strDate::String) = strDate == "-" ? DATE_FUTURE : ( date = Date(strDate, DF) ; date >= today() ? date : DATE_FUTURE )
function parseDate(d::Dict, k::String)
    haskey(d, k) ? parseDate(d[k]) : return DATE_FUTURE
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
    maxDate = DATE_FUTURE # min(findExDate(get(sa.Dividends, sym, nothing)), findEarnDate(get(sa.Earnings, sym, nothing)))
    getChains(sym, maxDate)
    locUnder = Under[sym]
    chs = ChainMap[sym]
    for expr in sort!(collect(keys(chs)))
        # expr < maxDate || break # exprs sorted asc
        oqs = filter(isPut, chs[expr].chain)
        # TODO: use tex instead of bdays?
        timult = 252 / bdays(today(), expr)
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
            println("$(sym)[$(expr)]: $(underBid) -> $(strike) at $(primitDir) ($(getQuote(oq))) / $(strike) = $(rate)")
            if all || rate > 0.1
                push!(res, (;sym, expr, mov=1.0 - strike/underBid, underBid, strike, primitDir, rate, div=getDividend(sym), oq))
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
function findRoll(sym, at, cost=0.0, style=Style.put)
    res = NamedTuple[]
    getChains(sym)
    locUnder = Under[sym]
    chs = ChainMap[sym]
    for expr in sort!(collect(keys(chs)))
        oqs = filter(x->getStyle(x) == style, chs[expr].chain)
        # TODO: get Calendars further out
        expr < Date(2025,1,1) || continue
        # TODO: use tex instead of bdays?
        tex = Calendars.calcTex(now(UTC), expr)
        timult = 1 / Calendars.texToYear(tex)
        # timult = 252 / bdays(today(), expr)
        underBid = locUnder["bid"]
        for oq in oqs
            strike = getStrike(oq)
            strike == at || continue
            getBid(oq) > 0.0 || continue
            primitDir = bap(oq, .4) - cost
            @show primitDir cost
            rate = timult * (primitDir - .0065) / strike # .0065 is the trade commission for fidelity / 100
            # println("$(sym)[$(expr)]: $(underBid) -> $(strike) at $(primitDir) ($(getQuote(oq))) / $(strike) = $(rate)")
            if rate > 0.3
                push!(res, (;sym, expr, mov=1.0 - strike/underBid, underBid, strike, primitDir, rate, div=getDividend(sym)))
            end
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