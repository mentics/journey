module Combos
using Dates, IterTools, NamedTupleTools
using SH, BaseTypes, SmallTypes, ChainTypes
using DateUtil, DictUtil, OutputUtil
using TradierData
using Markets, Chains
import SeekingAlpha

function lookAll(cands=SeekingAlpha.totry())
    global Looked = []
    for sym in cands
        append!(Looked, look(sym))
    end
    global LookedRaw = copy(Looked)
    global Looked = clean(Looked)
    pretyble(sort(delete.(Looked, :oq); by=x -> x.rate))
    return Looked
end
disp() = pretyble(sort(delete.(Looked, :oq); by=x -> x.rate))
check(sym) = pretyble(delete.(look(sym; all=true), :oq))

excludes(lll) = filter!(x -> !(x.sym in ActiveSyms) && !(x.sym in SeekingAlpha.BadPricing) && !(x.sym in TempIgnore) && !(x.sym in SeekingAlpha.Ignore) && x.strike <= 105.0, lll)
function clean(lll)
    # excludes(lll)
    # sort!(lll; rev=true, by=x->x.rate)
    return sort!(map(g -> g[findmax(x -> x.rate, g)[2]], groupby(r -> r.sym, sort!(lll; by=x->x.sym))); rev=true, by=x->x.rate)
end

const ExpirMap = Dict{String,Vector{Date}}()
getExpirations(sym) = useKey(() -> tradierExpirations(sym), ExpirMap, sym)

const RawChainMap = Dict{String,Dict{Date,Vector{Dict{String,Any}}}}()
const ChainMap = Dict{String,ChainsType}()
const Under = Dict{String,Dict{String,Any}}()
function getChains(sym::String, maxDate::Date; up=false)
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
function look(sym; all=false)
    try
    res = []
    # about = SeekingAlpha.Cands[sym]
    # about = SeekingAlpha.Data[:metrics]
    # maxDate = min(parseDate(about[:earningsUpcomingAnnounceDate]), parseDate(about[Symbol("dividendsEx-DivDate")]))
    # maxDate = min(parseDate(about, "earning_announce_date"), parseDate(about, "div_pay_date"))
    maxDate = min(findExDate(sa.Dividends[sym]), findEarnDate(sa.Earnings[sym]))
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
            startI = findfirst(oq -> getStrike(oq) > ratio * underBid, oqs)
            !isnothing(startI) || continue
            range = (startI-5):(startI-1)
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
                push!(res, (;sym, expr, underBid, strike, primitDir, rate, oq))
            end
        end
    end
    return res
    catch e
        @error "look" sym
        rethrow(e)
    end
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

# using Combos
# c = Combos
# c.lookAll(keys(sa.Cands))

end