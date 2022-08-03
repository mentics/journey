module Combos
using Dates
using SH, BaseTypes, SmallTypes
using DateUtil, DictUtil
using TradierData
using Markets, Chains

function lookAll(cands)
    global Looked = []
    for sym in cands
        append!(Looked, look(sym))
    end
    return Looked
end

const ExpirMap = Dict{String,Vector{Date}}()
getExpirations(sym) = useKey(() -> tradierExpirations(sym), ExpirMap, sym)

const RawChainMap = Dict{String,Dict{Date,Vector{Dict{String,Any}}}}()
const ChainMap = Dict{String,ChainsType}()
const Under = Dict{String,Dict{String,Any}}()
function getChains(sym; up=false)
    up || !haskey(ChainMap, sym) || ( now(UTC) > unix2datetime(first(ChainMap[sym])[2].ts/1000) + Minute(1) ) || return # isempty(ChainMap[sym]) || return
    println("Calling tradier for $(sym)")
    expAll = getExpirations(sym)

    ChainMap[sym] = ChainsType()
    RawChainMap[sym] = Dict{Date,Dict{String,Any}}()
    if isempty(expAll)
        Under[sym] = Dict()
        return
    end
    Under[sym] = tradierQuote(sym)

    exprs = expAll[1] == today() ? expAll[2:min(11,length(expAll))] : expAll[1:min(10,length(expAll))]
    for expr in exprs
        raw = tradierOptionChain(expr, sym)
        RawChainMap[sym][expr] = raw # Dict(d["strike"] => d for d in raw)
        ChainMap[sym][expr] = Chains.procChain(expr, raw)
    end
    return
end

using Between
function look(sym)
    res = []
    getChains(sym)
    locUnder = Under[sym]
    chs = ChainMap[sym]
    for expr in sort!(collect(keys(chs)))
        oqs = filter(isPut, chs[expr].chain)
        timult = 252 / bdays(today(), expr)
        # TODO: use IV to figure out how far out to go?
        # or could maybe get 52 week range
        underBid = locUnder["bid"]
        startI = findfirst(oq -> getStrike(oq) > 0.92 * underBid, oqs)
        !isnothing(startI) || continue
        for i in (startI-5):(startI-1)
            i >= 1 || continue
            oq = oqs[i]
            strike = getStrike(oq)
            primitDir = bap(oq)
            rate = timult * primitDir / strike
            println("$(sym)[$(expr)]: $(underBid) -> $(strike) at $(primitDir) ($(getQuote(oq))) / $(strike) = $(rate)")
            if rate > 0.5
                push!(res, (;sym, expr, underBid, strike, primitDir, rate, oq))
            end
        end
    end
    return res
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