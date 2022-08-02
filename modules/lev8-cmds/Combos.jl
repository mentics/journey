module Combos
using Dates
using SH, BaseTypes, SmallTypes
using DateUtil, DictUtil
using TradierData
using Markets, Chains

const PrefixedSymbols = ["XNYS:SKT",
"XNYS:F",
"XNAS:PBCT",
"XNYS:TDS",
"XNYS:IPG",
"XNYS:WHG",
"XNAS:WEYS",
"XNYS:BEN",
"XNYS:CNP",
"XNYS:MDU",
"XNYS:IRM",
"XNYS:OXY",
"XNYS:GRC",
"XNYS:TR",
"XNYS:HP",
"XNYS:ABM",
"XNYS:PFE",
"XNYS:UGI",
"XNYS:T",
"XNAS:CTBI",
"XNYS:ADM",
"XNYS:LEG",
"XNYS:AOS",
"XNYS:FUL",
"XNYS:OHI",
"XNYS:PNR",
"XNYS:WFC",
"XNAS:THFF",
"XNYS:BFS",
"XNAS:SRCE",
"XNYS:NUE",
"XNYS:EV",
"XNYS:AFL",
"XNYS:HRL",
"XNYS:MCY",
"XNYS:BCE",
"XNYS:DCI",
"XNAS:WBA",
"XNYS:UVV",
"XNYS:BRC",
"XNYS:SON",
"XNYS:XOM",
"XNYS:VTR",
"XNYS:GIS",
"XNYS:CWT",
"XNYS:NNN",
"XNAS:LNT",
"XNYS:VZ",
"XNAS:INTC",
"XNYS:KO",
"XNAS:CBSH",
"XNYS:K",
"XNYS:EMR",
"XNYS:SJW",
"XNYS:NWN",
"XNAS:XEL",
"XNYS:RPM",
"XNYS:SWX",
"XNYS:SYY",
"XNYS:OKE",
"XNAS:EXPD",
"XNYS:VFC",
"XNYS:CL",
"XNAS:MGEE",
"XNYS:NEW",
"XNYS:TNC",
"XNYS:O",
"XNYS:CFR",
"XNAS:QCOM",
"XNYS:BKH",
"XNYS:TMP",
"XNYS:ABT",
"XNYS:AWR",
"XNYS:ED",
"XNYS:D",
"XNYS:GPC",
"XNYS:ABBV",
"XNYS:UPS",
"XNYS:CVX",
"XNYS:DUK",
"XNYS:MDT",
"XNYS:WEC",
"XNAS:CINF",
"XNYS:TGT",
"XNYS:PPG",
"XNYS:DOV",
"XNYS:UHT",
"XNAS:ADI",
"XNYS:ATO",
"XNYS:LOW",
"XNYS:WMT",
"XNAS:TXN",
"XNYS:FRT",
"XNYS:SPG",
"XNYS:PG",
"XNYS:CAT",
"XNAS:TROW",
"XNYS:MCD",
"XNYS:MSA"]

function makeSyms()
    symbols = [split(presym, ':')[2] for presym in PrefixedSymbols]
    quotes = tradierQuotes(symbols)
    global cands = quotes
    # filter(quotes) do q
    #     c = q["close"]
    #     !isnothing(c) && c <= 52.0
    # end
end

const ExpirMap = Dict{String,Vector{Date}}()
getExpirations(sym) = useKey(() -> tradierExpirations(sym), ExpirMap, sym)

const RawChainMap2 = Dict{String,Dict{Date,Vector{Dict{String,Any}}}}()
const ChainMap = Dict{String,ChainsType}()
const Under = Dict{String,Dict{String,Any}}()
function getChains(sym; up=false)
    up || !haskey(ChainMap, sym) || return # isempty(ChainMap[sym]) || return
    println("Calling tradier for $(sym)")
    expAll = getExpirations(sym)

    ChainMap[sym] = ChainsType()
    RawChainMap2[sym] = Dict{Date,Dict{String,Any}}()
    if isempty(expAll)
        Under[sym] = Dict()
        return
    end
    Under[sym] = tradierQuote(sym)

    exprs = expAll[1] == today() ? expAll[2:min(4,length(expAll))] : expAll[1:min(3,length(expAll))]
    for expr in exprs
        raw = tradierOptionChain(expr, sym)
        RawChainMap2[sym][expr] = raw # Dict(d["strike"] => d for d in raw)
        ChainMap[sym][expr] = Chains.procChain(expr, raw)
    end
    return
end

using Between
function look(sym)
    getChains(sym)
    locUnder = Under[sym]
    chs = ChainMap[sym]
    for expr in sort!(collect(keys(chs)))
        oqs = filter(isPut, chs[expr].chain)
        timult = 252 / bdays(today(), expr)
        # TODO: use IV to figure out how far out to go?
        # or could maybe get 52 week range
        bid = locUnder["bid"]
        i = findfirst(oq -> getStrike(oq) > 0.9 * bid, oqs)
        !isnothing(i) || continue
        oq = oqs[max(1, i-1)]
        strike = getStrike(oq)
        primitDir = bap(oq)
        rate = timult * primitDir / strike
        # if rate > 0.5
            println("$(sym)[$(expr)]: $(bid) -> $(strike) at $(primitDir) ($(getQuote(oq))) / $(strike) = $(rate)")
        # end
    end
end

function lookAll()
    for sym in cands
        look(sym)
    end
end

function run()
    makeSyms()
    lookAll()
end

function vol(sym)
    for expr in keys(RawChainMap2[sym])
        vol = 0
        opint = 0
        for tq in RawChainMap2[sym][expr]
            vol += tq["average_volume"]
            opint += tq["open_interest"]
        end
        println("$(sym) $(expr): $(vol) volume $(opint) opint")
    end
end

# TODO: get option chains and put premium rates

# c = Combos
# c.makeSyms()
# c.run()

end