module TradierData
using Dates
using DictUtil, CollUtil, DateUtil
using TradierConfig, TradierBase

export tradierQuote, tradierOptionChain, tradierHistQuotes, tradierExpirations, tradierCalendar, tradierClock

tradierQuote()::TradierResp =
    tradierGet("/markets/quotes?symbols=$(getDefaultSymbol())&greeks=false", Call(nameof(var"#self#")))["quotes"]["quote"]

tradierOptionChain(exp::Date)::TradierRespVec =
    tradierGet("/markets/options/chains?symbol=$(getDefaultSymbol())&expiration=$(Dates.format(exp, TRADIER_DATE_FORMAT))&greeks=true", Call(nameof(var"#self#"), exp))["options"]["option"]

function tradierHistQuotes(interval, dStart=nothing, dEnd=nothing)::TradierRespVec
    if isnothing(dStart)
        raw = tradierGet("/markets/history?symbol=$(getDefaultSymbol())&interval=$(interval)", Call(nameof(var"#self#"), interval))
    else
        strStart = Dates.format(dStart, TRADIER_DATE_FORMAT)
        strEnd = Dates.format(dEnd, TRADIER_DATE_FORMAT)
        path = "/markets/history?symbol=$(getDefaultSymbol())&interval=$(interval)&start=$(strStart)&end=$(strEnd)"
        raw = tradierGet(path, Call(nameof(var"#self#"), interval))
    end
    return ensureVector(getLastDict(raw, "history", "day"))
end

function tradierExpirations()
    raw = tradierGet("/markets/options/expirations?symbol=$(getDefaultSymbol())&includeAllRoots=true&strikes=true", Call(nameof(var"#self#")))
    return sort(map(s -> Date(s["date"], TRADIER_DATE_FORMAT), raw["expirations"]["expiration"]))
end

function tradierCalendar()::Dict{Date,Dict{String,Any}}
    dc = today()
    mc = Month(dc).value
    yc = Year(dc).value
    dn = today() + Month(1)
    mn = Month(dn).value
    yn = Year(dn).value
    monthCur = tradierGet("/markets/calendar?month=$(mc)&year=$(yc)", Call(nameof(var"#self#")))
    monthNext = tradierGet("/markets/calendar?month=$(mn)&year=$(yn)", Call(nameof(var"#self#")))
    res = Dict{Date,Dict{String,Any}}()
    for dict in (monthCur["calendar"]["days"]["day"], monthNext["calendar"]["days"]["day"])
        for day in dict
            res[Date(day["date"])] = day
        end
    end
    return res
end

function tradierClock()
    clock = tradierGet("/markets/clock", Call(nameof(var"#self#")))["clock"]
    isMktOpen = clock["state"] == "open"
    nextChange = fromMarketTZ(today(), Time(clock["next_change"])) # this comes in eastern time
    nextMktChange = now(UTC) < nextChange ? nextChange : nextChange + Day(1)
    nextMktChange > now(UTC) || error("Next market change must be in the future")
    isMktOpen && (now(UTC)- nextMktChange > Hour(10)) && error("Market is open too long ", nextMktChange, ", ", now(UTC)- nextMktChange)
    return (;isMktOpen, nextMktChange)
end

end

# function tradierQuotes(symbols::Array) # , cfg::TradierConfig=cfg())
#     raw = handle(cfg(), :tradierQuotes) do
#         s = join(symbols, ',')
#         return tradierGet(cfg(), "/markets/quotes?symbols=$(s)&greeks=false")
#     end
#     return raw["quotes"]
# end

# function tradierTimeAndSales(symbol, interval, dtStart, dtEnd, cfg::TradierConfig=cfg())
#     return handle(cfg, :tradierTimeAndSales) do
#         strStart = Dates.format(dtStart, TRADIER_DATE_TIME_FORMAT)
#         strEnd = Dates.format(dtEnd, TRADIER_DATE_TIME_FORMAT)
#         return tradierGet(cfg, "/markets/timesales?symbol=$(cfg().defaultSymbol)&interval=$(interval)&start=$(strStart)&end=$(strEnd)&session_filter=all")
#     end
# end

# function tradierOptionSymbols() # symbol::AbstractString=cfg.defaultSymbol, cfg::TradierConfig=cfg())
#     raw = handle(cfg(), :tradierOptionSymbols) do
#         return tradierGet(cfg(), "/markets/options/lookup?underlying=$(cfg().defaultSymbol)")
#     end
#     return raw
# end
