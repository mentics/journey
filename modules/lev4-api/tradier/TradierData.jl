module TradierData
using Dates
using BaseTypes
using DictUtil, CollUtil, DateUtil
using TradierConfig, TradierBase

export tradierQuote, tradierOptionChain, tradierHistQuotes, tradierExpirations, tradierCalendar, tradierClock

tradierQuote(sym::AStr=getDefaultSymbol())::TradierResp =
    tradierGet("/markets/quotes?symbols=$(sym)&greeks=false", Call(nameof(var"#self#")))["quotes"]["quote"]

tradierOptionChain(exp::Date, sym::String=getDefaultSymbol())::TradierRespVec = begin
        raw = tradierGet("/markets/options/chains?symbol=$(sym)&expiration=$(Dates.format(exp, TRADIER_DATE_FORMAT))&greeks=true", Call(nameof(var"#self#"), exp))
        if isnothing(raw)
            println("nothing: ", exp)
            error("stop")
        end
        if isnothing(raw["options"])
            println("nothing2: ", exp)
            error("stop")
        end
        # if !haskey(raw, "options")
        #     println("no first, exp: $(exp) ", keys(raw))
        # end
        # if !haskey(raw["options"], "option")
        #     println("no second, exp: $(exp) ", keys(raw))
        # end
        return raw["options"]["option"]
end
    # ( raw = tradierGet("/markets/options/chains?symbol=$(getDefaultSymbol())&expiration=$(Dates.format(exp, TRADIER_DATE_FORMAT))&greeks=true", Call(nameof(var"#self#"), exp)) ;
    # println(keys(raw["options"])) ;
    #     error("stop")
    # )

function tradierHistQuotes(interval, dStart=nothing, dEnd=nothing, sym::AStr=getDefaultSymbol())::TradierRespVec
    if isnothing(dStart)
        raw = tradierGet("/markets/history?symbol=$(sym)&interval=$(interval)", Call(nameof(var"#self#"), interval))
    else
        strStart = Dates.format(dStart, TRADIER_DATE_FORMAT)
        strEnd = Dates.format(dEnd, TRADIER_DATE_FORMAT)
        path = "/markets/history?symbol=$(sym)&interval=$(interval)&start=$(strStart)&end=$(strEnd)"
        raw = tradierGet(path, Call(nameof(var"#self#"), interval))
    end
    return ensureVector(getLastDict(raw, "history", "day"))
end

function tradierExpirations(sym::String=getDefaultSymbol())
    raw = tradierGet("/markets/options/expirations?symbol=$(sym)&includeAllRoots=true&strikes=true", Call(nameof(var"#self#")))
    return sort(map(s -> Date(s["date"], TRADIER_DATE_FORMAT), raw["expirations"]["expiration"]))
end

function tradierCalendar(from::Date, to::Date)::Dict{Date,Dict{String,Any}}
    res = Dict{Date,Dict{String,Any}}()
    foreach((Month(d).value, Year(d).value) for d in range(firstdayofmonth(from), firstdayofmonth(to); step=Month(1))) do (m, y)
        raw = tradierGet("/markets/calendar?year=$(y)&month=$(m)", Call(nameof(var"#self#")))
        for day in raw["calendar"]["days"]["day"]
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

function tradierDividends()
    raw = tradierGet("https://api.tradier.com/beta/markets/fundamentals/dividends?symbols=SPY", CallVec(nameof(var"#self#")))
    return raw[1]["results"][2]["tables"]["cash_dividends"][1]
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
