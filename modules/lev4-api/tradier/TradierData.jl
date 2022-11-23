module TradierData
using Dates
using BaseTypes
using LogUtil, DictUtil, CollUtil, DateUtil
using TradierConfig, TradierBase

export tradierQuote, tradierQuotes, tradierOptionChain, tradierHistQuotes, tradierExpirations, tradierCalendar, tradierClock, tradierDividends, tradierCorpCal

function tradierQuote(sym::AStr=getDefaultSymbol())::TradierResp
    raw = tradierGet("/markets/quotes?symbols=$(sym)&greeks=false", Call(nameof(var"#self#")))
    haskey(raw["quotes"], "quote") || error(raw)
    return raw["quotes"]["quote"]
end

tradierQuotes(syms::AStr...)::TradierResp = tradierQuotes(syms)
function tradierQuotes(syms::Coll{<:AStr})::TradierResp
    payload = "symbols=$(join(syms, ','))&greeks=false"
    raw = tradierPost("/markets/quotes", payload, Call(nameof(var"#self#")); retries=3)
    # haskey(raw["quotes"], "quote") || error(raw)
    return useKey(TradierResp, raw, "quotes")
end

import Globals
tradierOptionChain(exp::Date, sym::String=getDefaultSymbol())::TradierRespVec = begin
        raw = tradierGet("/markets/options/chains?symbol=$(sym)&expiration=$(Dates.format(exp, TRADIER_DATE_FORMAT))&greeks=true", Call(nameof(var"#self#"), exp))
        if isnothing(raw)
            println("nothing: ", exp)
            error("stop")
        end
        if !haskey(raw, "options") || isnothing(raw["options"])
            println("Could not load open chain with snap $(Globals.snap()) for exp: ", exp)
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
    # println(raw)
    # # data = tryKeys(raw, nothing, "expirations", "expiration")
    # println(data)
    if haskey(raw, "expirations") && !isnothing(raw["expirations"]) && haskey(raw["expirations"], "expiration")
        return sort(map(s -> Date(s["date"], TRADIER_DATE_FORMAT), raw["expirations"]["expiration"]))
    else
        @log warn "Expirations not found for" sym
        println("Expirations not found for ", sym)
        return Dict{String,Any}[]
    end
end

function tradierCalendar(from::Date, to::Date)::Dict{Date,Dict{String,Any}}
    res = Dict{Date,Dict{String,Any}}()
    foreach((Month(d).value, Year(d).value) for d in range(firstdayofmonth(from), firstdayofmonth(to); step=Month(1))) do (m, y)
        raw = tradierGet("/markets/calendar?year=$(y)&month=$(m)", Call(nameof(var"#self#")))
        for day in raw["calendar"]["days"]["day"]
            d = Date(day["date"])
            if d == Date(2022,6,20)
                res[d] = Dict("status"=>"closed", "date"=>"2022-06-20", "description"=>"Market is closed")
            else
                res[d] = day
            end
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

function tradierDividends(syms::Coll{<:AStr}=("SPY",))
    raw = tradierGet("https://api.tradier.com/beta/markets/fundamentals/dividends?symbols=$(join(syms, ','))", CallVec(nameof(var"#self#")))
    res = Dict{String,Any}()
    for symd in raw
        found = Dict[]
        for d2 in symd["results"]
            dl = d2["tables"]["cash_dividends"]
            !isnothing(dl) || continue
            typeof(dl) <: AVec ? append!(found, dl) : push!(found, dl)
        end
        res[symd["request"]] = found
    end
    return res
end

export findExDate
findExDate(::Nothing)::Date = DATE_FUTURE
findExDate(res::Vector{<:Dict}, tod::Date=today())::Date =
        minimum(filter(x -> x > tod, map(x -> Date(x["ex_date"]), res)); init=DATE_FUTURE)

function tradierCorpCal(syms::Coll{<:AStr}=("SPY",))
    raw = tradierGet("https://api.tradier.com/beta/markets/fundamentals/calendars?symbols=$(join(syms, ','))", CallVec(nameof(var"#self#")))
    res = Dict{String,Any}()
    for symd in raw
        found = Dict[]
        for d2 in symd["results"]
            # if !haskey(d2, "tables")
            #     @error "no tables" d2 syms
            #     error("no tables")
            # end
            haskey(d2, "tables") || continue
            dl = d2["tables"]["corporate_calendars"]
            !isnothing(dl) || continue
            typeof(dl) <: AVec ? append!(found, dl) : push!(found, dl)
            # isnothing(dl) || append!(found, dl) # list[findmax(x -> x["begin_date_time"], list)[2]])
        end
        res[symd["request"]] = found
    end
    return res
    # return sort!(raw[1]["results"][2]["tables"]["cash_dividends"]; rev=true, by=x->Date(x["ex_date"]))
end

const EARNINGS_TYPES = (7,8,9,10)
export findEarnDate
findEarnDate(::Nothing)::Date = DATE_FUTURE
function findEarnDate(res::Vector{<:Dict}, tod::Date=today())::Date
    earnings = filter(x -> x["event_type"] in EARNINGS_TYPES, res)
    dates = filter(x -> x > tod, map(x -> Date(x["begin_date_time"]), earnings))
    return minimum(dates; init=DATE_FUTURE)
end

end


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
