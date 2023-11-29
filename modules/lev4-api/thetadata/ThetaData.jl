module ThetaData
using Dates, HTTP, DataFrames
import DataStructures:SortedSet
using Arrow
import JLD2
using DateUtil, DictUtil, Caches
using SmallTypes
import Calendars as cal
using Paths


const INCOMING_PATH = joinpath(PATHS.db(), "market", "incoming", "thetadata")

const EARLIEST_DATE = Date(2012, 6, 1)

const CT = Float32

const HEADERS_GET = Ref(Dict("Accept" => "application/json"))
const DATE_FORMAT = DateFormat("yyyymmdd")
to_date(d) = to_date(string(d))
to_date(d::AbstractString) = Date(d, DATE_FORMAT)
function to_time(ms)
    minutes = ms ÷ 1000 ÷ 60
    Time(divrem(minutes, 60)...)
end
str(d::Date) = Dates.format(d, DATE_FORMAT)

# function query_all_dates(sym="SPY"; age=Hour(12))
#     return cache!(Vector{Date}, Symbol("dates-$(sym)"), age) do
#         url = "http://localhost:25510/v2/list/dates/option/quote?root=$(sym)"
#         resp = HTTP.get(url, HEADERS_GET[]; retry=false)
#         return map(s -> to_date(s), parseJson(String(resp.body), Dict)["response"])
#     end
# end

function expirs(sym="SPY"; age=Hour(12))
    return cache!(Vector{Date}, Symbol("expirs-$(sym)"), age) do
        url = "http://localhost:25510/v2/list/expirations?root=$(sym)"
        resp = HTTP.get(url, HEADERS_GET[]; retry=false)
        return map(s -> to_date(s), parseJson(String(resp.body), Dict)["response"])
    end
end

function query_dates_for_xpir(xpir::Date, sym="SPY"; age=Hour(12))
    return cache!(Vector{Date}, Symbol("dates-$(sym)-$(xpir)"), age) do
        url = "http://localhost:25510/v2/list/dates/option/quote?root=$(sym)&exp=$(str(xpir))"
        resp = HTTP.get(url, HEADERS_GET[]; retry=false)
        return map(s -> to_date(s), parseJson(String(resp.body), Dict)["response"])
    end
end

path_expirs(sym) = joinpath(INCOMING_PATH, "expirs-$(sym).jld2")
function expirs_save(sym="SPY")
    xpirs = expirs(sym)
    xpir_to_date = Dict{Date,Vector{Date}}()
    date_to_xpir = Dict{Date,Vector{Date}}()
    for xpir in xpirs
        dates = query_dates_for_xpir(xpir, sym)
        for date in dates
            push!(get!(Vector, xpir_to_date, xpir), date)
            push!(get!(Vector, date_to_xpir, date), xpir)
        end
    end
    JLD2.jldsave(path_expirs(sym); xpir_to_date, date_to_xpir)

    # df = mapreduce(vcat, xpirs) do xpir
    #     DataFrame( [ [xpir], [dates(xpir, sym)] ] , [:date, :dates])
    # end
    # sort!(df, [:date])
    # Arrow.write(path_expirs(sym), df)
end

function xpir_dates(sym="SPY"; age=Hour(12))
    return cache!(NTuple{2,Dict{Date,Vector{Date}}}, Symbol("expirs-dates-$(sym)"), age) do
        JLD2.load(path_expirs(sym), "xpir_to_date", "date_to_xpir")
    end
end

function bulk_push!(v, items)
    foreach(items) do item
        push!(v, item)
    end
    return v
end

function xpirs_for_dates(dates)
    _, date_to_xpir = xpir_dates()
    dates = filter(date -> haskey(date_to_xpir, date), dates)
    return collect(mapreduce(bulk_push!, dates; init=SortedSet()) do date
        date_to_xpir[date]
    end)
end

# function quote(sym="SPY")
#     url = "http://localhost:25510/v2/hist/option/quote?root=$(sym)&exp=20120601&start_date=20120601&end_date=20120601&ivl=1800000"
#     resp = HTTP.get(url, HEADERS_GET[]; retry=false)
#     return parseJson(String(resp.body), Dict)
# end

#=
bid_condition/ask_condition enum:
https://thetadata-api.github.io/thetadata-python/reference/#thetadata.enums.QuoteCondition
=#
using LRUCache
const HIST_CACHE2 = LRUCache.LRU{String,Union{Nothing,Dict}}(;maxsize=100)
function quotes(date_start, date_end, xpir; period=1800000, sym="SPY")
    println("Getting quotes for $(date_start) to $(date_end) for xpir=$(xpir)")

    url = "http://localhost:25510//v2/bulk_hist/option/quote?exp=$(str(xpir))&start_date=$(str(date_start))&end_date=$(str(date_end))&root=$(sym)&ivl=$(period)"
    data = get!(HIST_CACHE2, url) do
        start = now(UTC)
        resp = HTTP.get(url, HEADERS_GET[]; retry=false)
        dict = parseJson(String(resp.body), Dict)
        error_type = dict["header"]["error_type"]
        if error_type != "null"
            if error_type == "NO_DATA"
                return nothing
            else
                throw((dict["header"], url))
            end
        end
        stop = now(UTC)
        println("  data was not in cache, acquired from thetdata in $((stop - start)/1000) seconds: $(url)")
        return dict
    end
    if isnothing(data)
        println("WARN: no data for url=$(url)")
        return nothing
    end

    start = now(UTC)
    df = DataFrame([DateTime[], DateTime[], Style.T[], CT[], CT[], UInt32[], UInt8[], CT[], UInt32[], UInt8[]], [:ts, :expir, :style, :strike, :bid, :bid_size, :bid_condition, :ask, :ask_size, :ask_condition])
    for d in data["response"]
        info = d["contract"]
        style = info["right"] == "C" ? Style.call : Style.put
        strike = info["strike"]
        for tick in d["ticks"]
            ts = DateUtil.fromMarketTZ(to_date(tick[10]), to_time(tick[1]))
            bid_size = tick[2]
            bid_condition = tick[3]
            bid = tick[4]
            ask = tick[8]
            ask_size = tick[6]
            ask_condition = tick[7]
            # @show (ts, xpir, style, strike, bid, ask)
            push!(df, [ts, cal.getMarketClose(handle_sat_xpir(xpir)), style, strike / 1000, bid, bid_size, bid_condition, ask, ask_size, ask_condition])
        end
    end
    stop = now(UTC)
    dur = (stop-start)/1000
    if dur > 2
        println("  created df in $(dur) seconds")
    end
    return df
end

function for_month(year, month)
    date_start = Date(year, month, 1)
    date_end = Dates.lastdayofmonth(date_start)
    # dates = Date(year, month, 1):Dates.lastdayofmonth(date_start)
    xpirs = xpirs_for_dates(date_start:date_end)
    start = now(UTC)
    df = mapreduce(vcat, xpirs) do xpir
        quotes(date_start, min(xpir, date_end), xpir)
    end
    stop = now(UTC)
    println("Completed loading data for $(year) $(month) in $((stop-start)/1000) seconds")
    sort!(df, [:style, :ts, :expir, :strike])
    return df
end

# vcatn(df1, df2) = isnothing(df2) ? df1 : vcat(df1, df2)

handle_sat_xpir(date) = Dates.dayofweek(date) == 6 ? date - Day(1) : date

#=
 "ms_of_day"
 "bid_size"
 "bid_condition"
 "bid"
 "bid_exchange"
 "ask_size"
 "ask_condition"
 "ask"
 "ask_exchange"
 "date"
 =#

end