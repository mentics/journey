module ThetaData
using Dates, HTTP, DataFrames
using DateUtil, DictUtil, Caches
using SmallTypes
import Calendars as cal
import DataFiles as dat
import CollUtil:push_all!
using Paths, FilesJLD2, FilesArrow

#region Expirations and Days
function query_xpirs(sym="SPY"; age=age_daily())
    return cache!(Vector{Date}, Symbol("expirs-$(sym)"), age) do
        url = "http://localhost:25510/v2/list/expirations?root=$(sym)"
        println("Querying expirs: $(url)")
        resp = HTTP.get(url, HEADERS_GET[]; retry=false)
        return map(s -> to_date(s), parseJson(String(resp.body), Dict)["response"])
    end
end

function query_dates_for_xpir(xpir::Date, sym="SPY"; age=age_daily())
    return cache!(Vector{Date}, Symbol("dates-$(sym)-$(xpir)"), age) do
        url = "http://localhost:25510/v2/list/dates/option/quote?root=$(sym)&exp=$(str(xpir))"
        println("Querying dates for expir: $(url)")
        resp = HTTP.get(url, HEADERS_GET[]; retry=false)
        return map(s -> to_date(s), parseJson(String(resp.body), Dict)["response"])
    end
end
#endregion Expirations and Days

#region Quotes
function get_quotes(year, month; sym="SPY")
    path = file_quotes(year, month; sym)
    return load_data(path, DataFrame)
end

function make_quotes(year, month; sym="SPY")
    path = file_quotes(year, month; sym)
    date_start = Date(year, month, 1)
    date_end = Dates.lastdayofmonth(date_start)
    # dates = Date(year, month, 1):Dates.lastdayofmonth(date_start)
    xpirs = get_xpirs_for_dates(date_start:date_end)
    start = time()
    df = mapreduce(vcat, xpirs) do xpir
        query_quotes(date_start, min(xpir, date_end), xpir)
    end
    stop = time()
    println("Completed acquiring data for ($(year), $(month)) in $(stop - start) seconds")
    sort!(df, [:style, :ts, :expir, :strike])
    save_data(path, df)
    return df
end

#=
bid_condition/ask_condition enum:
https://thetadata-api.github.io/thetadata-python/reference/#thetadata.enums.QuoteCondition

Example data:
http://localhost:25510/v2/bulk_hist/option/quote?root=SPY&exp=20120601&start_date=20120601&end_date=20120601&ivl=1800000
=#
using LRUCache
const HIST_CACHE2 = LRUCache.LRU{String,Union{Nothing,Dict}}(;maxsize=10000)
function query_quotes(date_start, date_end, xpir; period=1800000, sym="SPY")
    println("Getting quotes for $(date_start) to $(date_end) for xpir=$(xpir)")

    url = "http://localhost:25510//v2/bulk_hist/option/quote?exp=$(str(xpir))&start_date=$(str(date_start))&end_date=$(str(date_end))&root=$(sym)&ivl=$(period)"
    data = get!(HIST_CACHE2, url) do
        start = time()
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
        stop = time()
        println("  data was not in cache, acquired from thetadata in $(stop - start) seconds: $(url)")
        return dict
    end
    if isnothing(data)
        println("WARN: no data for url=$(url)")
        return nothing
    end

    start = time()
    df = DataFrame([DateTime[], DateTime[], CT[], Int8[], CT[], CT[], UInt32[], UInt8[], CT[], UInt32[], UInt8[]], [:ts, :expir, :under, :style, :strike, :bid, :bid_size, :bid_condition, :ask, :ask_size, :ask_condition])
    for d in data["response"]
        info = d["contract"]
        style = Int8(info["right"] == "C" ? 1 : -1)
        strike = info["strike"]
        for tick in d["ticks"]
            # tick[1] != 34200000 || continue # skip opening time
            ts = DateUtil.fromMarketTZ(to_date(tick[10]), to_time(tick[1]))
            ts != cal.getMarketOpen(Date(ts)) || continue # skip open time because I don't trust, too volatile
            bid_size = tick[2]
            bid_condition = tick[3]
            bid = tick[4]
            ask = tick[8]
            ask_size = tick[6]
            ask_condition = tick[7]
            under = Date(ts) <= DAT_UNDER_LAST_DATE ? dat.lup_under(ts) : get_under()[ts]
            @assert !ismissing(under) "under missing for $(ts)"
            push!(df, [ts, cal.getMarketClose(handle_special_xpirs(xpir)), under, style, strike / 1000, bid, bid_size, bid_condition, ask, ask_size, ask_condition])
        end
    end
    stop = time()
    dur = stop - start
    if dur > 2.0
        println("  created df in $(dur) seconds")
    end
    return df
end
#endregion Quotes

#region Roots
function query_prices(start_date=EARLIEST_SPY_DATE, end_date=Date(DateUtil.market_now() - Day(1)); sym="SPY", age=age_daily())
    return cache!(DataFrame, Symbol("prices-$(sym)"), age) do
        url = "http://localhost:25510/hist/stock/trade?root=$(sym)&start_date=$(str(start_date))&end_date=$(str(end_date))&ivl=1800000"
        # TODO: is it safe to use last trade instead of quote?
        # TODO: check that all expected ts are covered by comparing with DateUtil.all_weekday_ts
        println("Querying under: $(url)")
        resp = HTTP.get(url, HEADERS_GET[]; retry=false)
        ticks = parseJson(String(resp.body), Dict)["response"]
        tss = DateTime[]
        prices = Float32[]
        # res = Dict{DateTime,Float32}()
        for tick in ticks
            @assert tick[1] % (1000 * 60 * 30) == 0 "$(tick[1]) % (1000 * 60 * 30) == 0"
            ts = DateUtil.fromMarketTZ(to_date(tick[6]), to_time(tick[1]))
            price = tick[5]
            # res[ts] = price
            push!(tss, ts)
            push!(prices, price)
        end
        return DataFrame([tss, prices], [:ts, :price])
    end
end
#endregion Roots

#region Constants and Util
const EARLIEST_SPY_DATE = Date(2021, 12, 26)
const EARLIEST_OPTIONS_DATE = Date(2012, 6, 1)
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

function handle_special_xpirs(date)
    date = Dates.dayofweek(date) == 6 ? date - Day(1) : date
    # good friday holiday
    if date == Date(2014,4,18)
        date = Date(2014,4,17)
    end
    return date
end
# vcatn(df1, df2) = isnothing(df2) ? df1 : vcat(df1, df2)
#endregion Constants and Util

#region Explore and Test
# function check_0p_dist(df)
#     dat.lup_under(ts)
# end
#endregion Explore and Test

end