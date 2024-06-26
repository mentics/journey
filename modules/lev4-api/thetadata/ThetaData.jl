module ThetaData
using Dates, HTTP, DataFrames
using DateUtil, DictUtil, Caches
import Calendars as cal
import CollUtil:push_all!

#region Expirations and Days
function query_xpirs(sym="SPY"; age=age_daily())
    return cache!(Vector{Date}, Symbol("thetadata-expirs-$(sym)"), age) do
        url = "http://localhost:25510/v2/list/expirations?root=$(sym)"
        println("Querying expirs: $(url)")
        resp = HTTP.get(url, HEADERS_GET[]; retry=false)
        return map(s -> to_date(s), parseJson(String(resp.body), Dict)["response"])
    end
end

function query_dates_for_xpir(xpir::Date, sym="SPY"; age=age_daily())
    return cache!(Vector{Date}, Symbol("thetadata-dates-$(sym)-$(xpir)"), age) do
        url = "http://localhost:25510/v2/list/dates/option/quote?root=$(sym)&exp=$(str(xpir))"
        println("Querying dates for expir: $(url)")
        resp = HTTP.get(url, HEADERS_GET[]; retry=false)
        return map(s -> to_date(s), parseJson(String(resp.body), Dict)["response"])
    end
end
#endregion Expirations and Days

#region Options
#=
bid_condition/ask_condition enum:
https://thetadata-api.github.io/thetadata-python/reference/#thetadata.enums.QuoteCondition

Example data:
http://localhost:25510/v2/bulk_hist/option/quote?root=SPY&exp=20120601&start_date=20120601&end_date=20120601&ivl=1800000
=#
using LRUCache
const HIST_CACHE2 = LRUCache.LRU{String,Union{Nothing,Dict}}(;maxsize=10000)
function query_options(date_start, date_end, xpir; period=1800000, sym="SPY", cache=true)
    println("Getting quotes for $(date_start) to $(date_end) for xpir=$(xpir)")

    url = "http://127.0.0.1:25510/v2/bulk_hist/option/quote?exp=$(str(xpir))&start_date=$(str(date_start))&end_date=$(str(date_end))&root=$(sym)&ivl=$(period)"
    if !cache || !haskey(HIST_CACHE2, url)
    # data = get!(HIST_CACHE2, url) do
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
        data = dict
        HIST_CACHE2[url] = data
    else
        data = HIST_CACHE2[url]
    end
    if isnothing(data)
        println("WARN: no data for url=$(url)")
        return nothing
    end

    start = time()
    df = DataFrame([DateTime[], DateTime[], Int8[], CT[], CT[], UInt32[], UInt8[], CT[], UInt32[], UInt8[]], [:ts, :expir, :style, :strike, :bid, :bid_size, :bid_condition, :ask, :ask_size, :ask_condition])
    global kdata = data
    for d in data["response"]
        global kd = d
        info = d["contract"]
        style = Int8(info["right"] == "C" ? 1 : -1)
        strike = info["strike"]
        for tick in d["ticks"]
            global ktick = tick
            date = to_date(tick[10])
            DateUtil.isBusDay(date) || continue
            !(date in DateUtil.BAD_DATA_DATES()) || continue
            ts = DateUtil.fromMarketTZ(date, to_time(tick[1]))
            ts != cal.getMarketOpen(Date(ts)) || continue # skip open time because I don't trust, too volatile
            bid_size = tick[2]
            bid_condition = tick[3]
            bid = tick[4]
            ask = tick[8]
            ask_size = tick[6]
            ask_condition = tick[7]
            # under = Date(ts) <= DAT_UNDER_LAST_DATE ? dat.lup_under(ts) : get_under()[ts]
            # @assert !ismissing(under) "under missing for $(ts)"
            push!(df, [ts, to_xpirts(xpir), style, strike / 1000, bid, bid_size, bid_condition, ask, ask_size, ask_condition])
        end
    end
    stop = time()
    dur = stop - start
    if dur > 2.0
        println("  created df in $(dur) seconds")
    end
    return df
end

20130316
const HIST_CACHE_EOD = LRUCache.LRU{String,Union{Nothing,Dict}}(;maxsize=10000)
function query_options_eod(date_start, date_end, xpir; sym="UPRO", cache=true)
    println("Getting $(sym) quotes for $(date_start) to $(date_end) for xpir=$(xpir)")

    url = "http://127.0.0.1:25510/v2/bulk_hist/option/eod?exp=$(str(xpir))&start_date=$(str(date_start))&end_date=$(str(date_end))&root=$(sym)"
    if !cache || !haskey(HIST_CACHE_EOD, url)
        start = time()
        resp = HTTP.get(url, HEADERS_GET[]; retry=false)
        dict = parseJson(String(resp.body), Dict)
        error_type = get(dict["header"], "error_type", "null")
        if error_type != "null"
            if error_type == "NO_DATA"
                return nothing
            else
                throw((dict["header"], url))
            end
        end
        stop = time()
        println("  data was not in cache, acquired from thetadata in $(stop - start) seconds: $(url)")
        data = dict
        HIST_CACHE_EOD[url] = data
    else
        data = HIST_CACHE_EOD[url]
    end
    if isnothing(data)
        println("WARN: no data for url=$(url)")
        return nothing
    end

    # "format": ["ms_of_day","ms_of_day2","open","high","low","close","volume","count","bid_size","bid_exchange","bid","bid_condition","ask_size","ask_exchange","ask","ask_condition","date"]
    start = time()
    df = DataFrame([Date[], Date[], Int8[], CT[], CT[], CT[], CT[], CT[], UInt32[]], [:date, :expir, :style, :strike, :open, :high, :low, :close, :volume])
    global kdata = data
    for d in data["response"]
        global kd = d
        info = d["contract"]
        style = Int8(info["right"] == "C" ? 1 : -1)
        strike = info["strike"]
        for tick in d["ticks"]
            global ktick = tick
            date = to_date(tick[17])
            DateUtil.isBusDay(date) || continue
            # TODO: still need this?
            # !(date in DateUtil.BAD_DATA_DATES()) || continue
            open = tick[3]
            high = tick[4]
            low = tick[5]
            close = tick[6]
            volume = tick[7]
            push!(df, [date, xpir, style, strike / 1000, open, high, low, close, volume])
        end
    end
    stop = time()
    dur = stop - start
    if dur > 2.0
        println("  created df in $(dur) seconds")
    end
    return df
end
#endregion Options

#region Prices
function query_prices(date_start=EARLIEST_SPY_DATE, date_end=Date(DateUtil.market_now() - Day(1)); sym="SPY", age=age_daily(), interval=Minute(30))
    ivl = Dates.value(Millisecond(interval))
    return cache!(DataFrame, Symbol("thetadata-prices-$(sym)-$(str(date_start))-$(str(date_end))-$(ivl)"), age) do
        load_prices(date_start, date_end, sym, ivl)
    end
end

function load_prices(date_start, date_end, sym, ivl)
    url = "http://localhost:25510/hist/stock/trade?root=$(sym)&start_date=$(str(date_start))&end_date=$(str(date_end))&ivl=$(ivl)"
    # TODO: is it safe to use last trade instead of quote?
    # TODO: check that all expected ts are covered by comparing with DateUtil.all_weekday_ts
    println("Querying prices: $(url)")
    # resp = HTTP.get(url, HEADERS_GET[]; retry=false)
    resp = HTTP.get(url; retry=false)
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
#endregion Prices

#region Constants and Util
const EARLIEST_SPY_DATE = Date(2021, 12, 26)
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

function to_xpirts(xpir::Date)
    return cal.getMarketClose(handle_special_xpirs(xpir))
end

function handle_special_xpirs(date)
    date = Dates.dayofweek(date) == 6 ? date - Day(1) : date
    # good friday holiday
    if date == Date(2014,4,18)
        date = Date(2014,4,17)
    end
    # special holiday for death of someone
    # TODO: should we do something special for these cases where the duration should be calculated based on
    # original expiration, but close price should be the day before (or last ts before holiday/expiration)?
    if date == Date(2018,12,5)
        date = Date(2018,12,4)
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