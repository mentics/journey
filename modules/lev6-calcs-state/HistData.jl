module HistData
using Dates, DelimitedFiles
using Globals, BaseTypes, FileUtil, DateUtil, LogUtil, DictUtil
using Caches, TradierData

export dataDaily, dataDay
export DailyType, DailyRowType, RetsItemType

# TODO: do proper row type
const DailyRowType = NamedTuple{(:date, :open, :high, :low, :close, :volume), Tuple{Date, Float64, Float64, Float64, Float64, Int64}}
const DailyType = Vector{DailyRowType}
const RetsItemType = NamedTuple{(:interval, :ret, :date, :dailyIndex), Tuple{Int,Float64,Date,Int}}

const VixOpens = Dict{Date,Currency}()
function vixOpen(date::Date)
    !isempty(VixOpens) || populateVixOpens()
    return VixOpens[date]
end
function populateVixOpens()
    v = dataDaily("VIX")
    for row in v
        VixOpens[row.date] = row.open
    end
end

# In descending date order
function dataDaily(sym::AStr="SPY"; refresh=false)::Union{Nothing,DailyType}
    res = cache!(() -> updateDaily(sym), DailyType, Symbol("daily-$(lowercase(sym))"), Hour(4); refresh)
    if length(res) < 100
        println("Result length too short for dataDaily(", sym, ") ", length(res))
    end
    return res
end
# dataDaily(d::Date, sym::AStr="SPY")::DailyType = (daily = dataDaily(sym) ; daily[findfirst(r->r.date <= d, daily):end])
dataDaily(from::Date, to::Date, sym::AStr="SPY")::DailyType = filter(r -> from <= r.date <= to, dataDaily(sym))
dailyDict(from::Date, to::Date, sym::AStr="SPY")::Dict{Date,NamedTuple} = dictFromVals(getDate, filter(x -> from <= x.date <= to, dataDaily(sym)))
dailyDict(sym::AStr="SPY")::Dict{Date,NamedTuple} = dictFromVals(getDate, dataDaily(sym))

priceOpen(d::Date)::Currency = dataDay(d).open
function dataDay(d::Date, sym::AStr="SPY")::DailyRowType
    daily = dataDaily(sym)
    i = findfirst(r -> r.date == d, daily)
    if isnothing(i)
        error("Quote for $(sym) not found for $(d)")
    end
    daily[i]
end

# const PRICE_MAX = C(1e9)
# const PRICE_ZERO = C(0.0)

function extrema(daily, from::Date, to::Date, hi1::Real, lo1::Real, sym::AStr="SPY")
    # daily = dataDaily(from, to, sym)
    hi = hi1
    lo = lo1
    # loDate = to + Day(1)
    # hiDate = to+ Day(1)
    for d in Iterators.reverse(daily)
        d.date >= from || continue
        d.date <= to || break
        if d.high > hi
            hi = d.high
            # hiDate = d.date
        end
        if d.low < lo
            lo = d.low
            # loDate = d.date
        end
    end
    # return (;hi, lo, hiDate, loDate)
    return (;hi = C(hi), lo = C(lo))
end

# daily is in descending date order.
function makeRets(daily, interval::Int)::NamedTuple
    rets = Vector{Float64}()
    dates = Vector{Date}()
    for i in 1:length(daily)-interval
        rowStart = daily[i + interval]
        priceStart = rowStart.close
        rowEnd = daily[i]
        priceEnd = rowEnd.close
        ret = priceEnd / priceStart
        push!(rets, ret)
        push!(dates, rowStart.date)
    end
    # if length(rets[1]) > length(daily)
    #     throw("Rets is too many $(length(rets[1])) > $(length(daily))")
    # end
    return (;rets, dates)
end

# daily is in descending date order.
# pass in a view if you want to save a separate out-of-sample set.
function makeRetsExpirs(daily, maxInterval::Int)::NamedTuple
    rets = [Vector{Float64}() for i in 1:maxInterval]
    dates = [Vector{Date}() for i in 1:maxInterval]
    for i in 1:length(daily)-maxInterval
        rowExp = daily[i]
        if !(dayofweek(rowExp.date) in [1,3,5])
            continue
        end
        for j in 1:maxInterval
            rowOpen = daily[i + j]
            priceOpen = rowOpen.open
            priceClose = rowExp.close
            ret = priceClose / priceOpen
            push!(rets[j], ret)
            push!(dates[j], rowOpen.date)
            # (data[i].close / data[i + interval].open, data[i+interval].date) for i in from:to
        end
    end
    if length(rets[1]) > length(daily)
        throw("Rets is too many $(length(rets[1])) > $(length(daily))")
    end
    return (;rets, dates)
end

# TODO: combine the two of these
function makeRetsExpirs2(daily, maxInterval::Int)::Vector{Vector{RetsItemType}}
    res = Vector{Vector{RetsItemType}}([Vector{RetsItemType}() for i in 1:maxInterval])
    # res = OffsetVector([Vector{RetsItemType}() for i in 0:maxInterval], 0:maxInterval)
    # dates = OffsetVector([Vector{Date}() for i in 0:maxInterval], 0:12)
    for i in 1:length(daily)-maxInterval
        rowExp = daily[i]
        if !(dayofweek(rowExp.date) in [1,3,5])
            continue
        end
        for j in 1:maxInterval
            rowOpen = daily[i + j]
            priceOpen = rowOpen.open
            priceClose = rowExp.close
            ret = priceClose / priceOpen
            push!(res[j], (;interval=j, ret, date=rowOpen.date, dailyIndex=i+j))
            # push!(rets[j], ret)
            # push!(dates[j], rowOpen.date)
            # (data[i].close / data[i + interval].open, data[i+interval].date) for i in from:to
        end
    end
    # if length(rets[1]) > length(daily)
    #     throw("Rets is too many $(length(rets[1])) > $(length(daily))")
    # end
    return res
end

#region Local
const header = [:date, :open, :high, :low, :close, :volume]
# const pathDaily = dirData("hist", "daily", "spy", "spy-daily.csv")
# const pathDailyVix = dirData("hist", "daily", "spy", "vix-daily.csv")


import Paths
# pathDaily(sym::AStr)::String = joinpath(dirData("hist", "daily"), "$(sym)-daily.csv")
pathDaily(sym::AStr)::String = Paths.db_incoming("tradier", "daily", "$(sym).csv")

function updateDaily(sym)::DailyType
    path = pathDaily(lowercase(sym))
    try
        lastExpected = lastTradingDay(Dates.today() - Day(1)) # min(lastTradingDay(Dates.today()), today() - Day(1))
        if !isfile(path)
            @log warn "Can't find file for $(sym): $(path)"
            daily = getDataDaily(Date(2000,1,1), lastExpected, sym)
            writeCsv(path, daily; keys=string.(header))
        else
            lastLine = readLastLines(path)
            lastDate = Date(split(lastLine, ',')[1])
            # TODO: check if we get vix for friday during the weekend
            if lastDate < lastExpected
                newDaily = getDataDaily(lastDate+Day(1), lastExpected, sym)
                if !isnothing(newDaily)
                    if length(newDaily) > 0 && haskey(newDaily[1], "date")
                        appendCsv(path, newDaily; keys=string.(header))
                    else
                        @error "Unexpected hist daily response" sym lastLine lastDate lastExpected newDaily
                    end
                end
            end
        end
    catch e
        if e isa InterruptException
            rethrow(e)
        else
            @warn "Could not update HistData: " path
        end
    end
    return load(path)
end

function load(path)
    (mat, head) = readdlm(path, ',', Any, '\n'; header=true)
    names = Tuple(Symbol(s) for s in head)
    return reverse!([NamedTuple{names}(strToDate.(r)) for r in eachrow(mat)])
end
strToDate(x::AbstractString) = Date(x)
strToDate(x) = x

# function loadRecent(n::Int=20)
#     readLastLines(pathDaily, n)
# end

# function dailyRecent(n::Int=20)::Dict{Date,CSV.Row}
#     rows = CSV.File(IOBuffer(loadRecent(n)); header=header)
#     d = Dict{Date,CSV.Row}()
#     for row in rows
#         d[row.date] = row
#     end
#     return d
# end

getDataDaily(from, to, sym="SPY") = tradierHistQuotes("daily", from, to, sym)
#endregion

end