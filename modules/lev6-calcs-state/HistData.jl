module HistData
using Dates, DelimitedFiles
using FileUtil, DateUtil
using Caches, TradierData
# using TopTypes, Caching, DateUtil, FileUtil, Tradier

export dataDaily, dataDay#, makeRetsExpirs, priceOpen
export DailyType, DailyRowType, RetsItemType

# TODO: do proper row type
const DailyRowType = NamedTuple{(:date, :open, :high, :low, :close, :volume), Tuple{Date, Float64, Float64, Float64, Float64, Int64}}
const DailyType = Vector{DailyRowType}
const RetsItemType = NamedTuple{(:interval, :ret, :date, :dailyIndex), Tuple{Int,Float64,Date,Int}}

const header = [:date, :open, :high, :low, :close, :volume]
const pathDaily = joinpath("data", "hist", "daily", "spy", "spy-daily.csv")

const DAILY = :daily

dataDaily(;up=false)::DailyType = cache!(() -> updateDaily(), DailyType, DAILY, Hour(4); up)
dataDaily(d::Date)::DailyType = (daily = dataDaily() ; daily[findfirst(r->r.date <= d, daily):end])

priceOpen(d::Date)::Currency = dataDay(d).open
dataDay(d::Date)::DailyRowType = (daily = dataDaily() ; daily[findfirst(r->r.date == d, daily)])

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
function updateDaily()::DailyType
    if !isfile(pathDaily)
        # daily = Tradier.tradierHistQuotes(cfg, "SPY", "daily", Date(2000,1,1), Dates.today()+Day(1))
        daily = getDataDaily(Date(2000,1,1), Dates.today()+Day(1))
        writeCsv(pathDaily, daily; keys=string.(header))
    else
        lastLine = readLastLines(pathDaily)
        lastDate = Date(split(lastLine, ',')[1])
        lastExpected = lastTradingDay(Dates.today() - Day(1))
        if lastDate < lastExpected
            newDaily = getDataDaily(lastDate+Day(1), lastExpected)
            if !isnothing(newDaily)
                if length(newDaily) > 0 && haskey(newDaily[1], "date")
                    appendCsv(pathDaily, newDaily; keys=string.(header))
                else
                    throw("Unexpected hist daily response: " * string(newDaily))
                end
            end
        end
    end
    return load(pathDaily)
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

getDataDaily(from, to) = tradierHistQuotes("daily", from, to)
#endregion

end