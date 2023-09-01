module Calendars
using Dates
using BaseTypes, MarketDurTypes
using Globals, DateUtil, LogUtil, ThreadUtil, MarketDurUtil, FileUtil
using TradierData

export isMarketOpen, nextMarketChange, getMarketOpen, getMarketClose, getMarketTime
export calcTex, calcDurToExpr

function isMarketOpen()
    isnothing(snap()) || return true # snave is not allowed when market is not open
    check()
    return MktState.isOpen
end
# TODO: change name to mktChangeNext
nextMarketChange()::DateTime = ( check() ; MktState.nextChange )
getMarketOpen(d::Date)::DateTime = isBusDay(d) ? fromMarketTZ(d, first(getMarketTime(d).opens)) : error("Not a bday: $(d)")
getMarketClose(d::Date)::DateTime = isBusDay(d) ? fromMarketTZ(d, last(getMarketTime(d).opens)) : error("Not a bday: $(d)")
# DateTime(astimezone(ZonedDateTime(DateTime("$(d)T$(cal[d]["open"]["end"])"), tz"America/New_York"), tz"UTC"))
function getMarketTime(d::Date)::MarketTime
    ensureCal(d)
    return MktTime[d]
end
# calcTimeToClose(ts::DateTime, d::Date)::Period = ts - getMarketClose(d)

# in hours
# texPerYear() = 6.5 * 252 + .3 * (8760 - 6.5 * 252)
texPerYear()::Float64 = 6844.8 # calced with calcTex(now(UTC), now(UTC)+Year(1))
texToYear(tex)::Float64 = tex / texPerYear()

# @inline DateUtil.calcRate(from::DateTime, to::DateTime, ret::Real, risk::Integer)::Float64 = calcRate(from, to, ret, Float64(Float64(risk))
# function DateUtil.calcRate(from::DateTime, to::DateTime, ret::Union{Float64,Currency}, risk::Union{Float64,Currency})::Float64
#     texy = texToYear(calcTex(from, to))
#     return ret / risk / texy
# end

# 9:30 am - 4 pm = 6.5 hours / day
# 252 days per year
# 8760 hours per year
# ivToStdDev(iv::Float64, timeToExpY::Float64) = iv / sqrt(1.0/timeToExpY)
# ivTexToStdDevOld(iv::Float64, tex::Float64) = iv / sqrt(1.0/(tex/24/365))
ivTexToStdDev(iv::Float64, tex::Float64) = iv * sqrt(tex/texPerYear())

calcTex(from::DateTime, to::Date)::Float64 = calcTex(from, getMarketClose(to))
function calcTex(from::DateTime, to::DateTime)::Float64
    dur = calcDur(from, to)
    closed = Dates.value(dur.closed)
    pre = Dates.value(dur.pre)
    open = Dates.value(dur.open)
    post = Dates.value(dur.post)
    other = 0.3 * (Dates.value(dur.weekend) + Dates.value(dur.holiday)) # 0.3, and leaving the others at 1 was calculated by SpreadPricing.optTex()
    tex = (closed + pre + open + post + other)/3600.0
    return tex
end
# function calcTex(tsFrom::DateTime, dateTo::Date)::Float64
#     dur = calcDurToExpr(tsFrom, dateTo)
#     closed = Dates.value(dur.closed)
#     pre = Dates.value(dur.pre)
#     open = Dates.value(dur.open)
#     post = Dates.value(dur.post)
#     other = 0.3 * (Dates.value(dur.weekend) + Dates.value(dur.holiday)) # 0.3, and leaving the others at 1 was calculated by SpreadPricing.optTex()
#     tex = (closed + pre + open + post + other)/3600.0
#     return tex
# end

function calcDur(from::DateTime, to::DateTime)::MarketDur
    # TODO: make single call that returns date and time for market tz from DateTime
    fromDate = toDateMarket(from)
    fromTime = toTimeMarket(from)
    toDate = toDateMarket(to)
    toTime = toTimeMarket(to)
    toMt = getMarketTime(toDate)
    if fromDate == toDate
        return calcDurInDay(fromTime, toTime, toMt)
    else
        dur = calcDurInDay(fromTime, TIME_EOD, getMarketTime(fromDate))
        for date in (fromDate + Day(1)):Day(1):(toDate - Day(1))
            dur += marketDur(date)
        end
        dur += calcDurInDay(TIME_ZERO, toTime, toMt)
        return dur
    end
end

calcDurToClose(tsFrom::DateTime, dateTo::Date) = calcDur(tsFrom, getMarketClose(dateTo))
# function calcDurToExpr(tsFrom::DateTime, dateTo::Date)::MarketDur
#     dateFrom = toDateMarket(tsFrom)
#     if dateFrom == dateTo
#         return calcDurToClose(toTimeMarket(tsFrom), getMarketTime(dateTo))
#     else
#         dur = calcDurForDay(tsFrom, getMarketTime(dateFrom))
#         for date in (dateFrom + Day(1)):Day(1):(dateTo - Day(1))
#             dur += marketDur(date)
#         end
#         dur += calcDurToClose(TIME_ZERO, getMarketTime(dateTo))
#         return dur
#     end
# end

calcDurCloses(from::Date, to::Date)::MarketDur = calcDur(getMarketClose(from), getMarketClose(to))

#region Local
mutable struct MarketState
    @atomic isOpen::Bool
    @atomic nextChange::DateTime
    @atomic ts::DateTime
end
const MktTime = Dict{Date,MarketTime}()
const MktDur = Dict{Date,MarketDur}()

import JSON3, Intervals
JSON3.StructType(::Type{MarketTime}) = JSON3.Struct()
JSON3.StructType(::Type{MarketDur}) = JSON3.Struct()
JSON3.StructType(::Type{Intervals.Interval{Time,Intervals.Closed,Intervals.Closed}}) = JSON3.Struct()
JSON3.StructType(::Type{Second}) = JSON3.Struct()

const MktState = MarketState(false, DATETIME_ZERO, DATETIME_ZERO)
const BaseDir = dirData("cal")
const MktTimePath = joinpath(BaseDir, "marktime.json")
const MktDurPath = joinpath(BaseDir, "markdur.json")

function init()
    loadCal()
    updateState()
end
function __init__()
    # println("Running init")
    if ccall(:jl_generating_output, Cint, ()) != 1
        println("Loading Calendars")
        # println("We are actual loading the module for runtime, not caching code to disk. TestData keys: ", keys(TestData))
        init()
    else
        # println("Hit the else. TestData keys: ", keys(TestData))
    end
end

# function __init__()
#     loadMarket()
#     # updateCalendar()
#     updateState()
# end

function loadCal()
    println("Loading mark time/dur")
    @assert isempty(MktTime)
    @assert isempty(MktDur)
    merge!(MktTime, loadJson(MktTimePath, Dict{Date,MarketTime}))
    merge!(MktDur, loadJson(MktDurPath, Dict{Date,MarketDur}))
end
saveCal() = ( writeJson(MktTimePath, MktTime) ; writeJson(MktDurPath, MktDur) )

function check()
    isnothing(snap()) || error("Session timing doesn't work when snapped")
    MktState.nextChange < now(UTC) && updateState()
end

function updateState()
    ts = now(UTC)
    try
        isOpen, nextChange = today() != Date(2022,6,20) ? tradierClock() : (false, DateTime("2022-06-20T20:00:00")) # hard coded because the API missed this new holiday
        @atomic MktState.isOpen = isOpen
        @atomic MktState.nextChange = nextChange
    catch
        if e isa InterruptException
            rethrow(e)
        else
            @warn "Could not Calendars.updateState()"
            @atomic MktState.isOpen = false
            @atomic MktState.nextChange = ts + Day(1)
        end
    end
    @atomic MktState.ts = ts
end

# function updateCalendar(;from=firstdayofyear(today()), to=lastdayofyear(today()))::Nothing
#     Info[] = CalInfo(isOpen, nextChange, Info[].markTime, Info[].markDur, Info[].ts)
#     Info[].ts == DATETIME_ZERO && isfile(MarkDurPath) && loadInfo()
#     !haskey(Info[].markDur, from) || !haskey(Info[].markDur, to) || return
#     @info "updateCalendar" from to stacktrace()
#     @log debug "updateCalendar"
#     if year(from) > 2012
#         cal = tradierCalendar(from, to)
#         markTime = Dict(d => MarketTime(data) for (d, data) in cal)
#         markDur = Dict(d => MarketDur(mt) for (d, mt) in markTime)
#     else
#         markTime = Dict{Date,MarketTime}()
#         markDur = Dict{Date,MarketDur}()
#         for d in from:Day(1):to
#             if isweekend(d)
#                 markTime[d] = MarketDurTypes.MTIME_WEND
#                 markDur[d] = MarketDurTypes.DUR_WEND
#             elseif isBusDay(d)
#                 markTime[d] = MarketDurTypes.MTIME_OPEN
#                 markDur[d] = MarketDurTypes.DUR_OPEN
#             else
#                 markTime[d] = MarketDurTypes.MTIME_HOLIDAY
#                 markDur[d] = MarketDurTypes.DUR_HOLIDAY
#             end
#         end
#     end
#     Info[] = CalInfo(isOpen, nextChange, merge(Info[].markTime, markTime), merge(Info[].markDur, markDur), now(UTC))
#     # if Info[].ts == DATETIME_ZERO
#     #     Info[] = CalInfo(isOpen, nextChange, markTime, markDur, now(UTC))
#     # else
#     #     merge!(Info[].markTime, markTime)
#     #     merge!(Info[].markDur, markDur)
#     #     Info[].ts = now(UTC)
#     # end
#     saveInfo()
#     @log info "Updated calendar" isOpen nextChange
#     return
# end

function addYear(year)
    cal = tradierCalendar(firstdayofyear(Date(year)), lastdayofyear(Date(year)))
    markTime = Dict(d => MarketTime(data) for (d, data) in cal)
    markDur = Dict(d => MarketDur(mt) for (d, mt) in markTime)
    merge!(MktTime, markTime)
    merge!(MktDur, markDur)
end

# ensureCalYear(d::Date) = ensureCal(firstdayofyear(d), lastdayofyear(d))
# ensureCal(dt::Dates.AbstractDateTime...)::Nothing = ensureCal(Date.(dt)...)
function ensureCal(date::Date)::Nothing
    haskey(MktTime, date) || error("Calendar doesn't have date ", date) # updateCalendar(;from, to)
    return
end
function ensureCals(dates::Date...)::Nothing
    for date in dates
        haskey(MktTime, date) || error("Calendar doesn't have date ", date) # updateCalendar(;from, to)
    end
    # from, to = extrema(dt)
    # from = firstdayofyear(from)
    # to = lastdayofyear(to)
    # haskey(Info[].markTime, from) && haskey(Info[].markTime, to) || updateCalendar(;from, to)
    # return
end

# function calcDurPerYear()
#     dur = marketDur(Date(2022,1,1))
#     for day in Date(2022,1,2):Day(1):Date(2022,12,31)
#         dur += marketDur(day)
#     end
#     return dur
# end

marketDur(d::Date)::MarketDur = MktDur[d]
#endregion

# __init__() = println("Calendars.__init__ called")

# println("Calendars top level ran")

end