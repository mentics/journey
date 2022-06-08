module PricingUtil
using Dates, Intervals
using SH, BaseTypes, SmallTypes, OptionTypes, QuoteTypes, ChainTypes, OptionMetaTypes
using DateUtil, FileUtil, OutputUtil, OptionUtil
using Calendars, Snapshots, TradierData

export logistic, otherFunc
export readSpreads, readPricing, filterTs, getTicker, runData, setup
export calcTex

# logistic(x, x0, mx, k) = mx / (1.0 + ℯ^(-k * (x - x0)))
# logisticF(x0, mx, k) = x -> mx / (1.0 + ℯ^(-k * (x - x0)))
otherFunc(x, a, b, c) = a / (b * (x - c))

# function calcTex(xmat)
#     closed = xmat[:,1]
#     pre = xmat[:,2]
#     open = xmat[:,3]
#     post = xmat[:,4]
#     wend = 0.3 * xmat[:,5] # 0.3, and leaving the others at 1 was calculated by SpreadPricing.optTex()
#     holid = 0.3 * xmat[:,6]
#     tex = @. (closed + pre + open + post + wend + holid)/3600.0
#     return tex
# end
#===
SpreadPricing.optTex()
Fitness: 0.651475702

4-element Vector{Float64}:
 0.999976374449186
 0.9999949770428279
 0.9999795628925922
 0.29912893120239725
===#

# function calcTex(xmat, p, off)
#     closed = p[off] * xmat[:,1]
#     pre = p[off+1] * xmat[:,2]
#     open = xmat[:,3]
#     post = p[off+2] * xmat[:,4]
#     wend = p[off+3] * xmat[:,5]
#     # holid = p[off+4] * xmat[:,6] # TODO: maybe consider later
#     holid = p[off+3] * xmat[:,6]
#     tex = @. (closed + pre + open + post + wend + holid)/3600.0
#     return tex
# end

# function setup(filt, data, make)
#     global RunSource = filter(filt, data)
#     return runData(make(RunSource))
# end

runData() = RunData
function runData(data)
    global RunData = (data[1][RunRange[]], data[2][RunRange[]])
    return RunData
end

function readSpreads()::Nothing
    isempty(AllSpreadCalls) || return
    Calendars.ensureCal(Snapshots.earliestSnap(), today() + Month(3))
    basePath = "C:/data/tmp/pricing"
    empty!(AllSpreadCalls)
    empty!(AllSpreadPuts)
    for row in eachrow(readCsv(joinpath(basePath, "spreads", "spread-calls.csv")))
        row = fromRowSpreads(row)
        isnothing(row) || push!(AllSpreadCalls, row)
    end
    for row in eachrow(readCsv(joinpath(basePath, "spreads", "spread-puts.csv")))
        row = fromRowSpreads(row)
        isnothing(row) || push!(AllSpreadPuts, row)
    end
    return
end

function readPricing()::Nothing
    Calendars.ensureCal(Snapshots.earliestSnap(), today() + Month(3))
    basePath = "C:/data/tmp/pricing"
    empty!(AllCalls)
    empty!(AllPuts)
    for row in eachrow(readCsv(joinpath(basePath, "calls.csv")))
        row = fromRowPricing(row)
        isnothing(row) || push!(AllCalls, row)
    end
    for row in eachrow(readCsv(joinpath(basePath, "puts.csv")))
        row = fromRowPricing(row)
        isnothing(row) || push!(AllPuts, row)
    end
    return
end

function getTicker(sym::String, date::Date, ohlc::String)::Float64
    haskey(Tickers, sym) || loadTicker(sym)
    return Tickers[sym][date][ohlc]
end

function resids(data=CallsItm, xs=CallsItmXs, ys=CallsItmExtrins, panda=best_candidate(BbCallsItm))
    pretyble([(;data.curp, tex=calcTex(xs[i], panda), calced=model(xs[i], panda), actual=ys[i], diff=(model(xs[i], panda) - ys[i])) for i in RunRange[]])
end

#region Local
const AllCalls = Vector{NamedTuple}()
const AllPuts = Vector{NamedTuple}()
const AllSpreadCalls = Vector{NamedTuple}()
const AllSpreadPuts = Vector{NamedTuple}()
const RunRange = Ref{UnitRange{Int64}}(1:1000)
const Tickers = Dict{String,Dict{Date,Dict{String,Any}}}()
RunSource = nothing
RunData = nothing
BbRes = nothing

# TODO: these had bad looking data, should delete them?
badSnaps() = [ DateTime("2022-04-11T13:38:00"), DateTime("2022-05-18T13:30:00") ]
isWithinOpen(ts::DateTime)::Bool = toTimeMarket(ts) in getMarketTime(toDateMarket(ts)).opens
filterTs(ts) = isWithinOpen(ts) && !(ts in badSnaps()) && toTimeMarket(ts) != Time(9,30)

oqFromTuple(tup) = OptionQuote(Option(to(Style.T, tup[1]), Date(tup[2]), C(tup[3])), Quote(to(Action.T, tup[4]), C(tup[5]), C(tup[6])), OptionMeta(tup[7]))

function fromRowSpreads(row)::Union{Nothing,NamedTuple}
    ts = DateTime(row[1])
    curp = row[2]

    filterTs(ts) || return nothing
    abs(row[5] - curp) < 100 || return nothing

    oq1 = oqFromTuple((row[3:9]...,))
    oq2 = oqFromTuple((row[10:16]...,))

    calcExtrin(oq1, curp)[1] >= 0.0 || return nothing
    calcExtrin(oq2, curp)[1] >= 0.0 || return nothing

    # oq = OptionQuote(Option(to(Style.T, row[3]), Date(row[4]), C(row[5])), Quote(to(Action.T, row[6]), C(row[7]), C(row[8])), OptionMeta(row[9]))
    mktDate = toDateMarket(ts)
    vixOpen = getTicker("VIX", mktDate, "open")
    spyOpen = getTicker("SPY", mktDate, "open")
    spyClosePrev = getTicker("SPY", bdaysBefore(mktDate, 1), "close")
    return (;ts, curp, spyOpen, spyClosePrev, vixOpen, oq1, oq2)
end

# 2022-05-27T19:58:00,414.995,call,2022-06-06,156.000,open,258.810,259.210,2.27574937513399
function fromRowPricing(row)::Union{Nothing,NamedTuple}
    ts = DateTime(row[1])
    curp = row[2]

    filterTs(ts) || return nothing
    abs(row[5] - curp) < 100 || return nothing

    oq = oqFromTuple((row[3:9]...,))
    mktDate = toDateMarket(ts)
    vixOpen = getTicker("VIX", mktDate, "open")
    spyOpen = getTicker("SPY", mktDate, "open")
    spyClosePrev = getTicker("SPY", bdaysBefore(mktDate, 1), "close")
    return (;ts, curp, spyOpen, spyClosePrev, vixOpen, oq)
end

function loadTicker(sym::String)::Nothing
    @assert !haskey(Tickers, sym)
    data = tradierHistQuotes("daily", today() - Year(1), today(), sym)
    global Tickers[sym] = Dict([Date(d["date"]) => d for d in data])
    return
end
#endregion

end