module OptionPricing
using Dates, LsqFit
using SH, BaseTypes, SmallTypes, ChainTypes, OptionTypes, QuoteTypes, OptionMetaTypes
using DateUtil, FileUtil, OptionUtil
using Calendars, Snapshots

AllCalls = NamedTuple[]
AllPuts = NamedTuple[]
DataCalls = Vector{NamedTuple}()
PriceCalls = Vector{Float64}()
DataPuts = Vector{NamedTuple}()
PriceCalls = Vector{Float64}()
ModelCalls = nothing
ModelPuts = nothing

using Distributions
const NORM = Normal()

function modelAll(xs, p)
    [model(x, p) for x in eachrow(xs)]
end

function model(x, p)::Float64
    curp = p[1] * x[1]
    strikeDist = p[2] * x[2]
    vixOpen = p[3] * x[3]
    closed = p[4] * x[4]
    pre = p[5] * x[5]
    open = p[6] * x[6]
    post = p[7] * x[7]
    strike = p[8] * x[8]
    strikeRat = p[9] * p[9]
    tex = closed + pre + open + post
    rtex = sqrt(tex)

    d1 = (log(strikeRat) + (vixOpen * vixOpen / 2.0) * tex) / (vixOpen * rtex)
    d2 = d1 - vixOpen * rtex

    # if style == Style.put
        r = cdf(NORM, -d2) * strike - cdf(NORM, -d1) * curp
    # else
    #     r = under * cdf(NORM, d1) - exp(-rfrate * toExpYear) * strike * cdf(NORM, d2)
    # end
    return r
end

function run()
    Calendars.ensureCal(Snapshots.earliestSnap(), today() + Month(3))
    length(AllCalls) > 1000 || readPricing()
    length(DataCalls) > 1000 || prepData()

    xs = reduce(hcat, DataCalls)'

    global ModelCalls = curve_fit(modelAll, xs, PriceCalls, fill(1.0, 9))
    # global ModelPuts = curve_fit(model, DataPuts, PricePuts, (1.0, 1.0, 1.0, 1.0))

    return ModelCalls
end

function prepData()::Nothing
    global DataCalls = makeX.(AllCalls)
    global PriceCalls = makePrice.(AllCalls)
    global DataPuts = makeX.(AllPuts)
    global PricePuts = makePrice.(AllPuts)
    return nothing
end

function readPricing()::Nothing
    basePath = "C:/data/tmp/pricing"
    global AllCalls = NamedTuple[]
    global AllPuts = NamedTuple[]
    for data in (AllCalls, AllPuts)
        for row in eachrow(readCsv(joinpath(basePath, "calls.csv")))
            row = fromRow(row)
            isnothing(row) || push!(data, row)
        end
    end
    return
end

function fromRow(row)::Union{Nothing,NamedTuple}
    ts = DateTime(row[1])
    isWithinOpen(ts) || return nothing
    curp = row[2]
    oq = OptionQuote(Option(to(Style.T, row[3]), Date(row[4]), C(row[5])), Quote(to(Action.T, row[6]), C(row[7]), C(row[8])), OptionMeta(row[9]))
    vixOpen = getVixOpen(toDateMarket(ts))
    return (;ts, curp, vixOpen, oq)
end

function makeX(row)::Vector{Float64}
    # return (;ts, curp, vixOpen, oq)
    strike = getStrike(row.oq)
    strikeDist = strike - row.curp
    strikeRat = row.curp / strike
    tex = Calendars.calcDurToExpr(row.ts, getExpiration(row.oq))
    # return (;tex, row.curp, strikeDist, row.vixOpen)
    # return (tex, row.curp, strikeDist, row.vixOpen)
    return [row.curp, strikeDist, row.vixOpen, tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, strike, strikeRat]
end
makePrice(row) = getMid(getQuote(row.oq))

using Intervals
isWithinOpen(ts::DateTime)::Bool = toTimeMarket(ts) in Interval(marketTime(toDateMarket(ts)).opens...)

vixDaily = Dict{Date,Dict{String,Any}}()

function getVixOpen(date::Date)::Float64
    length(vixDaily) > 10 || loadVixData()
    return vixDaily[date]["open"]
end

using TradierData
function loadVixData()::Nothing
    data = tradierHistQuotes("daily", today() - Year(1), today())
    global vixDaily = Dict([Date(d["date"]) => d for d in data])
    return
end

end