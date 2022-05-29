module OptionPricing
using Dates, LsqFit, BlackBoxOptim
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
BboptimRes = nothing

using Distributions
const NORM = Normal()

using GLMakie
function drawPartials(n)
    # return [row.curp, strikeDist, row.vixOpen, tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, strike, strikeRat]
    data, price = makeData(AllCalls, x -> getStrike(x.oq) < x.curp)
    pts = collect(zip([r[n] for r in data], price))
    scatter(pts)
end

function run()
    Calendars.ensureCal(Snapshots.earliestSnap(), today() + Month(3))
    length(AllCalls) > 1000 || readPricing()
    length(DataCalls) > 1000 || prepData()

    # return (;ts, curp, vixOpen, oq)
    data, price = makeData(AllCalls, x -> getStrike(x.oq) < x.curp)
    println(length(data), ' ', length(price))
    compare_optimizers(modelOptim(data, price); SearchRange, MaxTime=10, NThreads=(Threads.nthreads()-1))
    # global BboptimRes = bboptimize(modelOptim(data, price); SearchRange, MaxTime=10)

    # xs = reduce(hcat, DataCalls)'
    # global ModelCalls = curve_fit(modelAll, xs, PriceCalls, fill(0.1, 9); lower=fill(-Inf, 9), upper=fill(Inf, 9))
    # return ModelCalls
end

function modelOptim(data, prices)
    function(p)
        return sum((modelCallsItm(data[i], p) - prices[i])^2 for i in eachindex(data))
    end
end

# function modelAll(xs, p)
#     res = [modelCalls(x, p) for x in eachrow(xs)]
#     # println(length(res), ' ', res[1])
#     println(p)
#     return res
# end

SearchRange = vcat(fill((0.0, 1.0), 5), fill((-1.0, 1.0), 240))
# [
#     (0., 1.),
#     (0., 1.),
#     (0., 1.),
#     (0., 1.), # times
#     (-1., 1.), # vix

#     (0., 1.),
#     (0, 0),
#     (0., 1.)
# ]

# return [row.curp, strikeDist, row.vixOpen, tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, strike, strikeRat]
function modelCallsItm(x, p)::Float64
    closed = p[1] * x[4]
    pre = p[2] * x[5]
    open = p[3] * x[6]
    post = p[4] * x[7]
    tex = closed + pre + open + post
    vty = p[5] * x[3] # vixOpen
    strikeRat = x[9]
    rrat = sqrt(strikeRat)
    lrat = log(strikeRat)
    invrat = 1.0 / strikeRat

    rtex = sqrt(tex)
    ltex = log(tex)
    invtex = 1.0 / tex
    inv2tex = 1.0 / (tex^2)

    rvty = sqrt(vty)
    lvty = log(vty)
    invty = 1.0 / vty

    terms = (
        (strikeRat, rrat, lrat, invrat),
        (tex, rtex, ltex, invtex, inv2tex),
        (rvty, lvty, invty)
    )

    pind = 6
    res = 0.0
    for i in eachindex(terms[1])
        for j in eachindex(terms[2])
            for k in eachindex(terms[3])
                res += p[pind] * terms[1][i] * terms[2][j] * terms[3][k] ; pind += 1

                res += p[pind] * terms[1][i] * terms[2][j] ; pind += 1
                res += p[pind] * terms[1][i] * terms[3][k] ; pind += 1
                res += p[pind] * terms[2][j] * terms[3][k] ; pind += 1
            end
        end
    end

    extrinsic = res

    strikeDist = x[2]
    return -strikeDist + extrinsic
end

function modelCallsOtm(x, p)::Float64
    curp = p[1] * x[1]
    strikeDist = p[2] * x[2]
    vty = p[3] * x[3] # vixOpen

    closed = p[4] * x[4]
    pre = p[5] * x[5]
    open = p[6] * x[6]
    post = p[7] * x[7]

    # strike = p[8] * x[8]
    strikeRat = p[9] * p[9]

    tex = closed + pre + open + post
    rtex = sqrt(tex)
    ltex = log(tex)

    rvty = sqrt(vty)
    lvty = log(vty)
    invty = 1.0 / vty

    return extrinsic
end

function makeData(allData, filt)
    data = filter(filt, allData)
    return (makeX.(data), makePrice.(data))
end

function prepData()::Nothing
    global DataCalls = makeX.(AllCalls)
    global PriceCalls = makePrice.(AllCalls)
    global DataPuts = makeX.(AllPuts)
    global PricePuts = makePrice.(AllPuts)
    return nothing
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
    data = tradierHistQuotes("daily", today() - Year(1), today(), "VIX")
    global vixDaily = Dict([Date(d["date"]) => d for d in data])
    return
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

end