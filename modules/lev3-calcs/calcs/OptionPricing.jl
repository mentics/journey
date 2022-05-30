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
CallsItm = nothing

function model1(xs, p)
    # @info "model1" xs p
    return @. p[1] / (p[2] * (xs - p[3]))
end

function model2(xs, p)
    return @. p[1] / (p[2] * (xs - p[3])) + p[4] / (p[5] * (xs - p[6])) + p[7] / (p[8] * (xs - p[9])^2)
end

fit1(xys) = p -> sum((model2(xys[:,1], p) .- xys[:,2]).^2)

function test()
    data = [-100000. .0; -300. 1.; -100. 3.; -50 8.; .0 20.]
    # println("test1: ", model1(data, [1., 2., 3.]))
    # println("test2: ", fit1(data)([1., 2., 3.]))
    SearchRange = fill((-100., 100.), 9)
    res = bboptimize(fit1(data); SearchRange, MaxTime=2)
    bc = best_candidate(res)
    println("result: ", model1(data, bc))
    return (bc, res)
end

logistic(x, x0, mx, k) = mx / (1.0 + ℯ^(-k * (x - x0)))
logisticF(x0, mx, k) = x -> mx / (1.0 + ℯ^(-k * (x - x0)))
other(x, a, b, c) = a / (b * (x - c))

function findbad()
    filter(AllCalls) do r
        strikeDist = abs(getStrike(r.oq) - r.curp)
        extr = getExtrinsic(r.oq, C(r.curp))[3]
        return strikeDist > 100 && extr > 2
    end
end

function findbad2()
    filter(AllCalls) do r
        strikeDist = getStrike(r.oq) - r.curp
        extr = getExtrinsic(r.oq, C(r.curp))[3]
        return strikeDist <= -100 && extr > 5
    end
end

function findbad3()
    filter(AllCalls) do r
        return abs(getBid(r.oq) - getAsk(r.oq)) > 0.7
    end
end

function resids(panda=best_candidate(BboptimRes))
    [(CallsItm[1][i][2], modelCallsItm(CallsItm[1][i], panda), CallsItm[2][i], (modelCallsItm(CallsItm[1][i], panda) - CallsItm[2][i])^2) for i in RunRange[]]
    # [modelOptim(CallsItm[1][i], CallsItm[2][i])(panda) for i in 1:length(CallsItm[1])]
end

function prep()
    Calendars.ensureCal(Snapshots.earliestSnap(), today() + Month(3))
    length(AllCalls) > 1000 || readPricing()
end

using GLMakie
function drawPartials(n)
    # return [row.curp, strikeDist, row.vixOpen, tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, strike, strikeRat]
    data, price = makeData(AllCalls) do x
        getStrike(x.oq) < x.curp
    end
    pts = collect(zip([r[n] for r in data], price))
    scatter(pts)
end

function drawStrice(filt=(x->true))
    # return [row.curp, strikeDist, row.vixOpen, tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, strike, strikeRat]
    data, extrin = makeDataExtr(AllCalls) do x
        getStrike(x.oq) < x.curp && filt(x)
    end
    pts = collect(zip([r[2] for r in data], extrin))
    scatter(pts)

    scatter!(pts)
end

function drawRes(panda=best_candidate(BboptimRes))
    scatter([(x[2], modelCallsItm(x, panda)) for x in CallsItm[1][RunRange[]]])
end

const RunRange = Ref{UnitRange{Int64}}(1:1000)
const PandaInit = Ref{Vector{Float64}}(Vector{Float64}())
function run(;range=1:1000)
    global RunRange[] = range
    if isempty(PandaInit[])
        PandaInit[] = initDefault() # [0.0160376, 0.0686671, 0.124022, 0.627685, 0.0402868, 0.939992, 0.99831, 86.3807, -88.1671, 0.191875, -66.161, 91.5975, -10.0204, 46.8196, -10.0945, -10.281, 88.2485, -66.8913, 57.3583, -34.4107, 0.973417, 76.4187, -0.383973, 76.1867, -99.7596, 75.5632, -41.3579, 45.3754, 59.4767, -52.3964, 64.0692, 77.6899, -8.29805, -21.0307, 38.899, -0.00562926, 50.718, -64.0306, 0.428358, -24.6193, 95.8691, -7.86242, 0.886855, 72.2335, -99.0591, -31.9659]
    end
    prep()

    global CallsItm = makeDataExtr(x -> getStrike(x.oq) < x.curp, AllCalls)
    data, extrin = (CallsItm[1][RunRange[]], CallsItm[2][RunRange[]])
    # println(length(data), ' ', length(extrin))
    global BboptimRes = bboptimize(modelOptim(data, extrin), PandaInit[]; SearchRange=searchRange(), MaxTime=10) #, NThreads=(Threads.nthreads()-1))
    PandaInit[] = best_candidate(BboptimRes)
    drawModelStrike()
    # xs = reduce(hcat, DataCalls)'
    # global ModelCalls = curve_fit(modelAll, xs, PriceCalls, fill(0.1, 9); lower=fill(-Inf, 9), upper=fill(Inf, 9))
end

function modelOptim(data, prices)
    function(p)
        return sum((modelCallsItm(data[i], p) - prices[i])^2 for i in eachindex(data))
    end
end

baseStrike(strikeDist) = [414.995, strikeDist, 27.5, 633300.0, 43200.0, 117120.0, 70500.0, 156.0, 414.995/(414.995+strikeDist)]

function drawModelStrike(p=best_candidate(BboptimRes))
    scatter([(x, modelCallsItm(baseStrike(x), p)) for x in 0.0:-1.0:-250.0])
end

termCount() = 3
termWidth() = 7
searchRange() = vcat(fill((0.0, 1.0), 7), fill((-200.0, 200.0), termCount() * termWidth()))
initDefault() = [(x[1] + x[2])/2 for x in searchRange()]

# return [row.curp, strikeDist, row.vixOpen, tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, strike, strikeRat]
function modelCallsItm(x, p)::Float64
    closed = p[1] * x[4]
    pre = p[2] * x[5]
    open = p[3] * x[6]
    post = p[4] * x[7]

    tex = (closed + pre + open + post)/3600.0
    vty = p[5] * x[3]
    strikeRat = p[6] * x[9]
    off = 7

    res = 0.0
    for _ in 1:termCount()
        # x0 = p[off] * tex + p[off+1] * vty + p[off+2] * strikeRat
        # off += 3
        # mx = p[off] * tex + p[off+1] * vty + p[off+2] * strikeRat
        # off += 3
        # k = p[off] * tex + p[off+1] * vty + p[off+2] * strikeRat
        # off += 3
        # x = p[off] * tex + p[off+1] * vty + p[off+2] * strikeRat
        # off += 3

        # ll = other(x, x0, mx, k)
        # res += p[off] * ll
        # # @info "res" res ll off p[off] x x0 mx k
        # off += 1

        x = p[off] * tex + p[off+1] * vty + p[off+2] * strikeRat
        off += 3

        ll = other(x, p[off], p[off+1], p[off+2])
        off += 3
        res += p[off] * ll
        # @info "res" res ll off p[off] x x0 mx k
        off += 1
    end
    # TODO: add line?
    return res < 0.0 ? 100000*res : res
end

function makeDataExtr(filt, allData)
    data = filter(filt, allData)
    return (makeX.(data), makeExtrinsic.(data))
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
makeExtrinsic(row) = getExtrinsic(row.oq, C(row[2]))[3]

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

# TODO: these had bad looking data, should delete them?
badSnaps() = [ DateTime("2022-04-11T13:38:00"), DateTime("2022-05-18T13:30:00") ]

# TODO: add curp - open and open - closePrev to capture the intraday vty

function fromRow(row)::Union{Nothing,NamedTuple}
    ts = DateTime(row[1])
    isWithinOpen(ts) || return nothing
    !(ts in badSnaps()) || return nothing
    toTimeMarket(ts) != Time(9,30) || return nothing
    curp = row[2]
    oq = OptionQuote(Option(to(Style.T, row[3]), Date(row[4]), C(row[5])), Quote(to(Action.T, row[6]), C(row[7]), C(row[8])), OptionMeta(row[9]))
    vixOpen = getVixOpen(toDateMarket(ts))
    return (;ts, curp, vixOpen, oq)
end

end