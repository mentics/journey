module OptionPricing
using Dates, LsqFit, BlackBoxOptim
using GLMakie
using SH, BaseTypes, SmallTypes, ChainTypes, OptionTypes, QuoteTypes, OptionMetaTypes
using DateUtil, FileUtil, OptionUtil
using Calendars, Snapshots

#region Header
AllCalls = NamedTuple[]
AllPuts = NamedTuple[]
CallsItm = nothing
CallsItmXs = nothing
CallsItmExtrins = nothing
BbCallsItm = nothing

const RunRange = Ref{UnitRange{Int64}}(1:1000)
const PandaInit = Ref{Vector{Float64}}(Vector{Float64}())

# logistic(x, x0, mx, k) = mx / (1.0 + ℯ^(-k * (x - x0)))
# logisticF(x0, mx, k) = x -> mx / (1.0 + ℯ^(-k * (x - x0)))
other(x, a, b, c) = a / (b * (x - c))

# eg: abs(getStrike(r.oq) - r.curp) > 100 && getExtrinsic(r.oq, C(r.curp))[3] > 2
findCalls(pred) = filter(pred, AllCalls)
#endregion

#region Results
using OutputUtil
function resids(data=CallsItm, xs=CallsItmXs, ys=CallsItmExtrins, panda=best_candidate(BbCallsItm))
    pretyble([(;data.curp, tex=calcTex(xs[i], panda), calced=modelCallsItm(xs[i], panda), actual=ys[i], diff=(modelCallsItm(xs[i], panda) - ys[i])) for i in RunRange[]])
end

function drawPartials(n)
    data, price = makeData(AllCalls) do x
        getStrike(x.oq) < x.curp
    end
    pts = collect(zip([r[n] for r in data], price))
    scatter(pts)
end

function drawStrice(filt=(x->true))
    data, extrin = filter(filt, runData())
    pts = collect(zip([r[2] for r in data], extrin))
    display(scatter(pts))
    modelPts = [(data[i][2], modelCallsItm(data[i], PandaInit[])) for i in eachindex(data)]
    display(scatter!(modelPts))
end

function drawTexPrice(filt=(x->true))
    data, extrin = filter(filt, runData())
    texs::Vector{Float64} = calcTex.(data, PandaInit)
    pts::Vector{NTuple{2,Float64}} = collect(zip(texs, extrin))
    display(scatter(pts))
    modelPts::Vector{NTuple{2,Float64}} = [(texs[i], modelCallsItm(data[i], PandaInit[])) for i in eachindex(data)]
    display(scatter!(modelPts))
end

function drawRes(panda=best_candidate(BbCallsItm))
    scatter([(x[2], modelCallsItm(x, panda)) for x in CallsItm[1][RunRange[]]])
end

baseStrike(strikeDist) = [414.995, strikeDist, 27.5, 633300.0, 43200.0, 117120.0, 70500.0, 156.0, 414.995/(414.995+strikeDist)]
function drawModelStrike(p=best_candidate(BbCallsItm))
    scatter([(x, modelCallsItm(baseStrike(x), p)) for x in 0.0:-1.0:-250.0])
end
#endregion

function run(;range=1:1000)
    length(AllCalls) > 1000 || readPricing()
    global RunRange[] = range
    if isempty(PandaInit[]) || (length(searchRange()) != length(PandaInit[]))
        PandaInit[] = initDefault() # [0.0160376, 0.0686671, 0.124022, 0.627685, 0.0402868, 0.939992, 0.99831, 86.3807, -88.1671, 0.191875, -66.161, 91.5975, -10.0204, 46.8196, -10.0945, -10.281, 88.2485, -66.8913, 57.3583, -34.4107, 0.973417, 76.4187, -0.383973, 76.1867, -99.7596, 75.5632, -41.3579, 45.3754, 59.4767, -52.3964, 64.0692, 77.6899, -8.29805, -21.0307, 38.899, -0.00562926, 50.718, -64.0306, 0.428358, -24.6193, 95.8691, -7.86242, 0.886855, 72.2335, -99.0591, -31.9659]
    end
    prep()

    global CallsItm = filter(x -> getStrike(x.oq) < x.curp, AllCalls)
    global CallsItmXs, CallsItmExtrins = makeDataExtr(CallsItm)
    data, extrin = (CallsItmXs[1][RunRange[]], CallsItmExtrins[2][RunRange[]])
    global BbCallsItm = bboptimize(modelOptim(data, extrin), PandaInit[]; SearchRange=searchRange(), MaxTime=10) #, NThreads=(Threads.nthreads()-1))
    PandaInit[] = best_candidate(BbCallsItm)
    # drawStrice()
    drawTexPrice()
end

#region Calc
function modelOptim(data, prices)
    function(p)
        return sum((modelCallsItm(data[i], p) - prices[i])^2 for i in eachindex(data))
    end
end

termCount() = 3
termWidth() = 7
searchRange() = vcat(
    fill((0.0, 1.0), 6), # tex
    fill((-100.0, 100.0), 3), # vty
    fill((-200.0, 200.0), termCount() * termWidth()))
initDefault() = [(x[1] + x[2])/2 for x in searchRange()]

function calcTex(x, p)::Float64
    open = x[3]
    pre = p[2] * x[2]
    post = p[3] * x[4]
    closed = p[4] * x[1]
    wend = p[5] * x[5]
    holid = p[6] * x[6]
    tex = p[1] * (closed + pre + open + post + wend + holid)/3600.0
    return tex
end

function calcVty(x, p)::Float64
    curpRat = p[7] * x[8]
    openCloseRat = p[8] * x[9]
    vixOpen = p[9] * x[10]
    return curpRat + openCloseRat + vixOpen
end

# return [tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, tex.weekend.value, tex.holiday.value, strikeRat, curpRat, openCloseRat, row.vixOpen]
function modelCallsItm(x, p)::Float64
    tex = calcTex(x, p)
    off = 7
    vty = calcVty(x, p)
    strikeRat = x[7]
    off += 3

    res = 0.0
    for _ in 1:termCount()
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
#endregion

#region MakeData
function makeDataExtr(filt, allData)
    return (makeX.(data), makeExtrinsic.(data))
end

function makeX(row)::Vector{Float64}
    # (;ts, curp, spyOpen, spyClosePrev, vixOpen, oq)
    strike = getStrike(row.oq)
    strikeRat = row.curp / strike - 1.0
    curpRat = row.curp / row.spyOpen - 1.0
    openCloseRat = row.spyOpen / row.spyClosePrev - 1.0
    tex = Calendars.calcDurToExpr(row.ts, getExpiration(row.oq))
    return [tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, tex.weekend.value, tex.holiday.value, strikeRat, curpRat, openCloseRat, row.vixOpen]
end
makeExtrinsic(row) = getExtrinsic(row.oq, C(row[2]))[3]

function readPricing()::Nothing
    Calendars.ensureCal(Snapshots.earliestSnap(), today() + Month(3))
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
# 2022-05-27T19:58:00,414.995,call,2022-06-06,156.000,open,258.810,259.210,2.27574937513399
function fromRow(row)::Union{Nothing,NamedTuple}
    ts = DateTime(row[1])
    curp = row[2]

    isWithinOpen(ts) || return nothing
    !(ts in badSnaps()) || return nothing
    toTimeMarket(ts) != Time(9,30) || return nothing
    abs(row[5] - curp) < 100 || return nothing

    oq = OptionQuote(Option(to(Style.T, row[3]), Date(row[4]), C(row[5])), Quote(to(Action.T, row[6]), C(row[7]), C(row[8])), OptionMeta(row[9]))
    mktDate = toDateMarket(ts)
    vixOpen = getTicker("VIX", mktDate, "open")
    spyOpen = getTicker("SPY", mktDate, "open")
    spyClosePrev = getTicker("SPY", bdaysBefore(mktDate, 1), "close")
    return (;ts, curp, spyOpen, spyClosePrev, vixOpen, oq)
end

using Intervals
isWithinOpen(ts::DateTime)::Bool = toTimeMarket(ts) in getMarketTime(toDateMarket(ts)).opens

Tickers = Dict{String,Dict{Date,Dict{String,Any}}}()

function getTicker(sym::String, date::Date, ohlc::String)::Float64
    haskey(Tickers, sym) || loadTicker(sym)
    return Tickers[sym][date][ohlc]
end

using TradierData
function loadTicker(sym::String)::Nothing
    @assert !haskey(Tickers, sym)
    data = tradierHistQuotes("daily", today() - Year(1), today(), sym)
    global Tickers[sym] = Dict([Date(d["date"]) => d for d in data])
    return
end
#endregion

end