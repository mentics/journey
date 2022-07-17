module OptionPricing
using Dates, BlackBoxOptim
# using GLMakie
using SH, BaseTypes, SmallTypes, ChainTypes, OptionTypes, QuoteTypes, OptionMetaTypes
using DateUtil, FileUtil, OptionUtil, PricingUtil, DrawUtil
using Calendars, Snapshots

#region Header
# AllCalls = NamedTuple[]
# AllPuts = NamedTuple[]
# CallsItm = nothing
# CallsItmXs = nothing
# CallsItmExtrins = nothing
# BbCallsItm = nothing

const RunRange = Ref{UnitRange{Int64}}(1:1000)
const PandaInit = Ref{Vector{Float64}}(Vector{Float64}())

# logistic(x, x0, mx, k) = mx / (1.0 + ℯ^(-k * (x - x0)))
# logisticF(x0, mx, k) = x -> mx / (1.0 + ℯ^(-k * (x - x0)))
fit1(x, a, b, c) = a / (b * (x - c))

using Distributions
function fit2(t, pos, vty, rfr)
    t = abs(t)
    t < 1.0 || (t = 1.0 + log(abs(t)))
    abs(rfr) < 1.0 || (rfr = sign(rfr) + sign(rfr) * log(abs(rfr)))
    rfrate = rfr
    rt = sqrt(abs(t))
    d1 = (log(abs(pos)) + (rfrate + vty^2 / 2.0) * t) / (vty * rt)
    d2 = d1 - vty * rt
    r = cdf(BlacksPricing.NORM, d1) - exp(-rfrate * t) * pos * cdf(BlacksPricing.NORM, d2)
    if isnan(r)
        @error t pos vty rfr rt d1 d2 cdf(BlacksPricing.NORM, d1) (-rfrate * t) exp(-rfrate * t) cdf(BlacksPricing.NORM, d2)
        error("stop")
    end
    return r
end

# eg: abs(getStrike(r.oq) - r.curp) > 100 && getExtrinsic(r.oq, C(r.curp))[3] > 2
#endregion

#region exploring
function run1()
    prepDataAll()
    Method = :adaptive_de_rand_1_bin_radiuslimited
    # https://github.com/robertfeldt/BlackBoxOptim.jl/blob/master/examples/benchmarking/latest_toplist.csv
    # :xnes: got stuck around .004
    # :adaptive_de_rand_1_bin : slowly continued down
    # :generating_set_search: ?
    # :separable_nes: stuck around .0032
    # :adaptive_de_rand_1_bin_radiuslimited: Doc said this is "go-to"... keeps going down
    SearchRange = vcat(fill((0.0, 1.0), 3), fill((-100.0, 100.0), numParams()-3))
    (!isempty(PandaInit[]) && length(PandaInit[]) == numParams()) || (PandaInit[] = [(x[1] + x[2])/2 for x in SearchRange])

    funcToOpt = modelOptim1(Store)
    global BbCallsItm = bboptimize(funcToOpt, PandaInit[]; SearchRange, MaxTime=10000000, Method)
    #, NThreads=(Threads.nthreads()-1))
    PandaInit[] = best_candidate(BbCallsItm)
    drawResult(specs[1])
end

Store = Dict()
function makeSpecs(numSpecs::Int)::Vector
    allexps = unique!(map(x -> getExpiration(x.oq), PricingUtil.AllCalls))
    exps = filter(x -> Date(2022, 5, 1) <= x <= Date(2022, 6, 10), allexps)
    dists = 1.0:1.0:20.0
    data = vec([(;exp, dist) for exp in exps, dist in dists])
    num = min(length(data), numSpecs)
    specs = sample(data, num; replace=false)
    return specs
end

function prepDataAll(numSpecs=20)
    specs = makeSpecs(numSpecs)
    empty!(Store)
    # prevSpec = nothing
    for spec in specs
        # l1 = length(Store)
        Store[spec] = prepData(spec)
        # l2 = length(Store)
        # if l1 == l2
        #     @info "specs" l1 l2 spec prevSpec
        # end
        # prevSpec = spec
    end
end

function prepData(spec)
    (;exp, dist) = spec
    global orig = Vector{NamedTuple}()
    global xs = Vector{NamedTuple}()
    global ys = Vector{Float64}()
    global actual = Vector{Tuple{Float64,Float64}}()
    global actualTime = Vector{Tuple{DateTime,Float64}}()
    # texes = Vector{Float64}()
    push!(ys, 0.0)
    push!(xs, DATA1_ZERO)
    push!(actual, (0.0, 0.0))
    push!(actualTime, (getMarketClose(exp), 0.0))
    procCalls() do nt
        (;ts, curp, oq) = nt
        strike = getStrike(oq)
        getExpiration(oq) == exp && dist - 0.5 < strike - curp < dist + 0.5 || return
        datum = makeData1(nt)
        extrin3 = calcExtrins(oq, curp)
        y = extrin3[3] / curp
        push!(ys, y) # 3 = imp
        push!(xs, datum)
        push!(actual, (datum.tex, extrin3[3]))
        push!(actualTime, (ts, extrin3[3]))
        push!(orig, merge(nt, (;tex=datum.tex)))
        # push!(texes, datum.tex)
    end
    # model1(xs[1], zeros(100))
    # println(length(xs), length(actual), length(ys))
    return (; orig, xs, ys, actual, actualTime)
end

import BlacksPricing
function drawResult(spec)
    info = Store[spec]
    display(drawDots(info.actual; color=:white))
    modeled = [(x.tex, model1(x, PandaInit[]) * x.curp) for x in info.xs]
    # global blacks = map(orig) do o
    #     (; tex, curp, oq) = o
    #     # TODO: try not cheating
    #     vty = getIv(oq)
    #     y = BlacksPricing.priceOption(getStyle(oq), Float64(getStrike(oq)), .575 * OptionUtil.texToYear(tex), vty, curp)
    #     # y = BlacksPricing.priceOption(getStyle(oq), Float64(getStrike(oq)), OptionUtil.texToYear(tex), vty, curp)
    #     return (tex, y)
    # end
    # # draw!(blacks)
    drawDots!(modeled; color=:green)
    err = [abs(modeled[i][2] - info.actual[i][2]) for i in eachindex(modeled)]
    perm = sortperm(err; rev=true)
    sm = modeled[perm]
    sa = info.actual[perm]

    # mx = maximum(abs(modeled[i][2] - info.actual[i][2]) for i in eachindex(modeled))
    println(abs(sm[1][2] - sa[1][2]))
    # @assert mx == abs(sm[1][2] - sa[1][2])
    return sm[1:10]
end
function drawActualTime()
    display(draw(map(tup -> (datetime2unix(tup[1]), tup[2]), actualTime)))
    return
end

function makeData1(nt::NamedTuple)
    (;ts, curp, oq, spyOpen, spyClosePrev) = nt
    tex = calcTex(ts, getExpiration(oq))
    strike = getStrike(oq)
    strikeRat =  strike / curp - 1.0
    vty1 = PricingUtil.calcVty2(Date(ts))
    vty2 = PricingUtil.calcVty3(Date(ts))
    vty3 = PricingUtil.calcVtyNeg(Date(ts))
    curpRat = curp / spyOpen - 1.0
    openCloseRat = spyOpen / spyClosePrev - 1.0
    return (;tex, strikeRat, vty1, vty2, vty3, curpRat, openCloseRat, curp)
end
DATA1_ZERO = (;tex=0.0, strikeRat=0.01, vty1=0.01, vty2=0.01, vty3=0.01, curpRat=0.01, openCloseRat=0.01, curp=1.0)

# function modelOptim1(xs::Vector{NamedTuple}, ys::Vector{Float64})
function modelOptim1(dict)
    # data = collect(values(d))
    xs = vcat(map(x -> x.xs, values(dict))...)
    ys = vcat(map(x -> x.ys, values(dict))...)
    function(p)
        s = 0.0
        for i in 1:length(xs)
            s += (model1(xs[i], p) - ys[i])^2
        end
        return s
    end
end

numParams() = 3 + 2 * (4 * 5 * 4)
function model1(x::NamedTuple, p::Vector{Float64})
    off = 1
    texs = varia(x.tex)
    vtys = varia(linsum((x.vty1, x.vty2, x.vty3), p, off))
    off += 3
    strikeRats = varia(x.strikeRat)
    curpRats = varia(x.curpRat)
    openCloseRats = varia(x.openCloseRat)
    xs = vcat(texs, vtys, strikeRats, curpRats, openCloseRats)
    n = length(xs)
    x1 = linsum(xs, p, off)
    off += n
    x2 = linsum(xs, p, off)
    off += n
    x3 = linsum(xs, p, off)
    off += n
    x4 = linsum(xs, p, off)
    off += n
    res1 = fit1(x1, x2, x3, x4)

    x1 = linsum(xs, p, off)
    off += n
    x2 = linsum(xs, p, off)
    off += n
    x3 = linsum(xs, p, off)
    off += n
    x4 = linsum(xs, p, off)
    off += n
    res2 = fit2(x1, x2, x3, x4)

    res = res1 + res2
    # println("numParams: ", off+2)
    return res < 0.0 ? 100000*res : res
end

using EnergyStatistics
function checkCorr()
    xs = vcat(map(x -> x.xs, values(Store))...)[2:end]
    ys = vcat(map(x -> x.ys, values(Store))...)[2:end]
    dcorys = EnergyStatistics.dcenter!(EnergyStatistics.DistanceMatrix(ys))
    valsRand = rand(length(ys))
    valsConst = rand(length(ys))
    dcorand = EnergyStatistics.dcenter!(EnergyStatistics.DistanceMatrix(valsRand))
    dcoronst = EnergyStatistics.dcenter!(EnergyStatistics.DistanceMatrix(valsConst))
    global corrs = Dict()
    for var in keys(xs[1])
        plain = map(x -> x[var], xs)
        v = variaLabeled(plain)
        for term in keys(v)
            vals = v[term]
            dcorxs = EnergyStatistics.dcenter!(EnergyStatistics.DistanceMatrix(vals))

            dc = dcor(dcorys, dcorxs)
            dcRand = dcor(dcorand, dcorxs)
            dcConst = dcor(dcoronst, dcorxs)

            key = (var, term)
            r = (; key, vals, dc, dcRand, dcConst)
            # label = string(var, '-', label)
            println(key, ": ", r.dc, ' ', r.dcRand, ' ', r.dcConst)
            corrs[key] = r
        end
    end

    global corrMults = Dict()
    for nt1 in values(corrs)
        for nt2 in values(corrs)
            nt1.key != nt2.key || continue
            vals = nt1.vals .* nt2.vals
            dcorxs = EnergyStatistics.dcenter!(EnergyStatistics.DistanceMatrix(vals))

            dc = dcor(dcorys, dcorxs)
            dcRand = dcor(dcorand, dcorxs)
            dcConst = dcor(dcoronst, dcorxs)

            key = (nt1.key, nt2.key)
            r = (; key, vals, dc, dcRand, dcConst)
            println(key, ": ", r.dc, ' ', r.dcRand, ' ', r.dcConst)
            corrMults[key] = r
        end
    end

    res = sort(collect(Iterators.flatten((values(corrs), values(corrMults)))); rev=true, by=x -> x.dc / ((x.dcRand + x.dcConst)/2))
    return res
end
#endregion

#region Results
using OutputUtil
function resids(data=CallsItm, xs=CallsItmXs, ys=CallsItmExtrins, panda=best_candidate(BbCallsItm))
    pretyble([(;data.curp, tex=calcTex(xs[i], panda), calced=model(xs[i], panda), actual=ys[i], diff=(model(xs[i], panda) - ys[i])) for i in RunRange[]])
end

function drawStrice(filt=(x->true))
    data, extrin = filter(filt, runData())
    pts = collect(zip([r[2] for r in data], extrin))
    display(scatter(pts))
    modelPts = [(data[i][2], model(data[i], PandaInit[])) for i in eachindex(data)]
    display(scatter!(modelPts))
end

function drawPartials(n)
    data, price = makeData(AllCalls) do x
        getStrike(x.oq) < x.curp
    end
    pts = collect(zip([r[n] for r in data], price))
    scatter(pts)
end

function drawTexPrice(filt=(x->true))
    data, extrin = filter(filt, runData())
    texs::Vector{Float64} = calcTex.(data, PandaInit, 1)
    pts::Vector{NTuple{2,Float64}} = collect(zip(texs, extrin))
    display(scatter(pts))
    modelPts::Vector{NTuple{2,Float64}} = [(texs[i], model(data[i], PandaInit[])) for i in eachindex(data)]
    display(scatter!(modelPts))
end

function drawRes(panda=best_candidate(BbCallsItm))
    scatter([(x[2], model(x, panda)) for x in CallsItm[1][RunRange[]]])
end

baseStrike(strikeDist) = [414.995, strikeDist, 27.5, 633300.0, 43200.0, 117120.0, 70500.0, 156.0, 414.995/(414.995+strikeDist)]
function drawModelStrike(p=best_candidate(BbCallsItm))
    scatter([(x, model(baseStrike(x), p)) for x in 0.0:-1.0:-250.0])
end
#endregion

runData() = (CallsItmXs[RunRange[]], CallsItmExtrins[RunRange[]])

function run(;range=1:1000)
    length(AllCalls) > 1000 || readPricing()
    global RunRange[] = range
    if isempty(PandaInit[]) || (length(searchRange()) != length(PandaInit[]))
        PandaInit[] = initDefault() # [0.0160376, 0.0686671, 0.124022, 0.627685, 0.0402868, 0.939992, 0.99831, 86.3807, -88.1671, 0.191875, -66.161, 91.5975, -10.0204, 46.8196, -10.0945, -10.281, 88.2485, -66.8913, 57.3583, -34.4107, 0.973417, 76.4187, -0.383973, 76.1867, -99.7596, 75.5632, -41.3579, 45.3754, 59.4767, -52.3964, 64.0692, 77.6899, -8.29805, -21.0307, 38.899, -0.00562926, 50.718, -64.0306, 0.428358, -24.6193, 95.8691, -7.86242, 0.886855, 72.2335, -99.0591, -31.9659]
    end
    global CallsItm = filter(x -> getStrike(x.oq) < x.curp, AllCalls)
    global CallsItmXs, CallsItmExtrins = makeData(CallsItm)
    data, extrin = runData()
    global BbCallsItm = bboptimize(modelOptim(data, extrin), PandaInit[]; SearchRange=searchRange(), MaxTime=1000) #, NThreads=(Threads.nthreads()-1))
    PandaInit[] = best_candidate(BbCallsItm)
    # drawStrice()
    drawTexPrice()
end

#region Calc
function modelOptim(data, prices)
    function(p)
        return sum((model(data[i], p) - prices[i])^2 for i in eachindex(data))
    end
end

termCount() = 4
termWidth() = 68
searchRange() = vcat(
    fill((0.0, 1.0), 5), # tex
    # fill((0.0, 1.0), 3), # tex varia
    # fill((-100.0, 100.0), 9), # vty
    # fill((-200.0, 200.0), 3), # strikeRat
    fill((-100.0, 100.0), termCount() * termWidth()))
initDefault() = [(x[1] + x[2])/2 for x in searchRange()]

sqrt0(x) = sqrt(x + 1.0) - 1.0
log0(x) = log(x + 1.0)
# varia(x, p, off) = (p[off] * x, p[off+1] * sqrt0(x), p[off+2] * log0(x), p[off+3] * x^2)
# variaSum(x, p, off) = sum(varia(x, p, off))
varia(x) = [x, sqrt0(x), log0(x), x^2]
variaLabeled(v::Vector) = (; plain=v, sqrt=sqrt0.(v), log=log0.(v), squared=(^).(v, 2))
# variaSum(x, p, off) = sum(varia(x, p, off))
combsMult(x1, x2) = map(Iterators.product(1:4, 1:4)) do (i1, i2)
    x1[i1] * x2[i2]
end
function linsum(xs, p, off)
    sum = 0.0
    for i in eachindex(xs)
        sum += p[off+i-1] * xs[i]
    end
    return sum
end

function calcVty(x)
    curpRats = varia(x[8])
    openCloseRats = varia(x[9])
    vixOpens = varia(x[10])
    # s1 = linsum(curpRats, p, off) + linsum(openCloseRats, p, off+4) + linsum(vixOpens, p, off+8)
    # off += 12
    # s1 = linsum(curpRats, p, off) + linsum(openCloseRats, p, off+4) + linsum(vixOpens, p, off+8)
    # off += 12
    # s1 = linsum(curpRats, p, off) + linsum(openCloseRats, p, off+4) + linsum(vixOpens, p, off+8)
    # return (s1, s2, s3)
    return (curpRats..., openCloseRats..., vixOpens...)
end

#==
best cand: [0.00921976, 0.00435724, 0.00239191, 0.482184, 0.914635, -12.8375, 37.2243, -38.0637, 38.6945, 74.1122, 1.68297, -40.3226, -14.5234, 59.6427, -22.545, -57.9234, 10.5258, 28.4039, -95.1936, 67.8637, -30.29, 43.7353, 79.1174, 99.7856, 93.8175, 99.8825, 97.6112, 87.6959, -51.2668, 15.1893, -75.4993, 71.1365, -57.7175, 96.676, 94.9151, 93.5149, -93.4152, 40.4689, -98.8123, -93.4776, -38.8415, 70.5029, -14.3418, 84.5947, 66.3484, 66.5598, -46.2596, 84.5947, -79.005, 81.6592, 83.1006, -65.7612, -41.1922, 98.8924, 99.5899, 96.3218, -99.3085, 46.8482, -39.8935, 95.4578, -77.0793, -60.8053, 20.9299, -48.93, 66.4948, -80.7874, 87.9199, 29.8382, 87.5484, 5.83358, -43.8499, -64.1592, -47.1003, -96.9757, 99.6001, -52.2144, -99.7726, -90.2827, -90.9674, -33.9554, -55.702, 98.2328, 99.6728, 45.323, -95.6726, 89.0032, 99.516, 96.0963, -7.63129, -4.34654, 3.846, 99.8401, 57.9693, 90.6497, 89.4638, 55.8656, -93.5472, -12.3349, 46.3337, -11.5583, -65.6247, 89.4024, -3.39939, 2.42268, -61.0666, -74.9541, -37.1324, 37.9668, -85.8667, 95.6166, 1.62907, -78.6364, -67.6294, -63.4177, 19.6433, -38.7549, -95.3789, 82.8487, -31.7282, 91.8465, -61.9677, 31.535, -66.9818, 89.9618, 99.2348, -83.4828, 43.2768, 83.1164, 7.67389, 65.0177, -88.6302, -3.9239, 7.80914, 57.164, -43.2468, -46.0448, 33.5491, 44.7674, 19.9828, 11.4226, -87.0078, 8.59806, 10.0126, 95.8576, -1.21955, -28.1962, 43.2326, 64.2377, 30.4275, -87.8491, -21.8364, -95.8911, 9.82321, 98.8913, 76.7175, -44.7611, -5.02634, -64.8046, -63.7838, 50.8569, 24.1304, -99.6172, 12.6352, -42.0478, -69.5862, -98.2893, 88.5125, 77.5424, 81.9433, -99.7899, 28.2253, -76.796, 97.8479, 33.5881, -47.4067, 35.7256, -50.5448, 17.8472, -27.5503, -32.7301, -50.0634, -79.41, 89.9369, -40.9844, 73.0597, -84.2177, 85.7286, -28.5072, 14.5678, 90.547, -88.2986, -35.2485, 11.6331, -12.3747, 60.2185, -5.29847, -39.3342, 76.7083, -80.534, -66.1175, 51.6509, 80.6792, -18.2777, -25.6675, -98.0968, -42.07, -82.5995, -60.6574, -7.9192, -19.3081,
55.8575, 27.579, 34.4534, 98.1077, -88.8991, -15.7378, -19.9528, 46.6621, -39.3551, 47.7239, 30.9594, 10.9623, 99.5501, -72.3134, -30.2944, -67.5725, -98.1891, -73.3785, -91.6103, -6.02446, -98.3693, 30.3366, 68.9372, 36.6419, 88.119, -59.5043, 3.36252, 96.963, 74.3893, 82.5349, 1.65589, 82.9069, 71.9079, 74.8419, -3.49327, -20.2329, -10.7008, -96.7778, -52.9377, 97.484, 42.8982, -6.28671, 62.0757, 86.9025, 94.6915, -34.2176, 7.8385, -99.6791, -65.6676, -65.2382, -63.3296, 73.0552, -53.1026, -14.9195, 82.5152, -55.526, 90.1752, 97.2716, -2.97314, -92.6493, -44.301, -14.4927, 2.79972, -83.2961, -33.6207, 98.9808, -94.0495]
==#

# return [tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, tex.weekend.value, tex.holiday.value, strikeRat, curpRat, openCloseRat, row.vixOpen]
function model(x, p)::Float64
    tex = calcTex(x, p, 1)
    off = 6
    texs = varia(tex)

    vtys = calcVty(x)
    strikeRats = varia(x[7])

    xs1 = [texs..., vtys..., strikeRats...]
    xs2 = combsMult(texs, vtys)
    xs3 = combsMult(texs, strikeRats)
    xs4 = combsMult(strikeRats, vtys)
    all = collect(Iterators.flatten((xs1, xs2, xs3, xs4)))
    # all = xs1
    n = length(all)
    x1 = linsum(all, p, off)
    off += n
    x2 = linsum(all, p, off)
    off += n
    x3 = linsum(all, p, off)
    off += n
    x4 = linsum(all, p, off)
    off += n
    res = fit1(x1, x2, x3, x4)
    # @info "res" off res

    # x1 = tex1 + tex2 + tex3
    # off += 3
    # x2 = p[off] * vty1 + p[off+1] * vty2 + p[off+2] * vty3
    # off += 3
    # x3 = p[off] * strikeRat1 + p[off+1] * strikeRat2 + p[off+2] * strikeRat3
    # off += 3

    # res = 0.0
    # for _ in 1:termCount()

    #     ll = other(x, p[off], p[off+1], p[off+2])
    #     off += 3
    #     res += p[off] * ll
    #     # @info "res" res ll off p[off] x x0 mx k
    #     off += 1
    # end

    # rt = tex2
    # d1 = (log(strikeRat) + (vty * vty / 2.0) * tex) / (vty * rt)
    # d2 = d1 - vty * rt
    # r = under * cdf(NORM, d1) - strike * cdf(NORM, d2)

    return res < 0.0 ? 100000*res : res
end
#endregion

#region MakeData
function makeData(data)
    return (makeX.(data), makeExtrinsic.(data))
end

function makeX(row)::Vector{Float64}
    # (;ts, curp, spyOpen, spyClosePrev, vixOpen, oq)
    strike = getStrike(row.oq)
    # TODO: strikeRat is upside down
    strikeRat =  strike / row.curp - 1.0
    curpRat = row.curp / row.spyOpen - 1.0
    openCloseRat = row.spyOpen / row.spyClosePrev - 1.0
    tex = Calendars.calcDurToExpr(row.ts, getExpiration(row.oq))
    return [tex.closed.value, tex.pre.value, tex.open.value, tex.post.value, tex.weekend.value, tex.holiday.value, strikeRat, curpRat, openCloseRat, row.vixOpen]
end
makeExtrinsic(row) = getExtrinsic(row.oq, C(row[2]))[3]
#endregion

end