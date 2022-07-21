module ExplorePricing
using Dates, BlackBoxOptim, Combinatorics
using SH, BaseTypes, SmallTypes, ChainTypes, OptionTypes, QuoteTypes, OptionMetaTypes
using DateUtil, FileUtil, OptionUtil, PricingUtil, DrawUtil, CollUtil, VectorCalcUtil, MathUtil
using Calendars

locDraw = drawDots
locDraw! = drawDots!

# TODO: add searches for varias
# a function and a number of parameters
# it then loops through and finds which best correlate in each param slot

# vcat(fill(fill(1.0, length(ys)), NumParams-2), fill(fill(0.0, length(ys)), 2))
DEF_FIT1 = [1.0, 1.0]
DEF_FIT2 = [1.0, 1.0, 0.2, 1.0, 1.0]
DEF_XS = vcat(DEF_FIT1, DEF_FIT2, 0.0, 0.0, 0.0)
NumParams = length(DEF_XS)
SearchRange = fill((0.0, 2.0), NumParams)

function setConst()
    global DEF_XS = vcat(DEF_FIT1, DEF_FIT2, 0.0, 0.0, 0.0)
    global NumParams = length(DEF_XS)
end
function __init__()
    setConst()
end

# modelFunc(xs) =
#     fit2(xs[1], xs[2], xs[3], xs[4]) + fit1(xs[5], xs[6]) + fit1(xs[7], xs[8]) + xs[9] + xs[10]
modelFunc(xs) =
    errNum(fit2(xs[1], xs[2], xs[3], xs[4], xs[5]) + fit1(xs[6], xs[7]) + xs[9] + xs[10])

# modelAll(xss::Vector{Vector{Float64}}, p::Vector{Float64}) = modelAll(xss .* p)
# modelAll(xss::Vector{Vector{Float64}}) =
#     [modelFunc([xss[c][i] for c in eachindex(xss)]) for i in eachindex(xss[1])]
modelAll(xss::Vector{Vector{Float64}}, p::Vector{Float64}) =
    [modelFunc([xss[c][i] * p[c] for c in eachindex(xss)]) for i in eachindex(xss[1])]

# function modelInd(ind::Int, xs::Vector{Float64}, def::Vector{Vector{Float64}})::Vector{Float64}
#     xss = copy(def)
#     xss[ind] = xs
#     return modelAll(xss)
# end

modelCorr(ind::Int, xs::Vector{Float64}, defp::Vector{Float64}) =
    map(modelFunc, inditr(ind, xs, defp))

repat(v, i, x) = ( c = copy(v) ; c[i] = x ; c )
inditr(ind, xs, p) = (repat(p, ind, xs[i]) for i in eachindex(xs))
inditrm(ind, xs, p) = (repat(p, ind, xs[i] * p[ind]) for i in eachindex(xs))
modelOpt(ind::Int, xs::Vector{Float64}, p::Vector{Float64}) = map(modelFunc, inditrm(ind, xs, p))
# function modelOpt(ind::Int, xs::Vector{Float64}, p::Vector{Float64})::Vector{Float64}
#     [modelFunc(repat(p, ind, xs[i] * p[ind])) for i in eachindex(xs)]
# end

# fitnessInd(f, ind::Int, xs::Vector{Float64}, ys::Vector{Float64}, def::Vector{Vector{Float64}}) =
#     (p::Vector{Float64}) -> sum( (f(ind, xs .* p, def) .- ys) .^2 )
fitnessInd(f, ind::Int, xs::Vector{Float64}, ys::Vector{Float64}) =
    (p::Vector{Float64}) -> sum( (f(ind, xs, p) .- ys) .^2 )
fitness(f, xss::Vector{Vector{Float64}}, ys::Vector{Float64}) =
    (p::Vector{Float64}) -> sum( (f(xss, p) .- ys) .^2 )

function checkCorr()
    global ys = vcat(map(x -> x.ys, values(Store))...)
    # global V1 = map(x -> fill(x, length(ys)), DEF_XS)
    # global ysCorand = cor(rand(length(ys)), ys)
    # global ysCoronst = cor(fill(1.0, length(ys)), ys)
    global ysWork = copy(ys)
    xsnt = vcat(map(x -> x.xs, values(Store))...)
    global ntxs = ntvFromVnt(xsnt)

    global valsAll = Dict()
    foreach(k -> valsAll[k] = ntxs[k], keys(ntxs))
    foreach(((k1, k2),) -> valsAll[(k1, k2)] = ntxs[k1] .* ntxs[k2], combinations(keys(ntxs), 2))
    foreach(((k1, k2, k3),) -> valsAll[(k1, k2, k3)] = ntxs[k1] .* ntxs[k2] .* ntxs[k3], combinations(keys(ntxs), 3))
    # TODO: add varia

    global useKeys = Vector{Any}(nothing, NumParams)
    global keyVals = Vector{Vector{Float64}}(undef, NumParams)
    global locPanda = copy(DEF_XS) # Vector{Float64}(undef, NumParams)

    valsAllWork = copy(valsAll)
    for _ in 1:NumParams
        mx = (; i=0, c=0.0, key=nothing)
        for i in findall(isnothing, useKeys)
            corrAll = Dict()
            foreach(((k, v),) -> corrAll[k] = errNum(cor(modelCorr(i, v, DEF_XS), ysWork)), valsAllWork)
            c, key = findmax(c -> abs(c), corrAll)
            # TODO: consider using >= to get last greatest because we may have lots of repeats
            if c >= mx.c
                mx = (; i, c, key)
            end
            # @info "Iterating to find best corr" i mx
        end
        println("Term ", mx)
        ind = mx.i
        delete!(valsAllWork, mx.key)

        useKeys[ind] = mx.key
        vals = valsAll[mx.key]
        keyVals[ind] = vals

        global bboptInner = bboptimize(fitnessInd(modelOpt, ind, vals, ysWork), locPanda; SearchRange, MaxTime=5)
        copy!(locPanda, best_candidate(bboptInner))
        # locPanda[ind] = best_candidate(bboptInner)[1]
        # append!(locPanda, best_candidate(bboptInner))

        # score = fitness(modelLinSum, keyVals, ys)(locPanda)
        global guess = modelOpt(ind, vals, locPanda)
        ysWork = ysWork .- guess
    end
    @assert isnothing(findfirst(isnothing, useKeys))

    global bbopt = bboptimize(fitness(modelAll, keyVals, ys), locPanda; SearchRange, MaxTime=10)
    copy!(locPanda, best_candidate(bbopt))

    display(locDraw(ys; color=:white))
    global guess = modelAll(keyVals, locPanda)
    locDraw!(guess; color=:yellow)

    someCurp = first(Store)[2][1][1].curp
    println("Max error: ", someCurp * maximum(guess .- ys))
    return
end

#region CalcCorr
function calcCorr(f, ntxs, ys)
    ks = keys(ntxs)
    vals = map(k -> cor(f.(ntxs[k]), ys), ks)
    return NamedTuple{ks}(vals)
end

function calcCorrMults(f, ntxs, ys)
    res = Dict()
    foreach(combinations(keys(ntxs), 2)) do (k1, k2)
        vals = f.(ntxs[k1] .* ntxs[k2])
        c = cor(vals, ys)
        if !isfinite(c)
            global checkVals = vals
            @info "infinite c" c
            return
        end
        res[(k1,k2)] = c
    end
    return res
end

# findFuncCorr(m.fit2, 4, m.valsAll, m.ys)
function findFuncCorr(func, pcnt, valsAll, ys)
    use = []
    for pind in 1:pcnt
        locCorrAll = Dict()
        locValsAll = Dict()
        # TODO: reuse vector faster?
        f = x -> func(ntuple(i -> i == pind ? x : 1.0, pcnt)...)

        foreach(((k, v),) -> locValsAll[k] = map(f, v), valsAll)
        foreach(((k, v),) -> locCorrAll[k] = cor(v, ys), locValsAll)

        maxCor, maxKey = findmax(c -> abs(c), locCorrAll)
        push!(use, (;key=maxKey, corr=maxCor, vals=locValsAll[maxKey]))
    end
    return use
end
#endregion

#region Data
function makeData1(nt::NamedTuple)
    (;ts, curp, oq, spyOpen, spyClosePrev) = nt
    tex = texToYear(calcTex(ts, getExpiration(oq)))
    strike = getStrike(oq)
    strikeRat = strike / curp - 1.0
    vty1 = PricingUtil.calcVty2(Date(ts))
    vty2 = PricingUtil.calcVty3(Date(ts))
    vty3 = PricingUtil.calcVtyNeg(Date(ts))
    curpRat = curp / spyOpen - 1.0
    openCloseRat = spyOpen / spyClosePrev - 1.0
    checkRange(-10.0, 10.0, (; tex, strikeRat, vty1, vty2, vty3, curpRat, openCloseRat))
    return (;tex, strikeRat, vty1, vty2, vty3, curpRat, openCloseRat)
end
DATA1_ZERO = (;tex=0.0, strikeRat=0.01, vty1=0.01, vty2=0.01, vty3=0.01, curpRat=0.01, openCloseRat=0.01)

function prepDataAll(numSpecs=20)
    !isempty(PricingUtil.AllCalls) || readPricing()
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
    global curps = Vector{Float64}()
    global ys = Vector{Float64}()
    global actual = Vector{Tuple{Float64,Float64}}()
    global actualTime = Vector{Tuple{DateTime,Float64}}()
    # push!(ys, 0.0)
    # push!(xs, DATA1_ZERO)
    # push!(curps, 1.0)
    # push!(actual, (0.0, 0.0))
    # push!(actualTime, (getMarketClose(exp), 0.0))
    procCalls() do nt
        (;ts, curp, oq) = nt
        strike = getStrike(oq)
        getExpiration(oq) == exp && dist - 0.5 < strike - curp < dist + 0.5 || return
        datum = makeData1(nt)
        extrin3 = calcExtrins(oq, curp)
        y = extrin3[3] / curp
        push!(ys, y) # 3 = imp
        push!(xs, datum)
        push!(curps, nt.curp)
        push!(actual, (datum.tex, extrin3[3]))
        push!(actualTime, (ts, extrin3[3]))
        push!(orig, merge(nt, (;tex=datum.tex)))
        # push!(texes, datum.tex)
    end

    # model1(xs[1], zeros(100))
    # println(length(xs), length(actual), length(ys))
    return (; orig, xs, ys, actual, actualTime)
end

Store = Dict()
function makeSpecs(numSpecs::Int)::Vector
    allexps = unique!(map(x -> getExpiration(x.oq), PricingUtil.AllCalls))
    exps = filter(x -> Date(2022, 5, 1) <= x <= Date(2022, 6, 10), allexps)
    dists = 1.0:1.0:10.0
    data = vec([(;exp, dist) for exp in exps, dist in dists])
    num = min(length(data), numSpecs)
    specs = sample(data, num; replace=false)
    return specs
end
#endregion

#region Model
errNum(x) = isfinite(x) ? x : error("Invalid num $(x)")
fit1(a, b) = errNum(a / b)

using Distributions
const NORM = Normal()
function fit2(t, strikeRat, vty, under, strike)
    t = keepPos(t)
    # TODO
    # t < 10 || error("invalid input to fit2 ", t)
    # strikeRat = sigmoid(.5, 1.5, 10.0)(strikeRat)
    strikeRat = sigmoid(0.0, 2.0, .5, 1.0)(strikeRat)
    vty = keepPos(vty)
    under = keepPos(under)
    strike = keepPos(strike)
    rt = sqrt(t)
    d1 = (log(strikeRat) + (vty * vty / 2.0) * t) / (vty * rt)
    d2 = d1 - vty * rt
    r = under * cdf(NORM, d1) - strike * cdf(NORM, d2)
    # @info "" under strike d1 d2
    if !isfinite(r)
        @error "Invalid value in fit2" t strikeRat vty rt d1 d2 cdf(NORM, d1) cdf(NORM, d2)
        error("stop")
    end
    return r
end

# modelLinSum(x::Vector{Vector{Float64}}, p, i::Int)::Float64 = sum([x[term][i] * p[term] for term in eachindex(p)])
# runModel(f, xs::Vector{Vector{Float64}}, p) = [f(xs, p, i) for i in eachindex(xs[1])]
# diffFit(x1, x2) = (x1 - x2)^2
# # diffFit(x1, x2) = (abs(x2) > abs(x1) ? 100.0 : 1.0) * (x1 - x2)^2
# fitness(f, xs::Vector{Vector{Float64}}, ys::Vector{Float64}) =
#     (p::Vector{Float64}) -> sum(diffFit(ys[i], f(xs, p, i)) for i in eachindex(ys))
#endregion

TestNumParams = 5
import BlacksPricing
function blackPricing(ts::DateTime, curp::Float64, opt, p::Vector{Float64})
    date = toDateMarket(ts)
    price = BlacksPricing.priceOption(Style.call, Float64(getStrike(opt)),
                p[1] * texToYear(calcTex(ts, getExpiration(opt))),
                PricingUtil.calcVty(date, curp, p[2:5]),
                curp)
    return price
end

function testDiff2(oq, x)
    b = getBid(oq)
    a = getAsk(oq)
    return x < b ? (x - b) :
        x > a ? (x - a) : 0.0
end

function testDiff(oq, x)
    getBid(oq) >= 0.0 || error("invalid bid")
    return x - Float64((getBid(oq) + getAsk(oq)) / 2)
end

testDiffs(data, p) = (testDiff(nt.oq, blackPricing(nt..., p))^2 for nt in data)

testFit(data) = p -> sum(testDiffs(data, p))
#     return function(p)
#         sum(data) do nt
#             x = blackPricing(nt..., p)
#             return testDiff(nt.oq, x)^2
#         end
#     end
# end

function showHighestError(data, p)
    res = map(data) do nt
        date = toDateMarket(nt.ts)
        c = blackPricing(nt..., p)
        vty = PricingUtil.calcVty(date, nt.curp, p[2:5])
        (; score=testDiff(nt.oq, c), calc=c, vty, nt)
    end
    sort!(res; rev=true, by=nt -> nt.score)
    return res[1:10]
end

TestDataAll = Vector{NamedTuple}()
TestData = Vector{NamedTuple}()
function testPrep(num=1000)
    global TestDataAll = Vector{NamedTuple}()
    procCalls() do nt
        # (;ts, curp, oq) = nt
        Date(2022, 5, 1) <= getExpiration(nt.oq) <= Date(2022, 6, 10) || return
        abs(nt.curp - getStrike(nt.oq)) < 11.0 || return
        push!(TestDataAll, (;nt.ts, nt.curp, nt.oq))
    end
    # println(length(TestData))
    global TestData = sample(TestDataAll, num; replace=false)
end

function test()
    !isempty(TestData) || testPrep()
    global testSearchRange = fill((0.0, 2.0), TestNumParams)
    global testPanda = fill(0.5, TestNumParams)
    global testBbopt = bboptimize(testFit(TestData), testPanda; SearchRange=testSearchRange, MaxTime=30)
    copy!(testPanda, best_candidate(testBbopt))
    @info "test result" maximum(testDiffs(TestData, testPanda)) testPanda
end

end