module ExplorePricing
using Dates, BlackBoxOptim, EnergyStatistics, Combinatorics
using SH, BaseTypes, SmallTypes, ChainTypes, OptionTypes, QuoteTypes, OptionMetaTypes
using DateUtil, FileUtil, OptionUtil, PricingUtil, DrawUtil, CollUtil, VectorCalcUtil
using Calendars

locDraw = drawDots
locDraw! = drawDots!

# TODO: add searches for varias
# a function and a number of parameters
# it then loops through and finds which best correlate in each param slot

NumParams = 10

modelFunc(xs) =
    fit2(xs[1], xs[2], xs[3], xs[4]) + fit1(xs[5], xs[6]) + fit1(xs[7], xs[8]) + xs[9] + xs[10]

modelAll(xss::Vector{Vector{Float64}}, p::Vector{Float64}) = modelAll(xss .* p)
modelAll(xss::Vector{Vector{Float64}}) =
    [modelFunc([xss[c][i] for c in eachindex(xss)]) for i in eachindex(xss[1])]

function modelInd(ind::Int, xs::Vector{Float64}, def::Vector{Float64})::Vector{Float64}
    xss = fill(def, NumParams)
    xss[ind] = xs
    return modelAll(xss)
end

fitnessInd(f, ind::Int, xs::Vector{Float64}, ys::Vector{Float64}, def::Vector{Float64}) =
    (p::Vector{Float64}) -> sum( f(ind, xs .* p, def) .- ys .^2 )
fitness(f, xss::Vector{Float64}, ys::Vector{Float64}) =
    (p::Vector{Float64}) -> sum( f(xss, p) .- ys .^2 )

function checkCorr()
    global ys = vcat(map(x -> x.ys, values(Store))...)
    # global ysCorand = cor(rand(length(ys)), ys)
    # global ysCoronst = cor(fill(1.0, length(ys)), ys)
    global ysWork = copy(ys)
    xsnt = vcat(map(x -> x.xs, values(Store))...)
    global ntxs = ntvFromVnt(xsnt)

    global useKeys = []
    global keyVals = Vector{Vector{Float64}}()
    global locPanda = Vector{Float64}()
    global locSearch = Vector{NTuple{2,Float64}}()
    global corrAll = Dict()

    global valsAll = Dict()
    foreach(k -> valsAll[k] = ntxs[k], keys(ntxs))
    foreach(((k1, k2),) -> valsAll[(k1, k2)] = ntxs[k1] .* ntxs[k2], combinations(keys(ntxs), 2))
    foreach(((k1, k2, k3),) -> valsAll[(k1, k2, k3)] = ntxs[k1] .* ntxs[k2] .* ntxs[k3], combinations(keys(ntxs), 3))
    # TODO: add varia

    V1 = fill(1.0, length(ys))

    for i in 1:NumParams
        foreach(((k, v),) -> corrAll[k] = k in useKeys ? 0.0 : cor(modelInd(i, v, V1), ysWork), valsAll)
        maxCor, maxKey = findmax(c -> abs(c), corrAll)
        @info "Term $(i)" maxKey maxCor

        vals = valsAll[maxKey]
        push!(useKeys, maxKey)
        push!(keyVals, vals)

        global bboptInner = bboptimize(fitnessInd(modelInd, i, vals, ysWork, V1), [1.0]; SearchRange=[(-100.0, 100.0)], MaxTime=2) # , Method)
        append!(locPanda, best_candidate(bboptInner))

        # score = fitness(modelLinSum, keyVals, ys)(locPanda)
        global guess = modelInd(i, keyVals, V1)
        ysWork = ys .- guess
    end

    global bbopt = bboptimize(fitness(modelAll, keyVals, ys), locPanda; SearchRange=fill((-100.0, 100.0), NumParams), MaxTime=10) # , Method)
    copy!(locPanda, best_candidate(bboptInner))

    display(locDraw(ys; color=:white))
    global guess = runModel(modelLinSum, keyVals, locPanda)
    locDraw!(guess; color=:yellow)
    return
end


function checkCorrOld()
    global ys = vcat(map(x -> x.ys, values(Store))...)
    global ysCorand = cor(rand(length(ys)), ys)
    global ysCoronst = cor(fill(1.0, length(ys)), ys)
    # display(locDraw(ys; color=:white))
    # global fitXs = zeros(length(ys))
    global ysWork = copy(ys) # vcat(map(x -> x.ys[2:end], values(Store))...)
    xsnt = vcat(map(x -> x.xs, values(Store))...)
    global ntxs = ntvFromVnt(xsnt)

    global useKeys = []
    global keyVals = Vector{Vector{Float64}}()
    global locPanda = Vector{Float64}()
    global locSearch = Vector{NTuple{2,Float64}}()
    global valsAll = Dict()
    global corrAll = Dict()

    foreach(k -> valsAll[k] = ntxs[k], keys(ntxs))
    foreach(((k1, k2),) -> valsAll[(k1, k2)] = ntxs[k1] .* ntxs[k2], combinations(keys(ntxs), 2))
    foreach(((k1, k2, k3),) -> valsAll[(k1, k2, k3)] = ntxs[k1] .* ntxs[k2] .* ntxs[k3], combinations(keys(ntxs), 3))

    cnt = 1
    while true
        foreach(((k, v),) -> corrAll[k] = k in useKeys ? 0.0 : cor(v, ysWork), valsAll)
        maxCor, maxKey = findmax(c -> abs(c), corrAll)
        vals = valsAll[maxKey]
        push!(useKeys, maxKey)
        push!(keyVals, vals)

        paramEst = clamp(-100.0, sign(maxCor) * sum(abs, ysWork) / sum(abs, vals), 100.0)
        push!(locPanda, paramEst)
        # push!(locSearch, (min(100.0*sign(useCorr.dc), 0.0), max(100.0*sign(useCorr.dc), 0.0)))
        # push!(locSearch, (min(100.0*sign(useCorr.dc), 0.0), max(100.0*sign(useCorr.dc), 0.0)))
        push!(locSearch, (-100.0, 100.0))

        global bbopt = bboptimize(fitness(modelLinSum, keyVals, ys), locPanda; SearchRange=locSearch, MaxTime=2) # , Method)

        copy!(locPanda, best_candidate(bbopt))
        # off = .5 * abs(locPanda[end])
        # locSearch[end] = (locPanda[end] - off, locPanda[end] + off)

        score = fitness(modelLinSum, keyVals, ys)(locPanda)
        @info "Term $(length(keyVals))" maxKey maxCor score
        global guess = runModel(modelLinSum, keyVals, locPanda)
        ysWork = ys .- guess

        (cnt < 8) || break
        cnt += 1
    end

    display(locDraw(ys; color=:white))
    global guess = runModel(modelLinSum, keyVals, locPanda)
    locDraw!(guess; color=:yellow)
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
    tex = calcTex(ts, getExpiration(oq))
    strike = getStrike(oq)
    strikeRat =  strike / curp - 1.0
    vty1 = PricingUtil.calcVty2(Date(ts))
    vty2 = PricingUtil.calcVty3(Date(ts))
    vty3 = PricingUtil.calcVtyNeg(Date(ts))
    curpRat = curp / spyOpen - 1.0
    openCloseRat = spyOpen / spyClosePrev - 1.0
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
    dists = 1.0:1.0:20.0
    data = vec([(;exp, dist) for exp in exps, dist in dists])
    num = min(length(data), numSpecs)
    specs = sample(data, num; replace=false)
    return specs
end
#endregion

#region Model
fit1(a, b) = a / b

using Distributions
const NORM = Normal()
function fit2(t, pos, vty, rfr)
    t = abs(t)
    t < 1.0 || (t = 1.0 + log(abs(t)))
    abs(rfr) < 1.0 || (rfr = sign(rfr) + sign(rfr) * log(abs(rfr)))
    rt = sqrt(abs(t))
    d1 = (log(abs(pos)) + (rfr + vty^2 / 2.0) * t) / (vty * rt)
    d2 = d1 - vty * rt
    r = cdf(NORM, d1) - exp(-rfr * t) * pos * cdf(NORM, d2)
    if !isfinite(r)
        @error t pos vty rfr rt d1 d2 cdf(NORM, d1) (-rfr * t) exp(-rfr * t) cdf(NORM, d2)
        error("stop")
    end
    return r
end

modelLinSum(x::Vector{Vector{Float64}}, p, i::Int)::Float64 = sum([x[term][i] * p[term] for term in eachindex(p)])
runModel(f, xs::Vector{Vector{Float64}}, p) = [f(xs, p, i) for i in eachindex(xs[1])]
diffFit(x1, x2) = (x1 - x2)^2
# diffFit(x1, x2) = (abs(x2) > abs(x1) ? 100.0 : 1.0) * (x1 - x2)^2
fitness(f, xs::Vector{Vector{Float64}}, ys::Vector{Float64}) =
    (p::Vector{Float64}) -> sum(diffFit(ys[i], f(xs, p, i)) for i in eachindex(ys))
#endregion

end