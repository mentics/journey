module ProbKde
using Dates, KernelDensity
using BaseTypes, Bins, ProbTypes, DateUtil
using Globals, LogUtil, CollUtil, VectorCalcUtil, FileUtil, DictUtil
using Caches, HistData, Markets, Calendars

# TODO: consider adding tex=0, x=x data to force endpoint data

# TODO: is it filtering for only expiration days for the "to" days when calculating and testing? should it?

export probKde, pk
const pk = @__MODULE__

function testProb(bdaysTarget=10, bdaysBack=20; weightBy=x->1)
    forDate = today()
    spy = toDict(getDate, dataDaily(forDate, "SPY"))
    vix = toDict(getDate, dataDaily(forDate, "VIX"))
    @assert length(spy) == length(vix)
    dateFrom = bdaysAfter(minimum(x for x in keys(spy)), bdaysBack + 1)
    dateTo = bdaysBefore(maximum(x for x in keys(spy)), bdaysTarget + 1)
    dates = sort!(collect(filter(x -> dateFrom <= x <= dateTo, keys(spy))))
    scoreNormal = 0.0
    scoreNormalMax = 0.0
    scoreNormalRatio = 0.0
    scoreComp = 0.0
    scoreCompMax = 0.0
    scoreCompRatio = 0.0
    count = 0
    for date in dates
        dateTarget = bdaysAfter(date, bdaysTarget)
        spyOpen = F(spy[date].open)
        vixOpen = F(vix[date].open)
        probNormal = probOpenToClose(spyOpen, vixOpen, date, dateTarget)
        probComp, probs = kdeOpenToClose(spyOpen, vixOpen, date, dateTarget, bdaysBack; weightBy)
        @assert probNormal.center == probs[end].center
        @assert probNormal.vals ≈ probs[end].vals

        actual = spy[dateTarget].close / spyOpen
        bin = Bins.nearest(actual)

        normal = probNormal.vals[bin]
        scoreNormal += normal
        normalMax = maximum(probNormal.vals)
        scoreNormalMax += normalMax
        scoreNormalRatio += normal / normalMax

        comp = probComp.vals[bin]
        scoreComp += comp
        compMax = maximum(probComp.vals)
        scoreCompMax += compMax
        scoreCompRatio += comp / compMax

        # TODO: Many comprob's in early times are showing almost all in left-most bin. maybe something wrong because probNormal and extra are not.
        # if probComp.vals[1] == maximum(probComp.vals)
        sv = sort(probComp.vals; rev=true)
        if sv[1] > sv[2]*10
            # println("WARN: left became max at ", date)
            println("WARN: max became mult of second at ", date)
            showProbs(probComp, probs, spy[dateTarget].close)
            global check = (;probComp, probs)
            break
        end
        # sleep(2.0)
        count += 1
    end
    @show (scoreNormal, scoreNormalMax, scoreNormalRatio) ./ count (scoreComp, scoreCompMax, scoreCompRatio) ./ count
    return
end

function showProbs(probComp, probs, actual=nothing)
    fig, ax = du.start()
    drawProbs((probs..., probComp); labels=Iterators.flatten((string.(eachindex(probs[end-1:-1:1])), ("norm", "comp"))))
    isnothing(actual) || GLMakie.vlines!(actual; label="actual")
    GLMakie.axislegend(ax)
end
using DrawUtil
import GLMakie
# function compareProbs(probs)
#     # display(du.drawProb(probs[1].center, probs[1].vals; label="1"))
#     fig, ax = du.start()
#     drawProbs(probs)
#     GLMakie.axislegend(ax)
# end
function drawProbs(probs; labels=string.(eachindex(probs)))
    for (prob, label) in zip(probs, labels)
        # println("drawing ", label)
        du.drawProb!(prob.center, prob.vals; label)
    end
end

function probKde(center::Float64, var::Float64, tex::Float64; up=false)::Prob
    @assert isfinite(center) && center > 0.0
    @assert isfinite(var) && var > 0.0
    @assert isfinite(tex) && tex > 0.0
    (up || Updated[] < now(UTC) - Hour(8)) && update()
    return Prob(Float64(center), makePdv(tex, var))
end
probKde(center::Float64, var::Float64, from::DateTime, to::DateTime; up=false)::Prob = probKde(center, var, calcTex(from, to); up)
probOpenToOpen(center::Float64, var::Float64, from::Date, to::Date; up=false)::Prob = probKde(center, var, getMarketOpen(from), getMarketOpen(to); up)
probOpenToClose(center::Float64, var::Float64, from::Date, to::Date; up=false)::Prob = probKde(center, var, getMarketOpen(from), getMarketClose(to); up)
probToClose(center::Float64, var::Float64, from::DateTime, to::Date; up=false)::Prob = probKde(center, var, from, getMarketClose(to); up)
probFromOpen(center::Float64, var::Float64, from::Date, to::DateTime; up=false)::Prob = probKde(center, var, getMarketOpen(from), to; up)

kdeOpenToClose(center::Float64, var::Float64, from::Date, target::Date, bdaysBack=20; kws...) = probKdeComp(center, var, getMarketOpen(from), getMarketClose(target), bdaysBack; kws...)
# probKdeComp(center::Float64, var::Float64, from::DateTime, target::Date, bdaysBack=20; kws...) = probKdeComp(center, var, from, getMarketClose(target), bdaysBack; kws...)
# Returns combined prob and list of all back probs plus actual prob for from instant
function probKdeComp(center::Float64, var::Float64, from::DateTime, target::DateTime, bdaysBack=20; up=false, weightBy=x->(24*bdaysBack)/(1+x))
    @assert isfinite(center) && center > 0.0
    @assert isfinite(var) && var > 0.0
    fromDate = toDateMarket(from)
    @assert isBusDay(fromDate)
    @assert isBusDay(toDateMarket(target))
    (up || Updated[] < now(UTC) - Hour(8)) && update()

    backDate = bdaysBefore(fromDate, bdaysBack)
    spy = hd.dailyDict(backDate, fromDate, "SPY")
    vix = hd.dailyDict(backDate, fromDate, "VIX")
    dates = sort!(collect(keys(spy)))
    # @info "calcing prob" dates[1] dates[2]
    probsTexes = map(dates) do date
        (;prob=probFromOpen(spy[date].open, vix[date].open, date, target), tex=calcTex(getMarketOpen(date), target))
    end
    tex = calcTex(from, target)
    push!(probsTexes, (;prob=probKde(center, var, from, target), tex))
    probs = (x->x.prob).(probsTexes)
    comprob = pt.combineProbs(center, probs; ws=map(x->weightBy(x.tex - tex), probsTexes))
    @assert isnothing(findfirst(x -> !isfinite(x), comprob.vals)) string(findfirst(x -> !isfinite(x), comprob.vals), ' ', first(comprob.vals, 10))
    # compareProbs((probs..., comprob))
    return (comprob, probs)
end

function comprob(from::Date, to::Date)
    spy = hd.dailyDict(from, to, "SPY")
    vix = hd.dailyDict(from, to, "VIX")
    dates = sort!(collect(keys(spy)))
    datesVix = sort!(collect(keys(spy)))
    @assert dates == datesVix
    # @info "calcing prob" dates[1] dates[2]
    probs = Prob[]
    push!(probs, probOpenToOpen(spy[dates[1]].open, vix[dates[1]].open, dates[1], dates[end]))
    for i in 3:length(dates) # eachindex(dates)
        date1 = dates[i-1]
        date2 = dates[end]
        # probNext = probOpenToOpen(spy[date1].open, vix[date1].open, date1, date2)
        probNext = probOpenToOpen(spy[date1].open, vix[date1].open, date1, date2)
        # prob = pt.combineProbs(prob, probNext)
        push!(probs, probNext)
    end
    prob = pt.combineProbs(probs...)
    return (prob, probs)
end

#region Local
const NumVarBins = 20
const MaxDays = 200
const Updated = Ref{DateTime}(DateTime(0))
const VarBins = Ref(Vector{Float64}(undef, NumVarBins - 1))
const Kdes = Ref(Vector{BivariateKDE}(undef, NumVarBins))
const KdeInterps = Ref(Vector{InterpKDE{<:BivariateKDE}}(undef, NumVarBins))

const BaseDir = dirData("prob")
makePath(sym::Symbol) = joinpath(BaseDir, "kde-$(sym).ser")

# using JSON3
# import Interpolations, OffsetArrays
# # KernelDensity.BivariateKDE{StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}, StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}}
# # JSON3.StructType(::Type{BivariateKDE}) = JSON3.Struct()
# JSON3.StructType(::Type{KernelDensity.BivariateKDE{StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}, StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}}}) = JSON3.Struct()
# JSON3.StructType(::Type{KernelDensity.InterpKDE{KernelDensity.BivariateKDE{StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}, StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}}, Interpolations.FilledExtrapolation{Float64, 2, Interpolations.ScaledInterpolation{Float64, 2, Interpolations.BSplineInterpolation{Float64, 2, OffsetArrays.OffsetMatrix{Float64, Matrix{Float64}}, Interpolations.BSpline{Interpolations.Quadratic{Interpolations.Line{Interpolations.OnGrid}}}, Tuple{Base.OneTo{Int64}, Base.OneTo{Int64}}}, Interpolations.BSpline{Interpolations.Quadratic{Interpolations.Line{Interpolations.OnGrid}}}, Tuple{StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}, StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}}}, Interpolations.BSpline{Interpolations.Quadratic{Interpolations.Line{Interpolations.OnGrid}}}, Float64}}}) = JSON3.Struct()

function update()
    ( path = makePath(:kdes) ; isfile(path) && unix2datetime(mtime(path)) > (now(UTC) - Hour(8)) && (loadData() ; return) )
    println("Calculating ProbKde")
    forDate = market().startDay
    dailySpy = dataDaily(forDate, "SPY")
    dailyVix = dataDaily(forDate, "VIX")
    @assert length(dailySpy) == length(dailyVix)
    @log debug "Calculating historical distributions for" forDate (forDate == today()) dailySpy[1] dailyVix[1]
    rets = calcRets(MaxDays, dailySpy, dailyVix)
    retsNtv = ntvFromVnt(rets)
    calcVarBins(retsNtv)
    calcKdes(retsNtv)
    Updated[] = now(UTC)
    saveData()
    return
end

using Serialization
function saveData()
    serialize(makePath(:varbins), VarBins[])
    serialize(makePath(:kdes), Kdes[])
    serialize(makePath(:kdeinterps), KdeInterps[])
    return
end

function loadData()
    println("Loading ProbKde")
    pathKde = makePath(:kdes)
    VarBins[] = deserialize(makePath(:varbins))
    Kdes[] = deserialize(pathKde)
    KdeInterps[] = deserialize(makePath(:kdeinterps))
    Updated[] = unix2datetime(mtime(pathKde))
    return
end

function calcRets(maxNumDays::Int, daily::DailyType, dailyVar::DailyType)::Vector{NamedTuple}
    res = Vector{NamedTuple}()
    for i in 1:(min(length(daily), length(dailyVar)) - maxNumDays)
        rowTo = daily[i]
        # TODO: what to do about expiration days?
        # if !(dayofweek(rowTo.date) in [1,3,5])
        #     continue
        # end
        for j in 1:maxNumDays
            rowFrom = daily[i + j]
            rowVarFrom = dailyVar[i + j]
            fromOpen = rowFrom.open
            fromClose = rowFrom.close
            toClose = rowTo.close
            # toDateTime = getMarketClose(rowTo.date)
            retOpen = toClose / fromOpen
            retClose = toClose / fromClose
            varOpen = log(rowVarFrom.open)
            varClose = log(rowVarFrom.close)
            texOpen = calcTex(getMarketOpen(rowFrom.date), rowTo.date)
            texClose = calcTex(getMarketClose(rowFrom.date), rowTo.date)
            push!(res, (;tex=texOpen, var=varOpen, ret=retOpen)) # , date=rowFrom.date, dailyIndex=i+j))
            push!(res, (;tex=texClose, var=varClose, ret=retClose)) # , date=rowFrom.date, dailyIndex=i+j))
        end
    end
    return res
end

function calcVarBins(retsNtv::NamedTuple)
    varSorted = sort(retsNtv.var)
    binSize = length(varSorted) ÷ NumVarBins
    bin = 1
    for i in binSize:binSize:(length(varSorted) - binSize)
        VarBins[][bin] = varSorted[i]
        bin += 1
    end
end

function calcKdes(retsNtv::NamedTuple)
    datas = [(Vector{Float64}(), Vector{Float64}()) for _ in 1:NumVarBins]
    var = retsNtv.var
    tex = retsNtv.tex
    ret = retsNtv.ret
    for i in eachindex(var)
        bin = findVarBin(var[i])
        push!(datas[bin][1], tex[i])
        push!(datas[bin][2], ret[i])
    end
    for i in 1:NumVarBins
        Kdes[][i] = calcKde(datas[i])
        KdeInterps[][i] = InterpKDE(Kdes[][i])
    end
end

function findVarBin(val::Float64)
    for i in 1:length(VarBins[])
        if val < VarBins[][i]
            return i
        end
    end
    return NumVarBins
end

function calcKde(data::NTuple{2,Vector{Float64}})::BivariateKDE
    bws = (1.0, Bins.width())
    bounds = (0.0, 1.1 * maximum(data[1])), (0.8, 1.1) .* extrema(data[2])
    println("Calcing kde with length rets ", length(retsNtv.tex), " extrema(data[1])=$(extrema(data[1])), extrema(data[2])=$(extrema(data[2]))")
    println(bounds)
    return KernelDensity.kde(data; bandwidth=bws, boundary=bounds)
end

# using MultiKDE
# const KdeDims = [ContinuousDim(), ContinuousDim(), ContinuousDim()]
# const Kde = Ref{KDEMulti}()
# function calcKde(rets::AVec{NamedTuple})::KDEMulti
#     println("Calcing kde with length rets ", length(rets))
#     observations = map(x -> collect(values(x)), rets)
#     return KDEMulti(KdeDims, [0.1, .1, .01], observations)
# end

function makePdv(tex::Float64, var::Float64)
    varBin = findVarBin(log(var))
    k = Kdes[][varBin]
    ik = KdeInterps[][varBin]
    # texBinWidth = k.x.step
    # TODO: figure out what correct calc is instead of just normalizing
    vals = Bins.with(0.0)

    for (i, x) in Bins.midsi()
        pd = pdf(ik, tex, x)
        # TODO: Why can this be negative? Ask on discourse?
        @assert pd > -1e-4 "pd too negative $(pd) tex=$(tex), var=$(var), varBin=$(varBin), i=$(i), x=$(x), kpdf=$(pdf(k, tex, x))"
        pd >= 0.01 * Bins.binPercent() || (pd = 0.0)
        vals[i] = Bins.width() * pd
    end
    # TODO: messy
    left = 0.0
    right = 0.0
    w = Float64(k.y.step)
    for x in k.y
        pd = pdf(ik, tex, x)
        pd > 0.0 || continue
        Bins.isLeft(x) && (left += w * pd)
        Bins.isRight(x) && (right += w * pd)
    end
    vals[1] = left
    vals[end] = right
    normalize!(vals)

    return vals
end
#endregion

end


# using StructEquality
# @struct_equal BivariateKDE
# @struct_equal InterpKDE
# function test()
#     # const VarBins = Ref(Vector{Float64}(undef, NumVarBins - 1))
#     # const Kdes = Ref(Vector{BivariateKDE}(undef, NumVarBins))
#     # const KdeInterps = Ref(Vector{InterpKDE{<:BivariateKDE}}(undef, NumVarBins))

#     VarBins[] = Vector{Float64}(undef, NumVarBins - 1)
#     Kdes[] = Vector{BivariateKDE}(undef, NumVarBins)
#     KdeInterps[] = Vector{InterpKDE{<:BivariateKDE}}(undef, NumVarBins)
#     !isfile(makePath(:varbins)) || rm.((makePath(:varbins), makePath(:kdes), makePath(:kdeinterps)))
#     update()
#     global copyVarBins = copy(VarBins[])
#     global copyKdes = copy(Kdes[])
#     global copyKdeInterps = copy(KdeInterps[])

#     VarBins[] = Vector{Float64}(undef, NumVarBins - 1)
#     Kdes[] = Vector{BivariateKDE}(undef, NumVarBins)
#     KdeInterps[] = Vector{InterpKDE{<:BivariateKDE}}(undef, NumVarBins)
#     @assert !isassigned(Kdes[], 1)
#     loadData()
#     @assert VarBins[] == copyVarBins
#     @assert Kdes[] == copyKdes
#     @assert KdeInterps[] == copyKdeInterps
# end
