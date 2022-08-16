module ProbKde
using Dates, KernelDensity
using LogUtil, CollUtil, VectorCalcUtil
using BaseTypes, Bins, ProbTypes
using Caches, HistData, Markets, Calendars

# TODO: consider adding tex=0, x=x data to force endpoint data

export probKde

function probKde(center::Real, tex::Real, var::Real; up=false)::Prob
    (up || Updated[] < now(UTC) - Hour(8)) && update()
    return Prob(Float64(center), makePdv(tex, var))
end

makeProb(center::Real, tex::Real, var::Real) = Prob(Float64(center), makePdv(tex, var))

#region Local
NumVarBins = 20
MaxDays = 200
const Updated = Ref{DateTime}(DateTime(0))
const VarBins = Vector{Float64}(undef, NumVarBins - 1)
const Kdes = Vector{BivariateKDE}(undef, NumVarBins)
const KdeInterps = Vector{InterpKDE{<:BivariateKDE}}(undef, NumVarBins)

function update()
    println("Updating ProbKde")
    forDate = market().startDay
    dailySpy = dataDaily(forDate, "SPY")
    dailyVix = dataDaily(forDate, "VIX")
    @log debug "Calculating historical distributions for" forDate (forDate == today()) dailySpy[1] dailyVix[1]
    global rets = calcRets(MaxDays, dailySpy, dailyVix)
    global retsNtv = ntvFromVnt(rets)
    calcVarBins(retsNtv)
    calcKdes(retsNtv)
    Updated[] = now(UTC)
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
        VarBins[bin] = varSorted[i]
        bin += 1
    end
end

function calcKdes(retsNtv::NamedTuple)
    global datas = [(Vector{Float64}(), Vector{Float64}()) for _ in 1:NumVarBins]
    var = retsNtv.var
    tex = retsNtv.tex
    ret = retsNtv.ret
    for i in eachindex(var)
        bin = findVarBin(var[i])
        push!(datas[bin][1], tex[i])
        push!(datas[bin][2], ret[i])
    end
    for i in 1:NumVarBins
        Kdes[i] = calcKde(datas[i])
        KdeInterps[i] = InterpKDE(Kdes[i])
    end
end

function findVarBin(val::Float64)
    for i in 1:length(VarBins)
        if val < VarBins[i]
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
    k = Kdes[varBin]
    ik = KdeInterps[varBin]
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