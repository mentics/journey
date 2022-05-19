module ProbHist
using Dates
using Globals, DateUtil, LogUtil
using BaseTypes, Bins, ProbTypes
using Caches, HistData, Markets

export probHist

probHist(center::Real, interval::Int; up=false)::Prob = Prob(Float64(center), probHists(; up)[interval == 0 ? 1 : interval]) # TODO: do something better for interval 0

#region Local
const PHType = Vector{Float64}
const PROBHISTS = :probHists
const PROBHISTS_TYPE = Vector{PHType}
const CFG_DEF = Dict{Symbol,Any}(
    :maxInterval => 30,
    :numHistDays => 5000,
    :skipDays => 0, # So we can have some out-of-sample test data
    # :useWeights => false,
    # :weightDaysBack => 5,
    # :tailMult => 100.0
)

function __init__()
    if !Globals.has(:Distris)
        Globals.set(:Distris, CFG_DEF)
    end
end

probHists(; up::Bool=false)::Vector{PHType} = cache!(updateProbHists, PROBHISTS_TYPE, PROBHISTS, Hour(10); up)

function updateProbHists()::Vector{PHType}
    forDate = market().startDay
    daily = dataDaily(forDate)
    @log debug "Calculating historical distributions for" forDate (forDate == today()) daily[1]
    probHists = makeProbHists(Globals.get(:Distris), forDate, daily)
    return probHists
end

function makeProbHists(cfg, forDate::Date, data)::Vector{PHType}
    if !isBusDay(forDate)
        throw("Invalid forDate $(forDate) is not a bday")
    end

    res = Vector{PHType}()
    # Calc from/to by skipping data that is later than forDate and the amount configured to skip and including enough at the end to handle interval and weight calcs
    toBeforeDate = 1
    while data[toBeforeDate].date >= forDate
        toBeforeDate += 1
    end
    from = 1 + cfg[:skipDays] + toBeforeDate
    to = min(cfg[:skipDays] + cfg[:numHistDays] + cfg[:maxInterval], length(data))
    @log debug "Distri data" from to data[from].date data[to].date

    dv = @view data[from:to]
    rets = HistData.makeRetsExpirs2(dv, cfg[:maxInterval])
    @log debug "Calculated rets for daily data indexes" length(rets[end]) from to

    for interval in 1:cfg[:maxInterval]
        push!(res, makeProbHist(rets[interval]))
    end
    return res
end

function makeProbHist(rets::AVec{RetsItemType})::PHType
    # bins = zeros(numBins())
    vals = Bins.with(0.0)
    # lower = 0.0
    # upper = 0.0
    weightSum = 0.0
    for i in 1:length(rets)
        (;ret) = rets[i]
        weight = 20.0 / (20.0 + i)
        weightSum += weight
        if Bins.isLeft(ret) # ret <= binsLeft()
            # lower += weight
            vals[1] += weight
        elseif Bins.isRight(ret) # ret >= binsRight()
            # upper += weight
            vals[end] += weight
        else
            b = Bins.nearest(ret)
            # if !Bins.isValidInd(b)
            #     @error "Invalid bin number" b ret i binsLeft()
            # end
            # vals[b-1] += weight # TODO: why -1? because binNearest works on numVals, but this was using just numBins
            vals[b] += weight
        end
    end
    # return vcat(lower/weightSum, vals ./ weightSum, upper/weightSum)
    vals ./= weightSum
    return vals
end
#endregion

end