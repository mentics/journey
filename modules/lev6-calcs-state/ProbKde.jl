module ProbKde
using Dates, MultiKDE
using LogUtil
using BaseTypes, Bins, ProbTypes
using HistData, Markets, Calendars
# using Globals, DateUtil, LogUtil
# using Caches, HistData, Markets

# probHist(center::Real, interval::Int; up=false)::Prob = Prob(Float64(center), probHists(; up)[interval == 0 ? 1 : interval]) # TODO: do something better for interval 0

#region Local
const Kde = Ref{KDEMulti}()

function update()
    forDate = market().startDay
    dailySpy = dataDaily(forDate, "SPY")
    dailyVix = dataDaily(forDate, "VIX")
    @log debug "Calculating historical distributions for" forDate (forDate == today()) dailySpy[1] dailyVix[1]
    rets = calcRets(200, dailySpy, dailyVix)
    Kde[] = calcKde(rets)
    return makeProb
end

function calcRets(maxNumDays::Int, daily::DailyType, dailyVar::DailyType)::Vector{NamedTuple}
    res = Vector{NamedTuple}()
    for i in 1:(length(daily) - maxNumDays)
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
            varOpen = rowVarFrom.open
            varClose = rowVarFrom.close
            texOpen = calcTex(getMarketOpen(rowFrom.date), rowTo.date)
            texClose = calcTex(getMarketClose(rowFrom.date), rowTo.date)
            push!(res, (;texOpen, varOpen, retOpen)) # , date=rowFrom.date, dailyIndex=i+j))
            push!(res, (;texClose, varClose, retClose)) # , date=rowFrom.date, dailyIndex=i+j))
        end
    end
    return res
end

const KdeDims = [ContinuousDim(), ContinuousDim(), ContinuousDim()]

function calcKde(rets::AVec{NamedTuple})::KDEMulti
    println("Calcing kde with length rets ", length(rets))
    observations = map(x -> collect(values(x)), rets)
    return KDEMulti(KdeDims, observations)
end

function makeProb(tex, vix)
    vals = Bins.with(0.0)

    for (i, x) in Bins.midsi()
        vals[i] = Bins.width() * pdf(ik, x)
    end
    # TODO: messy
    left = 0.0
    right = 0.0
    w = Float64(k.x.step)
    wh = w/2
    for x in k.x
        x2 = x + wh
        Bins.isLeft(x2) && (left += w * pdf(ik, x2))
        Bins.isRight(x2) && (right += w * pdf(ik, x2))
    end
    vals[1] = left
    vals[end] = right

    return vals
end
#endregion

end