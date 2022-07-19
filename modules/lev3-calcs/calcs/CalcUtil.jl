module CalcUtil
using SH, BaseTypes, Bins, ProbTypes, RetTypes

export calcMetrics #, calcKelly, calcKelly!

calcMetrics(prob::Prob, ret::Ret, bins=Bins.inds()) = ( @assert getCenter(prob) == getCenter(ret) ; calcMetrics(prob, getVals(ret), ret.numLegs, bins) )
function calcMetrics(prob::Prob, vals::AVec{Float64}, numLegs::Int, binsi=Bins.inds())
    pvals = getVals(prob)
    profit = 0.0
    for i in binsi
        v = vals[i]
        v > 0.0 && (profit += pvals[i] * v)
    end
    global cap = 0.5 + log(1.0 + profit)
    adjust = 0.005 * numLegs
    # @info "cap" cap adjust
    mm = calcMetrics1(pvals, vals, cap, adjust, binsi)
    return (;mm..., numLegs)
end

#region Local
# TODO: if use fewer bins, prob isn't adjusted, so it will be lower
MINP = 0.01 * Bins.binPercent()
function calcMetrics1(pvals::AVec{Float64}, vals::AVec{Float64}, cap::Float64, adjust::Float64, binsi)
    # @info "calcMetrics1" cap adjust
    profit = loss = prob = 0.0
    mn = Inf
    mx = -Inf
    # @info "check" cap adjust
    # for (p, v) in Iterators.zip(pvals, vals)
    totalP = 0.0
    for i in binsi
        p = pvals[i]
        if p < MINP
            # println("Skipping ", p, ' ', MINP)
            continue
        end
        totalP += p
        v = vals[i]
        vadj = min(cap, v - adjust)
        ad = p * vadj
        vadj > 0.0 && (profit += ad ; prob += p)
        vadj < 0.0 && (loss += ad)
        v < mn && (mn = v)
        v > mx && (mx = v)
    end
    # @info "cm" totalP cap adjust
    ev = profit + loss
    evr = calcEvr(profit, loss)
    return (; profit, loss, ev, evr, prob=prob / totalP, mn, mx)
end

# not sure where that number comes from but needed to normalize
# adjprofit(ind, profit) = profit * ((201 - abs(ind - 201 - 1))/116.79617818664343)

function calcEvr(profit::Float64, loss::Float64)::Float64
    @assert profit >= 0.0
    @assert loss <= 0.0
    # tot = profit - loss
    # tot > 0.0 || return NaN
    p = profit #/tot
    l = loss #/tot
    # ev = p + l
    # return p / (1 - 2*l) + 2*l / (1 + p)
    return p == 0.0 ? 100 * l : p / (1 - l) + l / (1 + p)
    # return p / (1 - l)
    # return p / (1 - 2*l) # used this before, but it let profit overshadow loss... but maybe it was just because it was a deep hole right near price with less than 2 days left
    # evr = if p == 0.0; l
    #       elseif l == 0.0; p
    #       else ev / (1 - l) end
    # return evr
end

#  fig, ax1, hm1 = heatmap(CalcUtil.getEvrs()...) ; Colorbar(fig[:,end+1], hm1) ; fig
function getEvrs()
    ps = range(0.0, 10.0; length=100)
    ls = range(-10.0, 0.0; length=100)
    evrs = [CalcUtil.calcEvr(p, l) for p in ps, l in ls]
    return (ps, ls, evrs)
end

calcMetrics2(p::Prob, v::AVec{Float64}) = calcMetrics(getVals(p), v)
function calcMetrics2(pv::AVec{Float64}, v::AVec{Float64})
    profit = loss = prob = 0.0
    lo = -0.04
    hi = .04
    for (p, v) in Iterators.zip(pv, v)
        if v >= hi
            profit += p * log(1.0 + v)
            prob += p
        elseif v <= lo
            loss += p * (1.0 - (1.0 - v)^2)
        else
            continue
        end
    end
    ev = profit + loss
    # TODO: is 1000 * the right multiple?
    evr = if loss > -0.01; 1000 * profit
          elseif profit == 0.0; loss
          else profit / abs(loss) end
    return (; profit, loss, ev, evr, prob)
end


# calcEvRatio(pvals, vals) = ( (p, l) = calcEvPnl(pvals, vals) ; l >= -0.01 ? 100 * p : p / abs(l) )
# function calcEvPnl(pvals, vals)
#     profit = loss = 0.0
#     for (p, v) in Iterators.zip(pvals, vals)
#         ad = p * v
#         v > 0.0 && (profit += ad)
#         v < 0.0 && (loss += ad)
#     end
#     return (profit, loss)
# end

# function calcProbProfit(pvals, vals)
#     s = 0.0
#     for i in 1:length(vals)
#         vals[i] > 0.0 && (s += pvals[i])
#     end
#     return s
# end

using Kelly
calcKelly(pv, rets) = calcKelly!(pv, copy(rets))
function calcKelly!(pv, rets)
    # mn, mx = extrema(rets)
    # risk = abs(mn)
    mn = minimum(rets)
    risk = abs(mn)
    if mn > 0.0
        return 17 + 1000 * mn
    end
    rets ./= risk
    # @assert minimum(rets) ≈ -1.0
    if abs(minimum(rets) + 1.0) > 0.0001
        error("What the hog doin'? ", minimum(rets))
    end
    kel = Kelly.optimize(pv, rets)
    isnan(kel) && return NaN
    return kel
end

# function smooth!(v::Vector{Float64}, k::Float64=1.0)
#     # Skip first and last
#     len = length(v)
#     s = sum(v) - v[1] - v[end]
#     newS = k*s
#     toAdd = newS / (len-2)
#     n = (1.0 - v[1] - v[end]) / (s + newS)
#     for i in 2:(len-1)
#         v[i] = n * (v[i] + toAdd)
#     end
# end

# function smooth!(v::Vector{Float64}, winSize::Int=10)
#     # Skip first and last
#     for i in (1+winSize):(length(v)-1)
#         sum = 0.0
#         for j in (i-winSize):i
#             sum += v[j]
#         end
#         y = sum / winSize
#         for j in (i-winSize):i
#             v[j] = y
#         end
#     end
# end

# function findMidBin(v::Vector{Float64})
#     @assert sum(v) ≈ 1.0
#     s = 0.0
#     for i in eachindex(v)
#         s += v[i]
#         s >= 0.5 && return i
#     end
#     return -1
# end
#endregion

end