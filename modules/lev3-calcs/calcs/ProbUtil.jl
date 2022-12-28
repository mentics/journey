module ProbUtil
using Bins, ProbTypes
using CalcUtil, VectorCalcUtil, NormDists

export probsNormDist, probsCdf

function probsNormDist(center::Real, stdDev::Float64, shift::Float64=0.0)::Prob
    d = NormDist(1.0, stdDev)
    vals = Bins.empty()
    vals[1] = cdf(d, Bins.XLEFT - shift)
    @assert isfinite(vals[1]) "Invalid val $(vals[1]) center=$(center) stdDev=$(stdDev) shift=$(shift) XLEFT=$(Bins.XLEFT)"
    for (i, x) in Bins.midsi()
        vals[i] = Bins.width() * pdf(d, x - shift)
    end
    # i = 1
    # for x in binXs()[2:end-1]
    #     i += 1
    #     vals[i] = binWidth() * pdf(d, x)
    # end

    # i = 1
    # for x in binXs()[2:end-1]
    #     i += 1
    #     left = pdf(d, x - .5*binWidth())
    #     right = pdf(d, x + .5*binWidth())
    #     # vals[i] = binWidth() * pdf(d, x)
    #     vals[i] = binWidth() * (right - left)
    # end

    # for i in binItr()
    #     right = binsLeft() + i*binWidth()
    #     rightCdf = cdf(d, right)
    #     vals[i] = rightCdf - leftCdf
    #     left = right
    #     leftCdf = rightCdf
    # end

    vals[end] = 1.0 - cdf(d, Bins.XRIGHT - shift)
    normalize!(vals)
    return Prob(Float64(center), vals)
end

function cdfFromLeft(prob::Prob, xr::Float64)
    x = xr / prob.center
    xs = Bins.xs()
    pv = prob.vals
    if x <= xs[1]
        p = 0.0
    elseif x >= xs[end]
        p = 1.0
    else
        p = pv[1]
        for i in Bins.binds()
            if xs[i] > x
                # TODO: add partial from last bin?
                break
            end
            p += pv[i]
        end
    end
    return p
end

function cdfFromRight(prob::Prob, xr::Float64)
    x = xr / prob.center
    xs = Bins.xs()
    pv = prob.vals
    if x <= xs[1]
        p = 1.0
    elseif x >= xs[end]
        p = 0.0
    else
        p = pv[end]
        for i in Iterators.reverse(Bins.binds())
            if xs[i] < x
                break
            end
            p += pv[i]
        end
    end
    return p
end

function probFromLeft(prob::Prob, x::Float64)
    xs = Bins.xs()
    pv = prob.vals
    res = 0.0
    p = 0.0
    if x <= pv[1]
        res = xs[1]
        p = pv[1]
    elseif x >= 1.0 - pv[end]
        res = xs[end]
        p = 1.0
    else
        p = pv[1]
        for i in Bins.binds()
            p += pv[i]
            res = xs[i]
            if p >= x
                break
            end
        end
    end
    return (res * prob.center, p)
end

function probFromRight(prob::Prob, x::Float64)
    xs = Bins.xs()
    pv = prob.vals
    res = 0.0
    p = 0.0
    if x <= pv[end]
        res = xs[end]
        p = pv[end]
    elseif x >= 1.0 - pv[end]
        res = xs[1]
        p = 1.0
    else
        p = pv[end]
        for i in Iterators.reverse(Bins.binds())
            p += pv[i]
            res = xs[i]
            if p >= x
                break
            end
        end
    end
    return (res * prob.center, p)
end

function probsCdf(probs::Prob, x::Float64)::Float64
    if x <= binsLeft()
        return probs.vals[1]
    elseif x >= binsRight()
        return 1.0
    end

    sum = probs.vals[1]
    left = binsLeft()
    vals = probs.vals
    for i in binItr()
        right = left + binWidth()
        val = vals[i]
        if x > right
            sum += val
        else
            sum += val * (x - left) / binWidth()
            break
        end
        left = right
    end
    return sum
end

end