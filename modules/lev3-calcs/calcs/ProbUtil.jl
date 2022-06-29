module ProbUtil
using Bins, ProbTypes
using CalcUtil, VectorCalcUtil, NormDists

export probsNormDist, probsCdf

function probsNormDist(center::Real, stdDev::Float64, shift::Float64=0.0)::Prob
    d = NormDist(1.0, stdDev)
    vals = Bins.empty()
    vals[1] = cdf(d, Bins.XLEFT - shift)
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

# function probsCdf(probs::Prob, x::Float64)::Float64
#     if x <= binsLeft()
#         return probs.vals[1]
#     elseif x >= binsRight()
#         return 1.0
#     end

#     sum = probs.vals[1]
#     left = binsLeft()
#     vals = probs.vals
#     for i in binItr()
#         right = left + binWidth()
#         val = vals[i]
#         if x > right
#             sum += val
#         else
#             sum += val * (x - left) / binWidth()
#             break
#         end
#         left = right
#     end
#     return sum
# end

end