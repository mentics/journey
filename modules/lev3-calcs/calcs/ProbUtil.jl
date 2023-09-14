module ProbUtil
using Bins, ProbTypes
using CalcUtil, VectorCalcUtil, NormDists

ERROR: not in use

export probsNormDist, probsCdf

probrand(prob::ProbWithMin) = xrforp(prob, rand())

cdf(prob::ProbWithMin, x::Float64) = cdfr(prob, x / prob.center)
function cdfr(prob::ProbWithMin, xr::Float64)
    xr > 0 || return prob.p0
    # xs = Bins.xs()
    vals = prob.vals
    xr > Bins.XLEFT || return cdf_left(prob, xr)
    xr < Bins.XRIGHT || return cdf_right(prob, xr)

    p = prob.p0 + vals[1]

    ind = Bins.leftOf(xr)
    # p += sum(vals[2:ind])
    p += sum(vals[i] for i in 2:ind; init=0.0)
    diff = round(xr - Bins.x(ind); digits=10)
    @assert diff >= 0
    if diff < Bins.WIDTH_HALF
        p -= vals[ind] * diff / Bins.WIDTH
    else
        w = round(diff - Bins.WIDTH_HALF; digits=10)
        @assert 0 <= w <= Bins.WIDTH_HALF (;w, diff, ind, p)
        p += vals[ind+1] * (w / Bins.WIDTH)
    end

    # for (i, xbin) in Bins.MIDSI
    #     if x < xbin
    #         diff = xbin - x
    #         if diff > Bins.WIDTH_HALF
    #             # it ends in the previous bin

    #         end
    #         # TODO: add partial from last bin?
    #         break
    #     end
    #     p += vals[i]
    # end
    return p
end

xforp(prob::ProbWithMin, p::Float64)::Float64 = xrforp(prob, prob.p0, prob.p2, p) * prob.center
xforp(prob::ProbSimple, p::Float64)::Float64 = xrforp(prob, 0.0, 0.0, p) * prob.center
function xrforp(prob::Prob, p0::Float64, p2::Float64, pquery::Float64)::Float64
    @assert 0 <= pquery <= 1
    vals = prob.vals
    leftval = p0 + vals[1]
    pquery > leftval || return xforp_left(prob, p0, pquery) # (p / leftval) * Bins.XLEFT
    p = pquery - leftval

    for (i, x) in Bins.MIDSI
        v = vals[i]
        p -= v
        # @show v p
        if p <= 0
            rat = 1 + p / v
            return x + (rat - 0.5) * Bins.WIDTH
        end
    end
    # println((;p0, p))

    res = xforp_right(prob, p2, pquery)
    @assert 0 <= res <= 2 (;res, pquery)
    return res
    # widthright = 2 - Bins.XRIGHT
    # rightdensity = rightdensity + vals[end] / widthright
    # return Bins.XRIGHT + p / rightdensity
end

#region Local

function cdf_left(prob::ProbWithMin, x::Float64)::Float64
    @assert x >= 0
    # A triangle with bottom side = Bins.XLEFT with area = prob.vals[1]
    # a = w * h / 2
    heightright = 2 * prob.vals[1] / Bins.XLEFT
    height = heightright * (x / Bins.XLEFT)
    area = x * height / 2
    # TODO: simplify
    return prob.p0 + area
end

function cdf_right(prob::ProbWithMin, x::Float64)::Float64
    @assert x <= 2 (;x, pc=prob.center)
    WIDTH_RIGHT = 2 - Bins.XRIGHT
    width = 2 - x
    heightleft = 2 * prob.vals[end] / WIDTH_RIGHT
    height = heightleft * (width / WIDTH_RIGHT)
    area = width * height / 2
    # TODO: simplify
    return min(1.0, 1.0 - area) # to deal with float inaccuracies, return must be <= 1.0
end

function xforp_left(prob::Prob, p0::Float64, p::Float64)::Float64
    @assert p >= 0
    # A triangle with bottom side = Bins.XLEFT with area = prob.vals[1], A = W * H / 2
    # h(w) = (w / W) * H
    # a = w * h(w) / 2
    # a = w * (w / W) * H / 2
    # a = w^2 * H / W / 2
    # w = sqrt(2 * W * a / H)
    p > p0 || return 0.0
    a = p - p0
    w0 = Bins.XLEFT
    h0 = 2 * prob.vals[1] / w0
    w = sqrt(2 * w0 * a / h0)
    return w
end

function xforp_right(prob::Prob, p2::Float64, p::Float64)::Float64
    @assert p >= 0
    # A triangle with bottom `side = 2 - Bins.XRIGHT` with `area = prob.vals[end]`, A = W * H / 2
    # h(w) = (w / W) * H
    # a = w * h(w) / 2
    # a = w * (w / W) * H / 2
    # a = w^2 * H / W / 2
    # w = sqrt(2 * W * a / H)
    p1 = (1 - p2)
    p < p1 || return 2.0
    a = p1 - p
    w0 = 2 - Bins.XRIGHT
    h0 = 2 * prob.vals[end] / w0
    w = sqrt(2 * w0 * a / h0)
    return 2 - w
end

#endregion

#region Old

# function cdfFromLeft(prob::Prob, xr::Float64)
#     x = xr / prob.center
#     xs = Bins.xs()
#     pv = prob.vals
#     if x <= xs[1]
#         p = 0.0
#     elseif x >= xs[end]
#         p = 1.0
#     else
#         p = pv[1]
#         for i in Bins.binds()
#             if xs[i] > x
#                 p += partialbinval()
#                 # TODO: add partial from last bin?
#                 break
#             end
#             p += pv[i]
#         end
#     end
#     return p
# end

# function cdfFromRight(prob::Prob, xr::Float64)
#     x = xr / prob.center
#     xs = Bins.xs()
#     pv = prob.vals
#     if x <= xs[1]
#         p = 1.0
#     elseif x >= xs[end]
#         p = 0.0
#     else
#         p = pv[end]
#         for i in Iterators.reverse(Bins.binds())
#             if xs[i] < x
#                 break
#             end
#             p += pv[i]
#         end
#     end
#     return p
# end

# function probsNormDist(center::Real, stdDev::Float64, shift::Float64=0.0)::Prob
#     d = NormDist(1.0, stdDev)
#     vals = Bins.empty()
#     vals[1] = cdf(d, Bins.XLEFT - shift)
#     @assert isfinite(vals[1]) "Invalid val $(vals[1]) center=$(center) stdDev=$(stdDev) shift=$(shift) XLEFT=$(Bins.XLEFT)"
#     for (i, x) in Bins.midsi()
#         vals[i] = Bins.width() * pdf(d, x - shift)
#     end
#     # i = 1
#     # for x in binXs()[2:end-1]
#     #     i += 1
#     #     vals[i] = binWidth() * pdf(d, x)
#     # end

#     # i = 1
#     # for x in binXs()[2:end-1]
#     #     i += 1
#     #     left = pdf(d, x - .5*binWidth())
#     #     right = pdf(d, x + .5*binWidth())
#     #     # vals[i] = binWidth() * pdf(d, x)
#     #     vals[i] = binWidth() * (right - left)
#     # end

#     # for i in binItr()
#     #     right = binsLeft() + i*binWidth()
#     #     rightCdf = cdf(d, right)
#     #     vals[i] = rightCdf - leftCdf
#     #     left = right
#     #     leftCdf = rightCdf
#     # end

#     vals[end] = 1.0 - cdf(d, Bins.XRIGHT - shift)
#     normalize!(vals)
#     return Prob(Float64(center), vals)
# end

# function probFromLeft(prob::Prob, x::Float64)
#     xs = Bins.xs()
#     pv = prob.vals
#     res = 0.0
#     p = 0.0
#     if x <= pv[1]
#         res = xs[1]
#         p = pv[1]
#     elseif x >= 1.0 - pv[end]
#         res = xs[end]
#         p = 1.0
#     else
#         p = pv[1]
#         for i in Bins.binds()
#             p += pv[i]
#             res = xs[i]
#             if p >= x
#                 break
#             end
#         end
#     end
#     return (res * prob.center, p)
# end

# function probFromRight(prob::Prob, x::Float64)
#     xs = Bins.xs()
#     pv = prob.vals
#     res = 0.0
#     p = 0.0
#     if x <= pv[end]
#         res = xs[end]
#         p = pv[end]
#     elseif x >= 1.0 - pv[end]
#         res = xs[1]
#         p = 1.0
#     else
#         p = pv[end]
#         for i in Iterators.reverse(Bins.binds())
#             p += pv[i]
#             res = xs[i]
#             if p >= x
#                 break
#             end
#         end
#     end
#     return (res * prob.center, p)
# end

#endregion
end