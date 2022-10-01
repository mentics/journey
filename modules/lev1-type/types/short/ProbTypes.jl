module ProbTypes
using SH, VectorCalcUtil

export pt, Prob
const pt = @__MODULE__

struct Prob
    center::Float64 # Generally unused except making sure we don't mix things with different centers
    vals::Vector{Float64}
end
SH.getCenter(p::Prob) = p.center
SH.getVals(p::Prob) = p.vals
valLeft(p::Prob) = p.vals[1]
valRight(p::Prob) = p.vals[end]

Base.:(+)(p1::Prob, p2::Prob) = ( @assert p1.center === p2.center ; Prob(p1.center, normalize!(p1.vals + p2.vals)) )

import Bins
function combine(p1::Prob, p2::Prob, w1::Number=0.5, w2::Number=1.0-w1)
    @assert w1 + w2 ≈ 1.0
    p1prices = Bins.xs() .* p1.center
    p2prices = Bins.xs() .* p2.center
    p1coid = sum(p1prices .* p1.vals)
    p2coid = sum(p2prices .* p2.vals)
    newCenter = w1 * p1coid + w2 * p2coid

    # nLeftVal = newCenter * Bins.left()
    # nRightVal = newCenter * Bins.right()
    @show p1coid p2coid newCenter
    p1xs = (newCenter / p1.center) .* Bins.xs()
    p2xs = (newCenter / p2.center) .* Bins.xs()
    p1Left, p1Right = nLeftVal / p1.center, nRightVal / p1.center
    p2Left, p2Right = nLeftVal / p2.center, nRightVal / p2.center
    @show p1Left p1Right p2Left p2Right

    left1Ind =
    xsNew = Bins.xs() .* newCenter
    leftNew = 0.0
    rightNew = 0.0
    for x in Bin.xs()
        # x1 = x * p1.center
        #  <= Bins.XLEFT
    end
    isleft = x -> x <= xsNew[1]
    isright = x -> x >= xsNew[end]

    leftVal = sum(filter(isleft, p1xs)) + sum(filter(isleft, p2prices))
    rightVal = sum(filter(isright, p1xs)) + sum(filter(isright, p2prices))
    valAt(p1, xsNew)
end

#=
400 / 400 = 1
420 / 400 = 1.05
400 / 420 = .95238
420 / 420 = 1
The new center also changes the scale of the spread, so we can't just copy over the arrays. We have to split bins and ratio it.
=#
# function shift(p1::Prob, newCenter)
#     tx = newCenter / p1.center
#     shifted = tx - 1.0
#     xs = Bins.xs() .+ shifted
#     @assert (xs[end] - xs[1] - Bins.width()) ≈ Bins.SPAN "new xs doesn't match span"
#     # @assert Bins.xs()[end ÷ 2 + 1] *  == xs[]
#     # Bins.xs() * p1.center == xs * newCenter
#     @show shifted xs[1] xs[end]
#     vals = p1.vals
#     valsNew = Bins.with(-Inf)
#     if shifted > 0
#         nst = Bins.nearest(xs[1])
#         valsNew[1] = sum(vals[1:nst])
#         @show nst valsNew[1]
#         for i in nst+1:length(vals)-1 # 2:(length(valsNew) - nst)
#             valsNew[i - nst + 1] = vals[i]
#         end
#         valStretch = vals[end] / nst
#         for i in (length(vals) - nst + 1):length(vals)
#             valsNew[i] = valStretch
#         end
#     else
#         valsNew[end] = sum(valsNew[Bins.nearest(xs[end]):end])
#     end
#     mx, mxi = findmax(p1.vals)
#     mxNew, mxiNew = findmax(valsNew)
#     @assert mx ≈ mxNew string(mx, ' ', mxNew)
#     @assert Bins.xs()[mxi] * p1.center ≈ Bins.xs()[mxiNew] * newCenter string(Bins.xs()[mxi] * p1.center, ' ', Bins.xs()[mxiNew] * newCenter)
#     return Prob(newCenter, valsNew)
# end

# function valFor(vals, numLeft::Integer, numRight::Integer, at::Number)
#     at > Bins.left() || return vals[1] / numLeft
#     at < Bins.right() || return vals[end] / numRight
# end

function shift(p1::Prob, newCenter)
    valsNew = Bins.with(-Inf)
    Bins.iterBins() do i, left, right
        priLeft = left * newCenter
        priRight = right * newCenter
        valsNew[i] = betweenPrices(p1, priLeft, priRight)
        @assert valsNew[i] >= 0.0 string(i, ' ', valsNew[i])
    end

    # @assert sum(valsNew) ≈ 1.0 string(sum(valsNew), ' ', 1.0)
    # mx, mxi = findmax(p1.vals)
    # mxNew, mxiNew = findmax(valsNew)
    # @assert mx ≈ mxNew string(mx, ' ', mxNew)
    # @assert Bins.xs()[mxi] * p1.center ≈ Bins.xs()[mxiNew] * newCenter string(Bins.xs()[mxi] * p1.center, ' ', Bins.xs()[mxiNew] * newCenter)

    return Prob(newCenter, valsNew)
end

function betweenPrices(p::Prob, left::Float64, right::Float64)::Float64
    vals = getVals(p)
    ratL = Bins.ratio(left / p.center)
    ratR = Bins.ratio(right / p.center)
    @show left right (left / p.center) (right / p.center) ratL ratR
    @assert ratL.ratioLeft >= 0
    ratL.rightX != Inf || return vals[end] * (ratR.ratioLeft - ratL.ratioLeft)
    ratR.leftX != 0.0 || return vals[1] * (ratR.ratioLeft - ratL.ratioLeft)
    s = ratL.ratioRight * vals[ratL.ind]
    @assert s >= 0.0
    # error("stop")
    # ratL.leftX != 0.0 ?
    #     s = ratL.ratioRight * vals[ratL.ind] : # ratL.ratioRight * vals[ratL.rightI] : # ratL.ratioLeft * vals[ratL.leftI] +
    #     s = ratL.ratioRight * vals[1]
    for i in ratL.ind+1:ratR.ind-1
        s += vals[i]
    end
    @assert s >= 0.0
    # error("stop")
    s += ratR.ratioLeft * vals[ratR.ind]
    @assert s >= 0.0
    # ratR.rightX != Inf ?
    #     s += ratR.ratioLeft * vals[ratR.ind] : # ratR.ratioLeft * vals[ratR.leftI] : # + ratR.ratioRight * vals[ratR.rightI]
    #     s += ratR.ratioLeft * vals[end]
    return s
end

end