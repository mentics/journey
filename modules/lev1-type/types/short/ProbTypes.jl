module ProbTypes
using SH, BaseTypes, VectorCalcUtil
import Bins

export pt, Prob, ProbSimple, ProbWithMin, toProbWithMin

const CONFIG7 = Ref((;
    binmin = 0.0, # 1e-1 * Bins.WIDTH,
    density_left = 0.0, # 1e-3,
    density_right = 0.0, # 1e-4,
    p0 = 0.0, # 1e-7,
    p2 = 0.0, # 1e-7
))

abstract type Prob end

struct ProbSimple <: Prob
    center::Float64
    vals::Vector{Float64}
end
struct ProbWithMin <: Prob
    center::Float64
    vals::Vector{Float64}
    p0::Float64
    p2::Float64
    binmin::Float64
    densityleft::Float64
    densityright::Float64
end
SH.getCenter(p::Prob) = p.center
SH.getVals(p::Prob) = p.vals
valLeft(p::Prob) = p.vals[1]
valRight(p::Prob) = p.vals[end]

Base.:(+)(p1::Prob, p2::Prob) = ( @assert p1.center === p2.center ; Prob(p1.center, normalize!(p1.vals + p2.vals)) )

function toProbWithMin(prob::ProbSimple)::ProbWithMin # , binmin::Float64, leftdensity::Float64, rightdensity::Float64)::ProbWithMin
    (;binmin, density_left, density_right, p0, p2) = CONFIG7[]
    fromvals = prob.vals
    vals = Bins.empty()
    vals[1] = fromvals[1] + density_left * Bins.XLEFT
    vals[end] = fromvals[end] + density_right * (2 - Bins.XRIGHT)
    total = p0 + p2
    for i in eachindex(vals)
        p = max(binmin, fromvals[i])
        vals[i] = p
        total += p
    end
    vals ./= total
    return ProbWithMin(prob.center, vals, p0, p2, binmin, density_left, density_right)
end

# function combineProbs2(p1::Prob, p2::Prob, w1::Number=0.5, w2::Number=1.0-w1)
#     @assert w1 + w2 ≈ 1.0
#     @assert p1.center > 0.0
#     @assert p2.center > 0.0
#     @assert isnothing(findfirst(x -> !isfinite(x), p1.vals))
#     @assert isnothing(findfirst(x -> !isfinite(x), p2.vals))
#     p1prices = Bins.xs() .* p1.center
#     p2prices = Bins.xs() .* p2.center
#     p1coid = sum(p1prices .* p1.vals)
#     p2coid = sum(p2prices .* p2.vals)
#     newCenter = w1 * p1coid + w2 * p2coid
#     @assert isfinite(newCenter) string("isfinite(newCenter) ", newCenter, ' ', w1, ' ', w2)
#     p1n = shift(p1, newCenter)
#     p2n = shift(p2, newCenter)
#     return Prob(newCenter, p1n.vals .* w1 + p2n.vals * w2)
# end
combineProbs(ps::Coll{Prob}; ws=fill(1.0, length(ps))) = combineProbs(calcCentroid(ps), ps; ws)
# function combineProbs(centerNew, ps::Coll{Prob}; ws=fill(1.0, length(ps)))
#     @assert isfinite(centerNew) string("isfinite(newCenter) ", centerNew, ' ', w1, ' ', w2)
#     valsNew = Bins.with(0.0)
#     for (i, p) in enumerate(ps)
#         pn = shift(p, centerNew)
#         valsNew .+= pn.vals .* ws[i]
#     end
#     return Prob(centerNew, normalize!(valsNew))
# end
function combineProbs(centerNew, ps::Coll{Prob}; ws=fill(1.0, length(ps)))
    @assert isfinite(centerNew) string("isfinite(newCenter) ", centerNew, ' ', w1, ' ', w2)
    valsNew = Bins.with(1.0)
    i = 1
    for (p, w) in zip(ps, ws)
        pn = shift(p, centerNew)
        # valsNew .*= 1 .+ (pn.vals .* w)
        # println("Adding $i with weight $w")
        valsNew .+= (pn.vals .* w)
        normalize!(valsNew)
        i += 1
    end
    return Prob(centerNew, normalize!(valsNew))
end
function testCombineProbs()

end

function calcCentroid(ps::Coll{Prob})
    coid = 0
    for p in ps
        @assert p.center > 0.0
        @assert isnothing(findfirst(x -> !isfinite(x), p.vals))
        prices = Bins.xs() .* p.center
        coid += sum(prices .* p.vals)
    end
    centroid = coid / length(ps)
    @assert isfinite(centroid) string("isfinite(newCenter) ", centroid, ' ', w1, ' ', w2)
    return centroid
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
    # @show "shift" p1.center newCenter
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

    @assert isapprox(sum(valsNew), sum(p1.vals); atol=1e-2) string(sum(valsNew), ' ', sum(p1.vals))
    return Prob(newCenter, valsNew)
end
using VectorCalcUtil
function testShift()
    p1 = Prob(100.0, vcu.normalize!(Bins.with(1.0)))
    shift(p1, 103.2)
    shift(p1, 97.6)
end

function betweenPrices(p::Prob, left::Float64, right::Float64, show=false)::Float64
    vals = getVals(p)
    ratL = Bins.ratio(left / p.center)
    ratR = Bins.ratio(right / p.center)
    show && (@show left right (left / p.center) (right / p.center) ratL ratR)
    @assert ratL.ratioLeft >= 0
    ratL.rightX != Inf || return vals[end] * (ratR.ratioLeft - ratL.ratioLeft)
    ratR.leftX != 0.0 || return vals[1] * (ratR.ratioLeft - ratL.ratioLeft)
    if ratL.ind == ratR.ind
        return (ratL.ratioRight - ratL.ratioLeft) * vals[ratL.ind]
    end
    s = ratL.ratioRight * vals[ratL.ind]
    @assert s >= 0.0
    for i in ratL.ind+1:ratR.ind-1
        # println("adding middle ", i)
        s += vals[i]
    end
    @assert s >= 0.0
    s += ratR.ratioLeft * vals[ratR.ind]
    @assert s >= 0.0
    show && (@show s)
    return s
end

end