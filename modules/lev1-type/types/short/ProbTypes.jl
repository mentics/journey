module ProbTypes
using SH, VectorCalcUtil

export Prob
export valLeft, valRight

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
function combine(p1::Prob, p2::Prob)
    xs1 = Bins.xs() .* p1.center
    xs2 = Bins.xs() .* p2.center
    s1 = sum(xs1 .* p1.vals)
    s2 = sum(xs2 .* p2.vals)
    centerNew = (s1 + s2) / 2
    # xsNew = Bins.xs() .* centerNew
    leftNew = 0.0
    rightNew = 0.0
    for x in Bin.xs()
        # x1 = x * p1.center
        #  <= Bins.XLEFT
    end
    isleft = x -> x <= xsNew[1]
    isright = x -> x >= xsNew[end]

    leftVal = sum(filter(isleft, xs1)) + sum(filter(isleft, xs2))
    rightVal = sum(filter(isright, xs1)) + sum(filter(isright, xs2))
    valAt(p1, xsNew)
end

end