module ProbTypes
using SH

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

end