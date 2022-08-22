module NormDists
# using SpecialFunctions

export NormDist, pdf, pdff, cdf, cdff

const ROOT_2PI = sqrt(2.0 * π)
const ROOT2 = sqrt(2.0)
const INV_ROOT2 = 1.0 / ROOT2

struct NormDist
    μ::Float64
    σ::Float64
    front::Float64
    bottom::Float64
end
NormDist(mean::Float64, stdDev::Float64) = NormDist(mean, stdDev, 1.0 / (stdDev * ROOT_2PI), 2 * stdDev^2)

pdf(nd::NormDist, x::Float64)::Float64 = nd.front * exp(- (x - nd.μ)^2 / nd.bottom)
pdff(nd::NormDist) = x -> pdf(nd, x)

# Adapted from https://github.com/JuliaStats/StatsFuns.jl/blob/master/src/distrs/norm.jl#L45
cdf(d::NormDist, x::Float64)::Float64 = error("not implemented special functions") # erfc(-(x - d.μ)/d.σ * INV_ROOT2) / 2
cdff(d::NormDist) = x -> cdf(d, x)

end