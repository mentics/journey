module MLyze
using StatsBase, Distributions, KernelDensity
import VectorCalcUtil as vcu
using ProbMeta

#=
VIX could probably fit a lognormal distribution.
=#
data_entropy(samples::AbstractVector; nbins=Bins.VNUM) = entropy(pmf(samples; nbins))

calc_pmf(samples::AbstractVector; nbins=Bins.VNUM) = vcu.normalize!(fit(Histogram, samples; nbins).weights ./ length(samples))
calc_pmf_kde(samples::AbstractVector; nbins=Bins.VNUM) = pdf_to_pmf(kde(samples), 1:nbins)

pdf_to_pmf(k, xs) = vcu.normalize!([pdf(k, x) for x in xs])

#=
-- Finding worst case entropy --
Get the pmf of y. Then calculate the cross entropy of each y with that pmf.
=#
# function max_cross_entropy(y_bin)
#     ky = calc_pmf_kde(y_bin)
#     [Flux.crossentropy(Flux.onehot(y), ky) for y in y_bin]
# end

end