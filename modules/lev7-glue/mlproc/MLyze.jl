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

sub_batch(batch, inds) = (;ce_compare=batch.ce_compare[inds], x=batch.x[:,inds], y=batch.y[:,inds])

function compare_batch_losses(loss_func, model, batch, inds)
    batch_loss = loss_func(model, sub_batch(batch, inds))
    batch_separate = [sub_batch(batch, i:i) for i in inds]
    batch_separate_losses = loss_func.(Ref(model), batch_separate)
    @show batch_loss mean(batch_separate_losses)
    return batch_loss, batch_separate_losses
end

end