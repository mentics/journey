module MLyze
using StatsBase, Distributions, KernelDensity, Flux
import VectorCalcUtil as vcu
using ProbMeta

#=
VIX could probably fit a lognormal distribution.
=#
data_entropy(samples::AbstractVector; nbins=Bins.VNUM) = entropy(pmf(samples; nbins))


#=
Using this during training is probably cheating unless you're limiting only events prior to the ts you use this for.
=#
calc_pmf(samples::AbstractVector; nbins=Bins.VNUM) = vcu.normalize!(fit(Histogram, samples; nbins).weights ./ length(samples))
calc_pmf_kde(samples::AbstractVector; nbins=Bins.VNUM) = pdf_to_pmf(kde(samples), 1:nbins)

pdf_to_pmf(k, xs) = vcu.normalize!([pdf(k, x) for x in xs])

#=
-- Finding max? or mean? entropy --
Get the pmf of y. Then calculate the cross entropy of each y with that pmf.
=#
function calc_ce_mean(y_bins)
    ky = calc_pmf_kde(y_bins)
    [Flux.crossentropy(Flux.onehot(y, 1:Bins.VNUM), ky) for y in 1:Bins.VNUM]
end

sub_batch(batch, inds) = (;ce_compare=batch.ce_compare[inds], x=batch.x[:,inds], y=batch.y[:,inds])

function compare_batch_losses(loss_func, model, batch, inds)
    batch_loss = loss_func(model, sub_batch(batch, inds))
    batch_separate = [sub_batch(batch, i:i) for i in inds]
    batch_separate_losses = loss_func.(Ref(model), batch_separate)
    @show batch_loss mean(batch_separate_losses)
    return batch_loss, batch_separate_losses
end

end