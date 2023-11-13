module IndexUtil
using MaxLFSR
using CollUtil

@inline lfsr(maxind) = MaxLFSR.LFSR(maxind)

@inline function lfsr_next(lfsr, ind)
    # Iterate until we reach a result that is within the correct range.
    while true
        ind = MaxLFSR.step(lfsr, ind)

        # Otherwise, perform a length check and exit.
        (ind <= length(lfsr)) && return ind
    end
end

function cv_folds(batch_count, k_fold)
    lfsr = IndexUtil.lfsr(batch_count)
    inds_all = [IndexUtil.lfsr_next(lfsr, i) for i in 1:batch_count]

    inds_per_fold = batch_count ÷ k_fold
    leftover = rem(batch_count, k_fold)
    if leftover == 0
        leftover = k_fold
        inds_per_fold -= 1
    end
    folds = [inds_all[((i - 1)*inds_per_fold + 1):(i * inds_per_fold)] for i in 1:k_fold]

    holdout = inds_all[(end - leftover + 1):end]
    return (; folds, holdout)
end

# folds is object returned from cv_folds above
function inds_for_fold(folds, foldi)
    @assert 1 <= foldi <= size(folds.folds, 1)
    validation = folds.folds[foldi]
    train = flatvec(folds.folds[1:end .!= foldi])
    return (;train, validation, folds.holdout)
end

end