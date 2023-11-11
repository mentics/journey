module IndexUtil
using MaxLFSR

@inline lfsr(maxind) = MaxLFSR.LFSR(maxind)

@inline function lfsr_next(lfsr, ind)
    # Iterate until we reach a result that is within the correct range.
    while true
        ind = MaxLFSR.step(lfsr, ind)

        # Otherwise, perform a length check and exit.
        (ind <= length(lfsr)) && return ind
    end
end

end