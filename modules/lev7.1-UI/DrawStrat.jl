module DrawStrat
using GLMakie
using DrawUtil
using SH, BaseTypes, StratTypes, Bins, RetTypes

export drawRet, drawRet!

function drawRet(r::Ret, probs, cp::Real, label::AStr)
    sp = r.center
    xs = sp .* Bins.xs()
    vals = getVals(r)

    xrange = (xs[1] - 0.5 * sp * Bins.width(), xs[end] + 0.5 * sp * Bins.width())
    yMin, yMax = extrema(vals)
    return newFig(ticksCentered(sp, xrange, (yMin, yMax))) do fig, ax
        if !isnothing(probs)
            drawProb(ax, probs[1], 1, (yMax - yMin)/10.0)
            drawProb(ax, probs[2], 2, (yMax - yMin)/10.0)
        end
        hlines!(ax, 0.0).inspectable[] = false
        vlines!(ax, sp; label="sp").inspectable[] = false
        vlines!(ax, cp; label="cp").inspectable[] = false
        lines!(ax, xs, vals; label)
        # foreach(enumerate(probs)) do (i, prob); drawProb(ax, prob, i, (yMax - yMin)/10.0) end
    end
end

function drawRet!(r::Ret, label::AStr)
    sp = r.center
    xs = sp .* Bins.xs()
    vals = getVals(r)
    p = lines!(xs, vals; label)
    display(p)
    updateLegend()
    return p
end

#region Local
#endregion

end