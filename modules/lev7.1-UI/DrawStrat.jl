module DrawStrat
using GLMakie
using DrawUtil
using SH, BaseTypes, StratTypes, Bins, RetTypes

export drawRet, drawRet!

# drawRet(r::Ret; probs=nothing, cp=nothing, label="", newWin=false) = drawRet(r, probs, cp, label; newWin)
function drawRet(r::Ret; probs=nothing, cp=nothing, label="", newWin=false)
    newWin || closeWin()
    sp = r.center
    xs = sp .* Bins.xs()
    vals = getVals(r)

    xrange = (xs[1] - 0.5 * sp * Bins.width(), xs[end] + 0.5 * sp * Bins.width())
    yMin, yMax = extrema(vals)
    ax = newFig(ticksCentered(sp, xrange, (yMin, yMax)), newWin) do fig, ax
        # TODO: draw on fig arg
        if !isnothing(probs)
            for (i, prob) in enumerate(probs)
                drawProb(ax, prob, i, (yMax - yMin)/10.0)
            end
        end
        hlines!(ax, 0.0).inspectable[] = false
        vlines!(ax, sp; label="sp").inspectable[] = false
        isnothing(cp) || (vlines!(ax, cp; label="cp").inspectable[] = false)
        lines!(ax, xs, vals; label)
        # foreach(enumerate(probs)) do (i, prob); drawProb(ax, prob, i, (yMax - yMin)/10.0) end
    end
end

function drawRet!(r::Ret; label::AStr="")
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