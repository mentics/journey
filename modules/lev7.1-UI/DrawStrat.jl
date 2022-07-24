module DrawStrat
using GLMakie
using DrawUtil
using SH, BaseTypes, StratTypes, Bins, RetTypes

export drawRet, drawRet!

# drawRet(r::Ret; probs=nothing, cp=nothing, label="", newWin=false) = drawRet(r, probs, cp, label; newWin)
function drawRet(r::Ret; probs=nothing, curp=nothing, label="", newWin=false)
    newWin || closeWin()
    sp = r.center
    if hasproperty(Main, :save) && haskey(Main.save, :drawExtentHalf)
        extentHalf = Main.save[:drawExtentHalf]
        vals = getVals(r)[Bins.nearest(1.0-extentHalf):Bins.nearest(1.0+extentHalf)]
    else
        extentHalf = nothing
        vals = getVals(r)
    end
    xs = sp .* Bins.xs(extentHalf)

    xrange = (xs[1] - 0.5 * sp * Bins.width(), xs[end] + 0.5 * sp * Bins.width())
    yMin, yMax = extrema(vals)
    ax = newFig(ticksCentered(sp, xrange, (yMin, yMax)), newWin) do fig, ax
        # TODO: draw on fig arg
        if !isnothing(probs)
            for (i, prob) in enumerate(probs)
                drawProb(ax, prob, i, 1.0)#(yMax - yMin)/2)
            end
        end
        hlines!(ax, 0.0; inspectable=false)
        vlines!(ax, sp; label="sp", inspectable=false)
        isnothing(curp) || (vlines!(ax, curp; label="cp", inspectable=false))
        lines!(ax, xs, vals; label)
        # foreach(enumerate(probs)) do (i, prob); drawProb(ax, prob, i, (yMax - yMin)/10.0) end
    end
end

function drawRet!(r::Ret; label::AStr="")
    sp = r.center
    if hasproperty(Main, :save) && haskey(Main.save, :drawExtentHalf)
        extentHalf = Main.save[:drawExtentHalf]
        vals = getVals(r)[Bins.nearest(1.0-extentHalf):Bins.nearest(1.0+extentHalf)]
    else
        extentHalf = nothing
        vals = getVals(r)
    end
    xs = sp .* Bins.xs(extentHalf)
    p = lines!(xs, vals; label)
    updateLegend()
    return p
end

#region Local
#endregion

end