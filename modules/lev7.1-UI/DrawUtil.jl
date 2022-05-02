module DrawUtil
using GLMakie
using SH, Bins

export draw, draw!, newFig, ticksCentered, updateLegend

function newFig(f, (xticks, yticks))
    fig = Figure(resolution = (1200, 1000))
    ax = Axis(fig[1,1])
    ax.xticks = xticks
    ax.yticks = yticks
    ax.xtickformat = xs -> [string(rnd(x, 0.5)) for x in xs]

    f(fig, ax)

    axislegend(ax)
    DataInspector(fig)
    display(fig)
    return ax
end

function ticksCentered(sp, (xMin, xMax), (yMin, yMax))
    ytickWidth = rndUp((yMax - yMin)/10, .5)
    yMin = rndDown(yMin, ytickWidth)
    yMax = rndUp(yMax, ytickWidth)
    maxY = max(abs(yMin), abs(yMax))
    # @info "ticksCentered" -maxY ytickWidth maxY
    yticks = -maxY:ytickWidth:maxY
    numTicks = 9
    ticksPerSide = div(numTicks, 2)
    xtickWidth = rndDown((xMax - xMin) / numTicks, 0.5)
    xtmin = rndDown(sp - ticksPerSide * xtickWidth, .5)
    xtmax = rndUp(sp + ticksPerSide * xtickWidth, .5)
    xtleft2 = xtmin + ((ticksPerSide-1)*xtickWidth)
    xtright2 = xtmax - ((ticksPerSide-1)*xtickWidth)
    xticks = vcat(xtmin:xtickWidth:xtleft2, sp, xtright2:xtickWidth:xtmax)
    return (xticks, yticks)
end

function updateLegend()
    foreach(filter(x -> x isa Legend, current_figure().content)) do x
        try
            delete!(x)
        catch
            #ignore
        end
    end

    axislegend()
end

export drawProb
function drawProb(ax, prob, colorIndex, scale)
    colors = (GLMakie.RGBA(0.5, 0.5, 1.0, 0.5), GLMakie.RGBA(0.0, 0.5, 0.5, 0.5), GLMakie.RGBA(0.5, 0.5, 0.5, 0.5))
    p = barplot!(ax, getCenter(prob) .* Bins.xs(), scale * 100.0 .* getVals(prob); color=colors[colorIndex], gap=0.0, inspectable=false)
    p.inspectable[] = false
    display(p)
    return p
end

export rnd, rndUp, rndDown
rnd(x, m, mode=RoundNearest) = round(x / m, mode) * m
rndUp(x, m) = rnd(x, m, RoundUp)
rndDown(x, m) = rnd(x, m, RoundDown)

#region Local
function __init__()
    set_theme!(theme_black())
    update_theme!(fontsize=12)
end

function SH.draw(f::Function, xs)
    lines(xs, f.(xs))
end

function SH.draw(xs, ys)
    lines(xs, ys)
end

function SH.draw(vals)
    lines(vals)
end

function SH.draw!(vals)
    lines!(vals)
end
#endregion

end