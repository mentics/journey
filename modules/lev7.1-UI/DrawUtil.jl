module DrawUtil
using GLMakie, GLFW
using SH, Bins

export draw, draw!, newFig, ticksCentered, updateLegend, closeWin
export drawDots, drawDots!

closeWin() = GLMakie.destroy!(GLMakie.global_gl_screen())

function newFig(f, (xticks, yticks), showLegend=true, newWin=false)
    fig = Figure(resolution = (1200, 1000))
    ax = Axis(fig[1,1])
    ax.xticks = xticks
    ax.yticks = yticks
    ax.xtickformat = xs -> [string(rnd(x, 0.5)) for x in xs]

    f(fig, ax)

    Main.save[:ax] = ax
    showLegend && axislegend(ax)
    DataInspector(fig)
    if newWin
        display(GLMakie.Screen(), ax)
    else
        display(fig)
    end
    # TODO: can try more screens: display(GLMakie.Screen(), figure_or_scene).
    # from https://makie.juliaplots.org/v0.17.8/documentation/backends/glmakie/index.html
    GLFW.SetWindowPos(GLMakie.gl_screens[1], 140, 80)
    # Sometimes it wasn't popping up on top, so this is to force it to do so
    GLFW.SetWindowAttrib(GLMakie.gl_screens[1], GLFW.FLOATING, 1)
    GLFW.SetWindowAttrib(GLMakie.gl_screens[1], GLFW.FLOATING, 0)
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
    closeWin()

    if hasproperty(Main, :save) && haskey(Main.save, :drawExtentHalf)
        extentHalf = Main.save[:drawExtentHalf]
        vals = getVals(prob)[Bins.nearest(1.0-extentHalf):Bins.nearest(1.0+extentHalf)]
    else
        extentHalf = nothing
        vals = getVals(prob)
    end
    xs = Bins.xs(extentHalf)

    colors = (GLMakie.RGBA(0.5, 0.5, 1.0, 0.5), GLMakie.RGBA(0.0, 0.5, 0.5, 0.5), GLMakie.RGBA(0.5, 0.5, 0.5, 0.5))
    p = barplot!(ax, getCenter(prob) .* xs, scale * 100.0 .* vals; color=colors[colorIndex], gap=0.0, inspectable=false)
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
    # set_window_config!(;
    #     # renderloop = renderloop,
    #     vsync = false,
    #     # framerate = 30.0,
    #     float = false,
    #     pause_rendering = false,
    #     focus_on_show = true,
    #     decorated = true,
    #     title = "Journey"
    # )
end

function SH.draw()
    closeWin()
    fig = lines([(0.0, 0.0)])
    DataInspector(fig.figure)
    return fig
end

function SH.draw(f::Function, xs)
    closeWin()
    fig = lines(xs, f.(xs))
    DataInspector(fig.figure)
    return fig
end

function SH.draw(xs, ys)
    closeWin()
    fig = lines(xs, ys)
    DataInspector(fig.figure)
    return fig
end

function SH.draw(vals)
    closeWin()
    fig = lines(vals)
    DataInspector(fig.figure)
    return fig
end

function SH.draw!(vals)
    lines!(vals)
end

function drawDots(vals; kws...)
    closeWin()
    fig = scatter(vals; kws...)
    DataInspector(fig.figure)
    return fig
end

function drawDots!(vals; kws...)
    fig = scatter!(vals; kws...)
    return fig
end
#endregion

end