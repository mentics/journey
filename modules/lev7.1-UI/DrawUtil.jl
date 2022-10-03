module DrawUtil
using GLMakie, GLFW
using SH, Bins

export draw, draw!, newFig, ticksCentered, updateLegend, closeWin
export drawDots, drawDots!
export du
const du = @__MODULE__

closeWin() = GLMakie.closeall()
# GLMakie.destroy!(GLMakie.current_figure().scene.current_screens[1])
# GLMakie.destroy!(GLMakie.global_gl_screen())

deffig() = Figure(;resolution = (1200, 1000))
function start()
    fig = deffig()
    ax = Axis(fig[1,1])
    display(fig)
    return fig, ax
end

function newFig(f, (xticks, yticks), showLegend=true, newWin=false)
    fig = deffig() # Figure(resolution = (1200, 1000))
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
    # and https://discourse.julialang.org/t/multiple-makie-display-windows/26295/22
    glscr = GLMakie.GLFW_WINDOWS[1]
    # resize!(glscr, )
    GLFW.SetWindowPos(glscr, 140, 80)
    # Sometimes it wasn't popping up on top, so this is to force it to do so
    GLFW.SetWindowAttrib(glscr, GLFW.FLOATING, 1)
    GLFW.SetWindowAttrib(glscr, GLFW.FLOATING, 0)
    return ax
end

function ticksCentered(sp, (xMin, xMax), (yMin, yMax))
    ytickWidth = rndUp((yMax - yMin + .0001)/10, .5)
    yMin = rndDown(yMin - .0001, ytickWidth)
    yMax = rndUp(yMax + .0001, ytickWidth)
    maxY = max(abs(yMin), abs(yMax))
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

function drawProb!(ax, prob, colorIndex, scale)
    closeWin()

    if hasproperty(Main, :save) && haskey(Main.save, :drawExtentHalf)
        extentHalf = Main.save[:drawExtentHalf]
        vals = getVals(prob)[Bins.nearest(1.0-extentHalf):Bins.nearest(1.0+extentHalf)]
    else
        extentHalf = nothing
        vals = getVals(prob)
    end
    xs = Bins.xs(extentHalf)

    center = getCenter(prob)
    colors = (GLMakie.RGBA(0.5, 0.5, 1.0, 0.5), GLMakie.RGBA(0.0, 0.5, 0.5, 0.5), GLMakie.RGBA(0.5, 0.5, 0.5, 0.5))
    p = barplot!(ax, center .* xs, scale * 100.0 .* vals; gap=0.0, width=center * Bins.width(), color=colors[colorIndex], inspectable=false)
    Main.save[:p] = p
    p.inspectable[] = false
    # display(p)
    return p
end

function drawProb(center, vals; kws...) #, colorIndex=1)
    # colors = (GLMakie.RGBA(0.5, 0.5, 1.0, 0.5), GLMakie.RGBA(0.0, 0.5, 0.5, 0.5), GLMakie.RGBA(0.5, 0.5, 0.5, 0.5))
    p = barplot(center .* Bins.xs(), .01 * vals ./ Bins.width(); gap=0.0, width=center * Bins.width(), inspectable=false, kws...) # , color=colors[colorIndex])
    vlines!(center)
    return p
end

function drawProb!(center, vals; kws...)
    # colors = (GLMakie.RGBA(0.5, 0.5, 1.0, 0.5), GLMakie.RGBA(0.0, 0.5, 0.5, 0.5), GLMakie.RGBA(0.5, 0.5, 0.5, 0.5))
    p = barplot!(center .* Bins.xs(), .01 * vals ./ Bins.width(); gap=0.0, width=center * Bins.width(), inspectable=false, kws...) #, color=colors[colorIndex])
    vlines!(center)
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

function SH.draw(f::Function, xs; kws...)
    closeWin()
    fig = lines(xs, f.(xs); kws...)
    DataInspector(fig.figure)
    return fig
end

function SH.draw(xs, ys; kws...)
    closeWin()
    fig = lines(xs, ys; kws...)
    DataInspector(fig.figure)
    return fig
end

function SH.draw(vals; kws...)
    # closeWin()
    fig = lines(vals; kws...)
    DataInspector(fig.figure)
    return fig
end

function SH.draw!(vals; kws...)
    lines!(vals; kws...)
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

export drawBars, drawBars!
function drawBars(args...; kws...)
    fig = barplot(args...; kws...)
    return fig
end
function drawBars!(args...; kws...)
    fig = barplot!(args...; kws...)
    return fig
end
#endregion

end