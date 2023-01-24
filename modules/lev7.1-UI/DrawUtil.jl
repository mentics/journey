module DrawUtil
import Makie:Makie, Figure, Axis, DataInspector, axislegend, Axis, AxisPlot
import GLMakie
# import GLFW
using SH, Bins

#region Public
export draw

draw!(type::Symbol, args...; kws...)::Axis = draw(type, args...; newFig=false, kws...)
function draw(type::Symbol, args...; kws...)::Axis
    f = getproperty(Makie, Symbol(string(type) * '!'))
    ax = getAxis(; kws...)
    f(ax, args...; kws...)
    return ax
end
#endregion

function prob!(ax, prob, colorIndex, scale)
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
function prob!(center, vals; kws...)
    fig = getFig(;newFig=false)
    # ax = Axis(fig[1,1])
    # colors = (GLMakie.RGBA(0.5, 0.5, 1.0, 0.5), GLMakie.RGBA(0.0, 0.5, 0.5, 0.5), GLMakie.RGBA(0.5, 0.5, 0.5, 0.5))
    p = barplot!(fig, center .* Bins.xs(), .01 * vals ./ Bins.width(); gap=0.0, width=center * Bins.width(), inspectable=false, kws...) #, color=colors[colorIndex])
    vlines!(center)
    return p
end

function prob(center, vals; kws...) #, colorIndex=1)
    fig = getFig()
    # colors = (GLMakie.RGBA(0.5, 0.5, 1.0, 0.5), GLMakie.RGBA(0.0, 0.5, 0.5, 0.5), GLMakie.RGBA(0.5, 0.5, 0.5, 0.5))
    p = barplot(fig, center .* Bins.xs(), .01 * vals ./ Bins.width(); gap=0.0, width=center * Bins.width(), inspectable=false, kws...) # , color=colors[colorIndex])
    vlines!(center)
    return p
end

function check()::FigureAxisPlot
    return GLMakie.lines(getFig(), [(0.0, 0.0)])
end

using Dates
using PlotUtils: optimize_ticks
# function setupDates(dates)
#     dateticks = optimize_ticks(dates[1], dates[end])[1]
#     fig = current_figure()
#     ax1 = Axis(fig[1,1])
#     ax1.xticks[] = (datetime2rata.(dateticks) , Dates.format.(dateticks, "mm/dd/yyyy"));
#     return fig
# end
function drawDates(dates, values; func=lines!, kws...)
    dateticks = optimize_ticks(dates[1], dates[end])[1]
    fig = current_figure() # Figure()
    ax1 = Axis(fig[1,1])
    plt = func(ax1, datetime2rata.(dates), values; kws...)
    ax1.xticks[] = (datetime2rata.(dateticks) , Dates.format.(dateticks, "mm/dd/yyyy"));
    # plt = lines!(ax1, datetime2unix.(dates), values; kws...)
    # ax1.xticks[] = (datetime2unix.(dateticks), Dates.format.(dateticks, "mm/dd/yyyy"));
    return fig
end
function drawDates(tups; kws...)
    drawDates([x[1] for x in tups], [x[2] for x in tups]; kws...)
end

function drawDates!(dates, values; kws...)
    # dateticks = optimize_ticks(dates[1], dates[end])[1]

    # fig = Figure()
    # ax1 = Axis(fig[1,1])
    plt = lines!(datetime2rata.(dates), values; kws...)
    # ax1.xticks[] = (datetime2rata.(dateticks) , Dates.format.(dateticks, "mm/dd/yyyy"));
    # plt = lines!(ax1, datetime2unix.(dates), values; kws...)
    # ax1.xticks[] = (datetime2unix.(dateticks), Dates.format.(dateticks, "mm/dd/yyyy"));
    # return fig
end
function drawDates!(tups; kws...)
    drawDates!([x[1] for x in tups], [x[2] for x in tups]; kws...)
end
#endregion

#region Local
function __init__()
    if ccall(:jl_generating_output, Cint, ()) != 1
        println("Loading DrawUtil")
        GLMakie.set_theme!(GLMakie.theme_black())
        GLMakie.update_theme!(fontsize=12)
        GLMakie.set_window_config!(;
            vsync = false,
            float = false,
            focus_on_show = true,
            decorated = true,
            render_on_demand = true,
            title = "Journey"
        )
    end
end

makeFig() = Figure(;resolution = (1200, 1000))

getAxis(args...; kws...)::Axis = Axis(getFig(args...; kws...)[1,1])

function getFig(; newFig=true, newWin=false, showLegend=false)::Figure
    fig = newFig ? makeFig() : current_figure()
    if newWin
        display(GLMakie.Screen(), fig)
    end
    if showLegend
        ax = Axis(fig[1,1])
        axislegend(ax)
    end
    DataInspector(fig; textcolor=:blue)
    display(fig)
    return fig
end

closeWin() = GLMakie.closeall()
#endregion

#region Util
# rnd(x, m, mode=RoundNearest) = round(x / m, mode) * m
# rndUp(x, m) = rnd(x, m, RoundUp)
# rndDown(x, m) = rnd(x, m, RoundDown)
#endregion

#region Old
# GLMakie.destroy!(GLMakie.current_figure().scene.current_screens[1])
# GLMakie.destroy!(GLMakie.global_gl_screen())

# function start()
#     fig = newFig()
#     ax = Axis(fig[1,1])
#     display(fig)
#     return fig, ax
# end

# function newFig(f, (xticks, yticks), showLegend=true, newWin=false)
#     fig = newFig()
#     ax = Axis(fig[1,1])
#     ax.xticks = xticks
#     ax.yticks = yticks
#     ax.xtickformat = xs -> [string(rnd(x, 0.5)) for x in xs]

#     f(fig, ax)

#     # Main.save[:ax] = ax
#     showLegend && axislegend(ax)
#     DataInspector(fig; textcolor=:blue)
#     disp = nothing
#     if newWin
#         disp = display(GLMakie.Screen(), ax)
#     else
#         disp = display(fig)
#     end
#     # TODO: can try more screens: display(GLMakie.Screen(), figure_or_scene).
#     # from https://makie.juliaplots.org/v0.17.8/documentation/backends/glmakie/index.html
#     # and https://discourse.julialang.org/t/multiple-makie-display-windows/26295/22
#     # glscr = GLMakie.GLFW_WINDOWS[1]
#     glscr = GLMakie.to_native(disp)
#     # resize!(glscr, )
#     GLFW.SetWindowPos(glscr, 140, 80)
#     # Sometimes it wasn't popping up on top, so this is to force it to do so
#     GLFW.SetWindowAttrib(glscr, GLFW.FLOATING, 1)
#     GLFW.SetWindowAttrib(glscr, GLFW.FLOATING, 0)
#     return ax
# end

function ticksCentered(sp, (xMin, xMax), (yMin, yMax))
    # ytickWidth = rndUp((yMax - yMin + .0001)/10, .5)
    ytickWidth = .5
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

# function updateLegend()
#     foreach(filter(x -> x isa Legend, current_figure().content)) do x
#         try
#             delete!(x)
#         catch
#             #ignore
#         end
#     end

#     axislegend()
# end
#endregion
end