module Smiles
using Dates, GLMakie
using BaseTypes
using SH, DateUtil, CollUtil, OptionUtil
using Calendars, Backtests
import SimpleStore as SS

function run(maxSeconds=1) # ::Nothing
    from = Date(2022,1,1)
    to = Date(2022,9,30)
    # global ntBids = Vector{NamedTuple}()
    # global ntAsks = Vector{NamedTuple}()
    global ntBids = Vector{Vector}()
    global ntAsks = Vector{Vector}()
    SS.run(from, to; maxSeconds) do tim, chain
        curp = F(chain.under.under)
        for xpir in chain.xpirs
            tex = calcTex(tim.ts, xpir)
            tex < Calendars.texPerYear() || continue
            vty = calcHistVols(tim.date)
            # dur = calcDur(tim.date, xpir)
            base = (;tex, vty...)
            oqs = chain.xsoqs[xpir].put
            for oq in oqs
                dist = (F(getStrike(oq)) / curp) - 1
                if abs(dist) / curp > 0.05
                    continue
                end
                b, a, mid = OptionUtil.calcExtrin(oq, curp)
                # push!(ntBids, (;dist, xtrin=F(b), base...))
                # push!(ntAsks, (;dist, xtrin=F(a), base...))
                push!(ntBids, [dist, F(b), base...])
                push!(ntAsks, [dist, F(a), base...])
            end
        end
    end
    # global bids = ntvFromVnt(ntBids)
    # global asks = ntvFromVnt(ntAsks)
    return
end

function draw()
    display(scatter(bids...; color=:green))
    scatter!(asks...; color=:red)
end

using HistData
function calcHistVols(date)
    to = date - Day(1)
    periods = (30, 90) # (15, 30, 60, 120)
    froms = (bdaysBefore(to, b) for b in periods)
    vols = (calcHistVol(from, to) for from in froms)
    pairs = ((p, v) -> Symbol("vty$(p)") => v).(periods, vols)
    return NamedTuple(pairs)
end

using IterTools
# TODO: subtract out trend?
const TPerYear = sqrt(252.0)
function calcHistVol(from, to)
    data = dataDaily(from, to)
    σ = std(map(xs -> log(xs[1].close/xs[2].close), IterTools.partition(data, 2, 1)))
    return σ * TPerYear
end

using MultiKDE
using Distributions, Random
function calcKde()
    # fields = fieldnames(first(ntBids))
    # ndims = length(fields)
    ndims = length(first(ntBids))
    dims = fill(ContinuousDim(), ndims)
    bw = [0.01, 0.04, 4.0, .08, .08] #, .08, .08, .08]
    global kde = KDEMulti(dims, bw, ntBids)
    return kde
end

function drawPrep()
    data = ntBids
    toshow = data # filter(data)
    vtys = (;vty15 = 0.16297050231935076, vty30 = 0.17708380732070528, vty60 = 0.137917459292731, vty120 = 0.1280144011951026)
    tex = 6.0
    # granularity_2d = 40
    # dist_range = LinRange(extrema(x -> x.dist, data)..., granularity_2d)
    # xtrin_range = LinRange(extrema(x -> x.xtrin, data)..., granularity_2d)
    # x_grid = [(dist, xtrin) for dist in dist_range for xtrin in xtrin_range]
    # y_grid = [MultiKDE.pdf(kde, [x..., tex, vtys...]) for x in x_grid]
    # global p = surface([x[1] for x in x_grid], [x[2] for x in x_grid], y_grid; axis=(type=Axis3, xlabel="Strike", ylabel="Xtrins", zlabel="Prob"))

    global dist_range = LinRange(extrema(x -> x[1], toshow)..., 40)
    global xtrin_range = LinRange(extrema(x -> abs(x[2]) < 2.0 ? x[2] : 0.0, toshow)..., 100)
    global x_grid = [(dist, xtrin) for dist in dist_range for xtrin in xtrin_range]
    global y_grid = [MultiKDE.pdf(kde, [x..., tex, vtys...]) for x in x_grid]
end

function draw()
    global p = surface([x[1] for x in x_grid], [x[2] for x in x_grid], y_grid; axis=(type=Axis3, xlabel="Strike", ylabel="Xtrins", zlabel="Prob"))
end


function drawKde2()
    dur = 7.0

    toshow = filter(x -> x.dur == dur, ntBids)[1:100]

    observations = [[b...] for b in toshow]
    observations_x1 = [_obs[1] for _obs in observations]
    observations_x2 = [_obs[2] for _obs in observations]
    observations_x3 = [_obs[3] for _obs in observations]
    granularity_2d = 20
    global x1_range = LinRange(minimum(observations_x1), maximum(observations_x1), granularity_2d)
    global x2_range = LinRange(minimum(observations_x2), maximum(observations_x2), granularity_2d)
    global x3_range = LinRange(minimum(observations_x3), maximum(observations_x3), granularity_2d)
    # global x_grid = [[_x1, _x2, _x3] for _x1 in x1_range for _x2 in x2_range for _x3 in x3_range]
    global x_grid = [[_x2, _x3] for _x2 in x2_range for _x3 in x3_range]

    y_grid = []
    for bw in bws
        y = [MultiKDE.pdf(kde, [dur, x...]) for x in x_grid]
        push!(y_grid, y)
    end

    global p = surface([x[1] for x in x_grid], [x[2] for x in x_grid], y_grid[3]; axis=(type=Axis3, xlabel="Strike", ylabel="Xtrins", zlabel="Prob"))
    # scal = 200
    # scales = -scal/-(extrema(x2_range)...), -scal/-(extrema(x3_range)...), -scal/-(extrema(y_grid[1])...)
    # @show scales
    # GLMakie.scale!(p.figure.scene, scales...)
    display(p)

    # surface!([_x[2] for _x in x_grid], [_x[3] for _x in x_grid], y_grid[2])
    # surface!([_x[2] for _x in x_grid], [_x[3] for _x in x_grid], y_grid[3])

    # x_grid = [[_x1, _x2, _x3] for _x1 in x1_range for _x2 in x2_range for _x3 in x3_range]
    # global y_mat = []
    # for bw in bws
    #     kde = KDEMulti(dims, bw, observations)
    #     # y = [MultiKDE.pdf(kde, _x) for _x in x_grid]
    #     y = [MultiKDE.pdf(kde, [x1, x2, x3]) for x1 in x1_range, x2 in x2_range, x3 in x3_range]
    #     push!(y_mat, y)
    # end

    # # Plot
    # highest = maximum([maximum(y) for y in y_mat])
    # scatter([_x[1] for _x in x_grid], [_x[2] for _x in x_grid], y_mat, label=[bw[1] for bw in bws][:, :]', size=(900, 450), legend=:outertopright)
    # scatter!(observations_x1, observations_x2, [highest for _ in 1:length(observations)], seriestype=:scatter, label="observations")
end

# function xtrin(m, dur, dist)
# end

calcDur(from, to) = bdays(from, to) + 1

end