module Explore
using Dates, ThreadPools
import Makie
using BaseTypes, LegTypes
import Keepers:Keeper
import SH, DateUtil, Calendars
import CollUtil:sortuple
import Positions
import VectorCalcUtil
import DataFiles as dat
using DataFrames, StatsBase
using ProbMultiKde
import ProbMultiKde as pmk
import ThreadUtil

const ProbLup = Dict{Date,pmk.KdeProb}()

# const CONFIG3 = Ref((;
#     adjustprices = C(-0.0),
#     kelprobadjust = 0.0
# ))

config() = (;
    adjustprices = C(-0.02),
    kelprobadjust = 0.2
)

#region LookForBestKelly
using SmallTypes, LegMetaTypes
using Markets, Expirations, Chains
import Kelly, ChainUtil, Between, ProbUtil

function prob_test(xpir=expir(1), kde=pmk.get_kde(now(UTC)))
    prob = pmk.makeprob(kde, market().curp, now(UTC), dat.market_close(xpir), Chains.chain(xpir).chain)
    return kde, prob
end

struct Result{T}
    r::T
end

# Base.isless(a::Result{T}, b::Result{T}) where T = a.r.evrate < b.r.evrate
Base.isless(a::Result, b::Result) = a.r.evrate < b.r.evrate

Base.vec(keeper::Keeper{Result}) = map(x -> x.r, keeper.store)

run(inc::Integer; kws...) = run([inc]; kws...)
function run(incs=1:3; skipto=nothing, kws...)
    global kcalckel = nothing
    global kmakeleg = nothing
    global krun = nothing
    global kcheck = nothing
    global kctx = nothing

    # kde = use_kde()
    # xpirs = Expirations.xpirsinrange(1, 4)
    xpirs = Expirations.expirs()[1:2]
    # incs = collect(incsin)
    if isnothing(skipto)
        res = findkel(make_ctx_now(), xpirs, incs; kws...) do xpirts, curp
            return ChainUtil.oqssEntry(chain(xpirts).chain, curp; legsCheck=Positions.positions(), minshort=0.05, minlong=0.05)
        end
        # global krun = res
    else
        res = skipto
    end

    if isempty(res)
        println("No results")
        return
    end

    r1 = res[1]
    lms = r1.lms
    target = Pricing.price(Action.open, lms)
    bdaysout = DateUtil.bdays(today(),SH.getExpir(lms))
    println("evrate:$(r1.evrate) probprofit:$(r1.probprofit) min:$(r1.neto) target:$(target) risk:$(r1.risk) daysout:$(bdaysout)");
    drawres(lms)

    # DrawUtil.newfig()
    # evrate_max, i_max = findmax(eachindex(res)) do i
    #     num = incs[i]
    #     rs = res[i]
    #     !isempty(rs) || return 0.0
    #     r1 = rs[1]
    #     lms = r1.lms
    #     target = Pricing.price(Action.open, lms)
    #     bdaysout = DateUtil.bdays(today(),SH.getExpir(lms))
    #     println("$(num): evrate:$(r1.evrate) probprofit:$(r1.probprofit) min:$(r1.neto) target:$(target) risk:$(r1.risk) daysout:$(bdaysout)");
    #     DrawUtil.draw!(:lines, SH.toDraw(lms); label="$(i)-pnl")
    #     DrawUtil.drawprob!(ProbLup[SH.getExpir(lms)]; label="$(i)-prob")
    #     return r1.evrate
    # end

    # if evrate_max > 0.0
    #     r = res[i_max]
    #     r1 = r[1]
    #     lms = r1.lms
    #     println("#$(incs[i_max]) has best")
    #     DrawUtil.updateLegend()
    #     return res, r1, lms
    # else
    #     println("No results")
    #     return
    # end
    return r1
end

function drawres(lms)
    prob = ProbLup[SH.getExpir(lms)]
    numlegs = length(lms)
    display(DrawUtil.newfig())
    DrawUtil.draw!(:lines, SH.toDraw(lms); label="$(numlegs)-pnl")
    DrawUtil.drawprob!(prob; label="$(numlegs)-prob")
    DrawUtil.updateLegend()
end

import Trading
function opentrade(r; minextra=P(0.01))
    # TODO: warn if curp has changed much
    println("Opening trade: neto:$(r.neto)")
    Trading.open_trade(market(), r.lms, r.neto; pre=false, minextra);
end

function make_ctx_now()
    mkt = market()
    ts = mkt.tsMarket
    curp = mkt.curp
    kde = pmk.get_kde(ts)
    (;ts, date=Date(ts), curp, kde)
end
make_ctx(ts, kde, curp) = (;ts, date=Date(ts), kde, curp)

findkel(f_xpir_to_oqss, ctx, xpirs::Coll{Date}, incs; keep=100) = findkel(f_xpir_to_oqss, ctx, Calendars.getMarketClose.(xpirs), incs)
function findkel(f_xpir_to_oqss, ctx, xpirtss::Coll{DateTime}, incs; keep=100, use_problup=true)
    use_problup && empty!(ProbLup)
    # println("Finding kel for curp: $(ctx.curp)")
    ress = [Keeper{Result}(keep) for _ in 1:Threads.nthreads()]
    kelly_buffer = [Kelly.make_buf() for _ in 1:Threads.nthreads()]
    for xpirts in xpirtss
        xpir = Date(xpirts)
        # println("Searching xpir: $(xpir)")
        oqss = f_xpir_to_oqss(xpirts, ctx.curp)
        # println("num calls $(length(oqss.call.long))")
        # println("num puts $(length(oqss.put.long))")
        # TODO: messy concating these after they were just separated
        oqs = vcat(oqss.call.long, oqss.put.long)
        prob = pmk.makeprob(ctx.kde, ctx.curp, ctx.ts, xpirts, oqs)
        riskrat = calcriskrat(ctx.date, xpir)
        ctx2 = (;ctx.ts, ctx.date, ctx.curp, prob, riskrat, xpir, kelly_buffer)
        use_problup && ( ProbLup[xpir] = prob )
        1 in incs && findkel1!(ress, ctx2, oqss)
        2 in incs && findkel2!(ress, ctx2, oqss)
        3 in incs && findkel3!(ress, ctx2, oqss)
    end
    # println("Finished searching")
    res = merge(ress, keep)
    return vec(res)
end

calcevrate(evret, xpir, date) = evret * DateUtil.timult(date, xpir)

# calcrate(r, date=today()) = DateUtil.calcRate(date, r.xpir, r.kel * r.ev, r.risk)
# calcret(r, date=today()) = r.kel * calcrate(r, date)

function make_leg(oq, side)
    # if SH.getBid(oq) > SH.getAsk(oq)
    #     @show oq side
    #     global kmakeleg=(;adjust=CONFIG10[].adjustprices, oq, side)
    #     error("stop")
    # end
    lm = LegMetaOpen(oq, side; adjustprices=config().adjustprices)
    if SH.getBid(lm) == 0
        global kmakeleg=(;adjust=config().adjustprices, oq, side, lm)
        error("bid 0")
    end
    return lm
end

function findkel1!(ress, ctx, oqss)
    Threads.@threads for call in oqss.call.long
        check!(ress, ctx, (make_leg(call, Side.long),))
    end
    Threads.@threads for put in oqss.put.long
        check!(ress, ctx, (make_leg(put, Side.long),))
    end
end

import GenCands
function findkel2!(ress, ctx, oqss)
    GenCands.paraSpreads(oqss.call, 10, (_...) -> true) do thid, lms
        check!(ress, ctx, lms)
    end
    GenCands.paraSpreads(oqss.put, 10, (_...) -> true) do thid, lms
        check!(ress, ctx, lms)
    end
end

function findkel3!(ress, ctx, oqss)
    count = zeros(Int, Threads.nthreads())
    finish = GenCands.paraSpreads(oqss.call, 4, (_...) -> true) do thid, lms
        for oq in oqss.call.long
            lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
            check!(ress, ctx, lms2)
        end
        yield()
        count[thid] += length(oqss.call.long)
        for oq in oqss.put.long
            lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
            check!(ress, ctx, lms2)
        end
        count[thid] += length(oqss.put.long)
        yield()
        return true
    end
    finish || return
    GenCands.paraSpreads(oqss.put, 4, (_...) -> true) do thid, lms
        for oq in oqss.call.long
            lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
            check!(ress, ctx, lms2)
        end
        count[thid] += length(oqss.call.long)
        yield()
        for oq in oqss.put.long
            lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
            check!(ress, ctx, lms2)
        end
        count[thid] += length(oqss.put.long)
        yield()
        return true
    end
    # println("Searched $(sum(count)) permutations")
end

# function testcheck(res, ctx, lms2)
#     for i in 1:10000
#         Explore.check!(res, ctx, lms2)
#     end
# end

function check!(ress, ctx, lms)
    neto = Pricing.price(Action.open, lms)
    if neto < 0.07
        return true
    end
    segs, netos = Between.toSegmentsN(lms)
    try
        LL.canprofit(segs) || return true
        risk = ctx.riskrat * max(Pricing.calcMarg(ctx.prob.center, segs))
        if risk < 0.5
            netos = P.(SH.getBid.(lms))
            segs = LL.toSegments(lms, netos)
            risk = ctx.riskrat * max(Pricing.calcMarg(ctx.prob.center, segs))
            LL.canprofit(segs) || return true
            if risk < 0.1
                global kcheck = (;ctx, lms, segs, netos)
                # println("Skipping risk:$(risk) too low")
                return true
                # @show risk segs
                # error("Risk too low")
            end
        end
        thid = Threads.threadid()
        kel, evret, ev = calckel(ctx.kelly_buffer[thid], ctx.prob, risk, segs)
        if isfinite(kel) && kel >= 0.28
            xpir = ctx.xpir
            evrate = calcevrate(evret, xpir, ctx.date)
            probprofit = Between.calcprobprofit(ctx.prob, segs)
            # TODO: filter out ones that have a lot of outcome too close to zero
            if probprofit >= 0.98
                push!(ress[thid], Result((;ev, kel, evret, evrate, risk, xpir, lms, probprofit, netos, neto=sum(netos))))
            end
        end
    catch e
        global kcheck = (;ctx, lms, segs, netos)
        rethrow(e)
    end
    return true
end
#endregion LookForBestKelly

#region Kelly
riskfreerate() = 0.04
calcriskrat(from, to) = 1 + riskfreerate() * DateUtil.riskyears(from, to)

using ProbTypes
import Kelly, Pricing, LinesLeg as LL
# function calcKel1(ts::DateTime, curp::Real, lms)
#     xpir = getExpir(lms)
#     Kelly.calcKel(makeprob(ts, xpir, curp), calcriskrat(Date(ts), xpir), lms)
# end

# calckel(lms) = calckel(ProbLup[SH.getExpir(lms)], lms)
# calckel(prob, lms) = calckel(prob, calcriskrat(today(), SH.getExpir(lms)), lms)

function calckel(buf, prob, risk::Real, segs)
    probadjust = config().kelprobadjust
    try
        return Kelly.calckel(buf, prob, risk, segs, probadjust)
    catch e
        global kcalckel = (;prob, risk, segs, probadjust)
        rethrow(e)
    end
end
#endregion Kelly

#region kde
# import Distributions, KernelDensityEstimate, DrawUtil
# function kdedata()
#     nspy = Distributions.Normal(1.0, 0.1)
#     numsamples = 10000
#     maxdur = 200
#     basevix = 10
#     vixspread = 2
#     return [(dur = 1 + i % maxdur ; spy = log(rand(nspy)) ; [spy*dur, basevix + vixspread*spy + rand() - 0.5, dur]) for i in 0:numsamples]
# end
# function testkde()
#     bws = [0.1, 0.1, 1.01]
#     data = stack(kdedata())
#     kdem1 = KernelDensityEstimate.kde!(data, bws)
#     range1 = -0.49:0.01:0.49
#     vals = map(x -> x[1], [kdem1(reshape([x, basevix, maxdur / 2], 3, 1)) for x in range1])
#     display(DrawUtil.draw(:scatter, range1, vals))
#     for k in 0.5:0.1:1.5
#         sleep(0.5)
#         vals = map(x -> x[1], [kdem1(reshape([x, basevix, k * maxdur / 2], 3, 1)) for x in range1])
#         DrawUtil.draw!(:scatter, range1, vals)
#     end
# end

# using MultiKDE, Distributions, Random
# function multikde()
#     data = kdedata()
#     dims = [ContinuousDim(), ContinuousDim(), ContinuousDim()]
#     bws = [0.1, 0.1, 1.01]
#     kde = KDEMulti(dims, bws, data)

#     xs = -0.49:0.01:0.49
#     display(DrawUtil.draw(:scatter, [0.0], [0.0]))
#     for dur in 1.0:10.0:200.0
#         vals = [MultiKDE.pdf(kde, [x, 10.0, dur]) for x in xs]
#         DrawUtil.draw!(:scatter, xs, vals)
#         println("done: $(dur)")
#         yield()
#     end
# end

# getxmin(prob::KdeProb) = prob.src.extents[1].mn
# getxmax(prob::KdeProb) = prob.src.extents[1].mx

import DrawUtil
DrawUtil.drawprob(prob::KdeProb; kws...) = DrawUtil.drawdist!(DrawUtil.getAxis(;newFig=true), prob.center, prob.xs.xs, probdispvals(prob); kws...)
DrawUtil.drawprob!(prob::KdeProb; kws...) = DrawUtil.drawdist!(DrawUtil.getAxis(;newFig=false), prob.center, prob.xs.xs, probdispvals(prob); kws...)
probdispvals(prob::KdeProb) = ( k = 0.01 / xswidth(prob) ; prob.prob_mass .* k )
xswidth(prob::KdeProb) = prob.xs.xs[2] - prob.xs.xs[1]

# function data_from_tsx(tsx, len)
#     len_tot = length(tsx.ts)
#     inds = len > 0 ? (1:len) : ((len_tot+len+1):len_tot)
#     return [[tsx.ret[i], tsx.logvol[i], tsx.tex[i]] for i in inds]
# end

# function kde_from_tsx(tsx, len)
#     return TheKde(make_kde(data_from_tsx(tsx, len)), extrema(first, data)...)
# end

# function make_kde(data)
#     dims = [ContinuousDim(), ContinuousDim(), ContinuousDim()]
#     bws = [0.005, 0.1, 20.0]

#     # TODO: consider normalizing the inputs, then might use same bandwidths
#     # the nature of the variables are different, so need to adjust

#     # bws = nothing

#     # kde = KDEMulti(dims, bws, data)
#     # bws = MultiKDE.default_bandwidth(data) ./ 4
#     return KDEMulti(dims, bws, data)
# end

# function plotkde(; len=-100000, gridcount=10)
#     tsx = ktsx
#     kde = kde_from_tsx(tsx, len).kde

#     len_tot = length(tsx.ts)
#     inds = len > 0 ? (1:len) : ((len_tot+len):len_tot)
#     # data = [[tsx.ret[i], tsx.logvol[i], tsx.tex[i]] for i in inds]
#     rets, logvols, texs = grid3((extrema(tsx.ret[inds]), extrema(tsx.logvol[inds]), extrema(tsx.tex[inds])); num=gridcount)

#     numcoords = length(rets) * length(logvols) * length(texs)
#     xs = Vector{Float64}(undef, numcoords)
#     ys = Vector{Float64}(undef, numcoords)
#     zs = Vector{Float64}(undef, numcoords)
#     ps = Vector{Float64}(undef, numcoords)

#     i = 0
#     coord = [0., 0., 0.]
#     for x in rets, y in logvols, z in texs
#         i += 1
#         coord .= [x, y, z]
#         xs[i] = x; ys[i] = y; zs[i] = z
#         ps[i] = MultiKDE.pdf(kde, coord)
#     end
#     VectorCalcUtil.normalize!(ps)

#     display(Makie.scatter(xs, ys, zs; color=ps, markersize=5, colormap=:viridis, axis=(type=Makie.Axis3,)))
# end

# function over(extent; num=10)
#     left = first(extent)
#     right = last(extent)
#     step = (right - left) / (num - 1)
#     return [left + i * step for i in 0:num-1]
# end
# grid3(extents; num=10) = map(x -> over(x; num), extents)

# function calckde(; len=-100000, gridcount=10, area=gridcount ÷ 10)
#     count = gridcount + 2 * area
#     m = zeros(count, count, count)
#     global km = m
#     data = data_from_tsx(ktsx, len)
#     tfs = VectorCalcUtil.fit_01.(data)

#     width = 1 / (gridcount - 1)
#     gridcs = [togrid(tfs[i].v, width) for i in eachindex(tfs)]

#     for i in eachindex(tfs[1].v)
#         for x in toarea(gridcs[1][i], area), y in toarea(gridcs[2][i], area), z in toarea(gridcs[3][i], area)
#             m[x, y, z] = kernel()
#         end
#     end
# end


#region Explore
function calckel(prob, lms::Coll{<:LegLike})
    segs, _ = Between.toSegmentsN(lms)
    riskrat = calcriskrat(tss(prob)...)
    risk = riskrat * max(Pricing.calcMarg(prob.center, segs))
    calckel(Kelly.make_buf(), prob, risk, segs)
end

function drawfield(m)
    inds = reshape(CartesianIndices(m), prod(size(m)))
    ps = [length(m[ind]) for ind in inds]
    xyzs = [Tuple(i) for i in inds]
    # global kxyzs = xyzs
    plot = Makie.scatter(xyzs; color=ps, markersize=5, colormap=:viridis, axis=(type=Makie.Axis3,))
    #  camera=Makie.cam3d_cad!
    display(plot)
    # return Makie.camera(plot.figure.scene)
    scene = plot.figure.scene
    Makie.cam3d_cad!(scene)
end

function drawrandfield(kde::SimpleKde, num=100; k=0.001)
    # pts = Vector{Vector{Float64}}()
    pts = Vector{NTuple{3,Float64}}()
    denss = Vector{Float64}()
    skipped = 0
    while length(pts) < num
        txin = Tuple(rand(3))
        x = pmk.txout(kde, txin)
        # @show txin x
        dens = pmk.kde_pdf(kde, x, k)
        if dens > 100
            push!(pts, x)
            push!(denss, dens)
        else
            skipped += 1
        end
    end
    @show skipped
    @show extrema(denss)
    VectorCalcUtil.normalize!(denss)
    plot = Makie.scatter(pts; color=denss, markersize=5, colormap=:viridis, axis=(type=Makie.Axis3,))
    #  camera=Makie.cam3d_cad!
    display(plot)
    # return Makie.camera(plot.figure.scene)
    scene = plot.figure.scene
    # Makie.cam3d_cad!(scene)
    return
end
#endregion Explore

end