module Explore
using Dates
using ThreadPools
import Makie
using BaseTypes, LegTypes
import Keepers:Keeper
import SH, DateUtil
import CollUtil:sortuple
import Positions
using OutputUtil
import VectorCalcUtil
import DataFiles
using DataFrames, StatsBase

const CONFIG8 = Ref((;
    adjustprices = C(-0.01),
    kelprobadjust = 0.1
))

#region test
function test1(x, extra=nothing)
    a = 2*x
    b = if isnothing(extra)
        a
    else
        a + extra
    end
    c = sqrt(b)
    return c
end

function test2(x; extra=nothing)
    a = 2*x
    b = if isnothing(extra)
        a
    else
        a + extra
    end
    c = sqrt(b)
    return c
end

function testCalcKel()
    curp = market().curp
    oqs = filter!(isCall, Chains.chain(expir(1)).chain)
    sort!(oqs; by=oq -> abs(curp - SH.getStrike(oq)))
    lms = sortuple(SH.getStrike, LegMetaOpen(oqs[1], Side.short), LegMetaOpen(oqs[2], Side.long))
    # lms = discount.(lms, 0.1)
    DrawUtil.drawprob(kprobs[SH.getExpir(lms)]; color=Makie.RGBA(.1, .1, .5, 0.5))
    display(DrawUtil.draw!(:lines, SH.toDraw(lms)))
    return calckel(lms)
end

using LegMetaTypes, QuoteTypes
discount(lm::LegMetaOpen, r=0.75) = LegMetaOpen(SH.getLeg(lm), discount(SH.getQuote(lm), r), SH.getMeta(lm))
discount(q::Quote, r=0.75) = Quote(P(q.bid + r), P(q.ask + r))
#endregion

#region Troubleshooting
function pbstoxys(pbs)
    xs = map( pb -> (pb[3].left + pb[3].right) / 2, pbs)
    probabilities = map( pb -> pb[3].p, pbs)
    outcomes = map( pb -> pb[2], pbs)
    products = map( pb -> pb[1], pbs)
    return (;xs, probabilities, outcomes, products)
end
import GLMakie
function drawkellycalc(sym; replace=false)
    xys = pbstoxys(Kelly.kpbs)
    if replace
        GLMakie.barplot(xys.xs, xys[sym]; width=1)
    else
        GLMakie.barplot!(xys.xs, xys[sym]; width=1)
    end
end
#endregion

#region TestMonteCarlo
import LinesLeg as LL
function testmc(r; numiter=1000, curp=market().curp, prob=Between.makeprob2(r.lms, curp).prob)
    lms = r.lms
    risk = 100 * r.risk
    segs = LL.toSegments(lms, CONFIG8[].adjustprices)
    bal = 10000.0
    netsum = 0.0
    pnlratesum = 0.0
    ncsum = 0.0
    unders = randistrets(prob, numiter)
    nets = Float64[]
    rets = Float64[]
    bals = Float64[]
    maxdrawdown = 1.0
    high = 0.0
    for under in unders
        numcontracts = floor(Int, r.kel * bal / risk)
        # @show r.kel, bal, risk, bal / risk, numcontracts * risk / bal
        underret = under / curp
        net = 100 * LL.at(segs, under)
        ret = numcontracts * net
        @show underret, net, numcontracts, ret, bal+ret, ret/bal
        bal += ret
        high = max(high, bal)
        drawdown = bal / high
        maxdrawdown = min(maxdrawdown, drawdown)

        push!(nets, net)
        push!(rets, net)
        push!(bals, bal)

        ncsum += numcontracts
        netsum += net
        pnlratesum += ret / bal
    end
    netmean = netsum / numiter
    ncmean = ncsum / numiter
    pnlratemean = pnlratesum / numiter
    @show bal netmean ncmean pnlratemean maxdrawdown
    return (;nets, rets, bals)
end

import Random
function randistrets(prob, n)
    rets = [ProbUtil.xforp(prob, p) for p in 1e-6:(1.0/n):(1.0-1e-6)]
    Random.shuffle!(rets)
    return rets
end
#endregion

#region LookForBestKelly
using SmallTypes, LegMetaTypes
using Markets, Expirations, Chains
import Kelly, ChainUtil, Between, ProbUtil

struct Result{T}
    r::T
end

Base.isless(a::Result{T}, b::Result{T}) where T = a.r.evrate < b.r.evrate

Base.vec(keeper::Keeper{Result}) = map(x -> x.r, keeper.store)

run(inc::Integer; kws...) = run([inc]; kws...)
function run(incsin=1:3; skipto=nothing, kws...)
    incs = collect(incsin)
    if isnothing(skipto)
        res = map(incs) do inc
            findkel(inc; kws...)
        end
        global krun = res
    else
        res = skipto
    end

    DrawUtil.newfig()
    evrate_max, i_max = findmax(eachindex(res)) do i
        num = incs[i]
        rs = res[i]
        !isempty(rs) || return 0.0
        r1 = rs[1]
        lms = r1.lms
        target = Pricing.price(Action.open, lms)
        bdaysout = DateUtil.bdays(today(),SH.getExpir(lms))
        println("$(num): evrate:$(r1.evrate) probprofit:$(r1.probprofit) min:$(r1.neto) target:$(target) risk:$(r1.risk) daysout:$(bdaysout)");
        DrawUtil.draw!(:lines, SH.toDraw(lms); label="$(i)-pnl")
        DrawUtil.drawprob!(kprobs[SH.getExpir(lms)]; label="$(i)-prob")
        return r1.evrate
    end

    if evrate_max > 0.0
        r = res[i_max]
        r1 = r[1]
        lms = r1.lms
        println("#$(incs[i_max]) has best")
        DrawUtil.updateLegend()
        return res, r1, lms
    else
        println("No results")
        return
    end
end

import Trading
function opentrade(r; minextra=P(0.01))
    # TODO: warn if curp has changed much
    println("Opening trade: neto:$(r.neto)")
    Trading.open_trade(market(), r.lms, r.neto; pre=false, minextra);
end

function findkel(inc; keep=100)
    kde = usekde()
    mkt = market()
    curp = mkt.curp
    ts = mkt.tsMarket
    date = Date(ts)
    println("Finding kel for curp: $(curp)")
    # xpirs = Expirations.xpirsinrange(1, 4)
    xpirs = Expirations.expirs()[1:2]
    ress = [Keeper{Result}(keep) for _ in 1:Threads.nthreads()]
    # kelly_buffer = [Vector{NTuple{3,Float64}}(undef, 500) for _ in 1:Threads.nthreads()]
    # kelly_buffer = [Matrix{Float64}(undef, 3, 500) for _ in 1:Threads.nthreads()]
    kelly_buffer = [Kelly.make_buf() for _ in 1:Threads.nthreads()]
    global kprobs = Dict{Any, Any}()
    for xpir in xpirs
        println("Searching xpir: $(xpir)")
        xpirts = Calendars.getMarketClose(xpir)
        # prob = Between.makeprob(xpir, curp)
        # TODO: it's getting chain twice
        prob = makeprob(kde, curp, ts, xpirts)
        riskrat = calcriskrat(date, xpir)
        ctx = (;curp, prob, riskrat, xpir, kelly_buffer)
        kprobs[xpir] = prob
        oqss = ChainUtil.oqssEntry(chain(xpir).chain, curp; legsCheck=Positions.positions(), shortbidgt=max(CZ,-CONFIG8[].adjustprices))
        println("num calls $(length(oqss.call.long))")
        println("num puts $(length(oqss.put.long))")
        1 in inc && findkel1!(ress, ctx, oqss)
        2 in inc && findkel2!(ress, ctx, oqss)
        3 in inc && findkel3!(ress, ctx, oqss)
    end
    println("Finished searching")
    # res = vcat(ress...)
    # return ress
    res = merge(ress, keep)
    # sort!(res; rev=true, by=r->r.evrate)
    return vec(res)
end

# function filt(res)
#     filter(res) do r
#         # r.probprofit > 0.55 && r.risk <= 5.0 &&
#         DateUtil.bdays(today(), SH.getExpir(r.lms)) >= 3
#     end
# end

# function sortres!(date, res)
#     sort!(res; by=r -> calcret(r, date))
# end

calcevrate(evret, xpir, date) = evret * DateUtil.timult(date, xpir)

# calcrate(r, date=today()) = DateUtil.calcRate(date, r.xpir, r.kel * r.ev, r.risk)
# calcret(r, date=today()) = r.kel * calcrate(r, date)

function make_leg(oq, side)
    lm = LegMetaOpen(oq, side; adjustprices=CONFIG8[].adjustprices)
    if SH.getBid(lm) == 0
        global kmakeleg=(;adjust=CONFIG8[].adjustprices, oq, side, lm)
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
    println("Searching calls")
    finish = GenCands.paraSpreads(oqss.call, 4, (_...) -> true) do thid, lms
        # println("findkel3 checking spread $(SH.getStrike(lms[1])) $(SH.getStrike(lms[2]))")
        # Threads.@threads
        twith(ThreadPools.QueuePool(12, 11)) do pool
            @tthreads pool for oq in oqss.call.long
                lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
                # global kcheckargs = (res, ctx, lms2)
                # error("stop")
                check!(ress, ctx, lms2)
            end
        end
        count[thid] += length(oqss.call.long)
        twith(ThreadPools.QueuePool(12, 11)) do pool
            @tthreads pool for oq in oqss.put.long
                lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
                check!(ress, ctx, lms2)
            end
        end
        count[thid] += length(oqss.put.long)
        return true
    end
    finish || return
    println("Searching puts")
    GenCands.paraSpreads(oqss.put, 4, (_...) -> true) do thid, lms
        # Threads.@threads
        for oq in oqss.call.long
            lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
            check!(ress, ctx, lms2)
        end
        count[thid] += length(oqss.call.long)
        # Threads.@threads
        for oq in oqss.put.long
            lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
            check!(ress, ctx, lms2)
        end
        count[thid] += length(oqss.put.long)
        return true
    end
    println("Searched $(sum(count)) permutations")
end

# function testcheck(res, ctx, lms2)
#     for i in 1:10000
#         Explore.check!(res, ctx, lms2)
#     end
# end

function check!(ress, ctx, lms)
    ss = tosegsz(ctx.curp, lms)
    !isnothing(ss) || return true # println("ss was nothing")
    (;segs, segsz, netos) = ss
    try
        thid = Threads.threadid()
        kel, evret, ev, risk = calckel(ctx.kelly_buffer[thid], ctx.prob, ctx.riskrat, segs, segsz)
        # global kcalckelargs = (ctx.kelly_buffer[thid], ctx.prob, ctx.riskrat, segs, segsz)
        # error("stop")
        if isfinite(kel) && kel > 0
            # println("$(i): Long on strike $(SH.getStrike(lms[1])) -> $(kel)")
            xpir = ctx.xpir
            evrate = calcevrate(evret, xpir, today())
            probprofit = Between.calcprobprofit(ctx.prob, segsz)
            if probprofit > 0.7
                push!(ress[thid], Result((;ev, kel, evret, evrate, risk, xpir, lms, probprofit, netos, neto=sum(netos))))
            end
        end
    catch e
        global kcheck = (;ctx, lms, ss)
        rethrow(e)
    end
    return true
end
#endregion

#region Kelly
riskfreerate() = 0.04
calcriskrat(from, to) = 1 + riskfreerate() * DateUtil.riskyears(from, to)

using ProbTypes
import Kelly, Pricing, LinesLeg as LL
# function calcKel1(ts::DateTime, curp::Real, lms)
#     xpir = getExpir(lms)
#     Kelly.calcKel(makeprob(ts, xpir, curp), calcriskrat(Date(ts), xpir), lms)
# end

function tosegsz(curp, lms::Coll{<:LegLike})
    # adjustprices = CONFIG5[].adjustprices
    segs, netos = Between.toSegmentsN(lms) # , adjustprices) <- moved to lms
    LL.canprofit(segs) || return nothing
    segsz = LL.toSegmentsWithZeros(segs; extent=(0.5*curp, 1.5*curp))
    return (;segs, segsz, netos)
end

calckel(lms) = calckel(kprobs[SH.getExpir(lms)], lms)
calckel(prob, lms) = calckel(prob, calcriskrat(today(), SH.getExpir(lms)), lms)
function calckel(prob, riskrat::Real, lms::Coll{<:LegLike})
    (;segs, segsz) = tosegsz(prob.center, lms)
    calckel(Kelly.make_buf(), prob, riskrat, segs, segsz)
end

function calckel(buf, prob, riskrat::Real, segs, segsz)
    probadjust = CONFIG8[].kelprobadjust
    risk = riskrat * max(Pricing.calcMarg(prob.center, segs))
    try
        return (;Kelly.calckel(buf, prob, risk, segsz; probadjust)..., risk)
    catch e
        global kcalckel = (;prob, risk, segsz, probadjust)
        rethrow(e)
    end
end
#endregion

#region CalcOptPrices
# using Random, Statistics
# using Distributions

# function american_put_option_price(S0, K, r, sigma, T, num_simulations, num_time_steps)
#     Random.seed!(42)  # Set a seed for reproducibility
#     dt = T / num_time_steps
#     discount_factor = exp(-r * dt)

#     # Perform Monte Carlo simulations
#     option_payoffs = zeros(num_simulations, num_time_steps + 1)
#     for i in 1:num_simulations
#         S = [S0]
#         for j in 1:num_time_steps
#             z = randn()
#             S_t = S[j] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * z)
#             push!(S, S_t)
#         end
#         option_payoffs[i, :] .= max.(K .- S, 0)
#     end

#     # Backward induction to calculate option price
#     for t in (num_time_steps - 1):-1:1
#         X = Float64[]
#         Y = Float64[]
#         for i in 1:num_simulations
#             if option_payoffs[i, t] > 0
#                 continuation_value = discount_factor * option_payoffs[i, t + 1]
#                 push!(X, [1, option_payoffs[i, t]])
#                 push!(Y, continuation_value)
#             end
#         end
#         if length(X) > 0
#             coefficients = X \ Y
#             for i in 1:num_simulations
#                 if option_payoffs[i, t] > 0
#                     continuation_value = discount_factor * option_payoffs[i, t + 1]
#                     exercise_value = coefficients[1] + coefficients[2] * option_payoffs[i, t]
#                     option_payoffs[i, t] = max(exercise_value, continuation_value)
#                 end
#             end
#         end
#     end

#     option_price = mean(option_payoffs[:, 1])
#     return option_price
# end

# function runmc()
#     # Example usage
#     S0 = 100.0      # Initial stock price
#     K = 110.0       # Strike price
#     r = 0.03      # Risk-free rate
#     sigma = 0.2   # Volatility (standard deviation)
#     T = 1.0       # Time to expiration
#     num_simulations = 10000
#     num_time_steps = 100

#     option_price = american_put_option_price(S0, K, r, sigma, T, num_simulations, num_time_steps)
#     println("American Put Option Price:", option_price)
# end


# using FinancialMonteCarlo

# function runfmc(sigma)
#     S0 = 451.92;
#     K = 400.0;
#     r = 0.03;
#     T = 4.0 / 52;
#     d = 0.01;
#     # D = 90.0;

#     # Nsim = 10000;
#     # Nstep = 30;
#     Nsim = 10000 * 2;
#     Nstep = 30 * 2;

#     σ = sigma
#     # σ = 0.10711; # Worked for S0=451.92, K=450.0, T = 2/52, 2 weeks out after friday close 7/21 - 8/4 2023
#     # σ = 0.11016; # Worked for S0=451.92, K=450.0, T = 1/52, 1 weeks out after friday close 7/21 - 7/28 2023
#     # σ = 0.1059; # Worked for S0=451.92, K=450.0, T = 4/52, 4 weeks out after friday close 7/21 - 8/18 2023
#     # VIX was 13.6

#     mcConfig = MonteCarloConfiguration(Nsim, Nstep);
#     rfCurve = ZeroRate(r);

#     opt = AmericanOption(T, K, false)

#     Model = BlackScholesProcess(σ, Underlying(S0, d))

#     @show EuPrice = pricer(Model, rfCurve, mcConfig, opt);
# end
#endregion

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

function ntm4s(ts, xpir, curp, oqs, style)
    f_extrin = style == Style.call ? OptionUtil.extrin_call : OptionUtil.extrin_put
    # TODO: could use that heap thing to do this more efficiently nextrem ?
    sort!(oqs; by=oq -> abs(curp - SH.getStrike(oq)))
    oq4 = oqs[1:4]
    fntm = DataFiles.make_ntm_data(f_extrin, "", style)
    return fntm((ts,), (xpir,), curp, SH.getStrike.(oq4), SH.getBid.(oq4), SH.getAsk.(oq4))
end

import Calendars, OptionUtil
function makekdeargs(xpirts::DateTime)
    mkt = market()
    curp = mkt.curp
    ts = mkt.tsMarket
    xpirts = Calendars.getMarketClose(xpir)
    makekdeargs(curp, ts, xpirts)
end
function makekdeargs(curp, ts, xpirts::DateTime)
    chain = Chains.chain(Date(xpirts)).chain

    calltex, _, callvol, _ = ntm4s(ts, xpirts, curp, filter(isCall, chain), Style.call)
    puttex, _, putvol, _ = ntm4s(ts, xpirts, curp, filter(isPut, chain), Style.put)
    @assert calltex == puttex # TODO: remove
    tex = calltex
    logvol = log(mean((callvol, putvol)))
    return (;tex, logvol)
end

struct Extent2
    mn::Float64
    mx::Float64
end

struct Xs2
    xs::Vector{Float64}
    numbins::Int
    binwidth::Float64
end

struct Transform3
    ad::NTuple{3, Float64}
    scale::NTuple{3, Float64}
end

struct KDENew7
    m::Array{Vector{NTuple{3,Float64}}, 3}
    txs::Transform3
    width::Float64
    extents::NTuple{3, Extent2}
    xs::Ref{Xs2}
end

struct ProbKde7{S}
    src::S
    ts::DateTime
    center::Float64
    pdf::Vector{Float64}
    cdf::Vector{Float64}
    xs::Xs2
end

# struct TheKde
#     kde::KDEMulti{Float64}
#     xmin::Float64
#     xmax::Float64
# end

function ProbUtil.cdf(prob::ProbKde7, x)
    xr = x / prob.center
    # len = length(prob.cdf)
    # binwidth = (prob.xmax - prob.xmin) / (len-1)
    (;mn, mx) = getxextent(prob)
    xr > mn || return 0.0
    xr < mx || return 1.0
    # indleft = floor(Int, 1 + (xr - prob.xmin) / binwidth)
    # indright = indleft + 1
    xs = getxs(prob)
    binwidth = getbinwidth(prob)
    # indright = searchsortedfirst(xs, xr)
    indright = floor(Int, 2 + (xr - mn) / binwidth) # this was faster: 19.7 ns vs. 25.4 ns
    # @show indright indright2
    indleft = indright - 1
    indleft > 0 || error("cdf: indleft > 0")
    # xin = xr - prob.xmin
    # xleft = (indleft-1) * binwidth
    # xright = (indright-1) * binwidth
    xin = xr
    xleft = xs[indleft]
    xright = xs[indright]
    @assert xleft <= xin <= xright string((;xleft, xin, xright))
    @assert xs[indleft] <= xr <= xs[indright] string((;xsleft=xs[indleft], xr, xsright=xs[indright]))
    rightratio = (xin - xleft) / (xright - xleft)
    # global kcdfargs = (prob, x)
    rightpart = rightratio * prob.cdf[indright]
    leftpart = (1.0 - rightratio) * prob.cdf[indleft]
    # @show indleft indright xin xleft xright prob.cdf[indleft] prob.cdf[indright] rightratio leftpart rightpart
    res = leftpart + rightpart
    @assert res >= 0.0 "assertion failed, prob cdf >= 0: $(res)"
    res > 1e-10 || return 0.0
    res < (1.0 - 1e-10) || return 1.0
    return res
end

# getxmin(prob::ProbKde7) = prob.src.extents[1].mn
# getxmax(prob::ProbKde7) = prob.src.extents[1].mx
getxs(prob::ProbKde7) = prob.xs.xs
getxextent(prob::ProbKde7) = prob.src.extents[1]
getbinwidth(prob::ProbKde7) = prob.xs.binwidth

import DrawUtil
DrawUtil.drawprob(prob::ProbKde7; kws...) = DrawUtil.drawdist!(DrawUtil.getAxis(;newFig=true), prob.center, prob.xs.xs, probdispvals(prob); kws...)
DrawUtil.drawprob!(prob::ProbKde7; kws...) = DrawUtil.drawdist!(DrawUtil.getAxis(;newFig=false), prob.center, prob.xs.xs, probdispvals(prob); kws...)
probdispvals(prob::ProbKde7) = ( k = 0.01 / xswidth(prob) ; prob.pdf .* k )
xswidth(prob::ProbKde7) = prob.xs.xs[2] - prob.xs.xs[1]

function makeprob(kde, xpir; kws...)
    mkt = market()
    return makeprob(kde, mkt.curp, mkt.tsMarket, Calendars.getMarketClose(xpir); kws...)
end

# makeprob(tkde, curp, xpir::Date) = makeprob(tkde, curp, Calendars.getMarketClose(xpir))
function makeprob(kde, curp, ts::DateTime, xpirts::DateTime; numbins=600, k=0.001)
    (;tex, logvol) = makekdeargs(curp, ts, xpirts)
    sxs = kde_xs(kde, numbins)
    xs = sxs.xs

    # coord = [0.0, logvol, tex]
    cdfvals = Vector{Float64}(undef, numbins)
    pdfvals = Vector{Float64}(undef, numbins)
    for i in 1:numbins
        # coord[1] = xs[i]
        coord = (xs[i], logvol, tex)
        pdfvals[i] = kde_pdf(kde, coord, k)
    end
    VectorCalcUtil.normalize!(pdfvals)
    cdfvals = accumulate(+, pdfvals)
    return ProbKde7(kde, ts, F(curp), pdfvals, cdfvals, sxs)
end

function tsx_for_kde()
    tsx = DataFiles.dftsx()
    dropmissing!(tsx, [:ret, :logret])
    global ktsx = tsx
    return tsx
end

function data_from_tsx(tsx, len)
    len_tot = length(tsx.ts)
    inds = len > 0 ? (1:len) : ((len_tot+len+1):len_tot)
    return [[tsx.ret[i], tsx.logvol[i], tsx.tex[i]] for i in inds]
end

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

function usekde(;refresh=false)
    if refresh || !isdefined(@__MODULE__, :kkde)
        tsx = tsx_for_kde()
        global kkde = calckde(tsx)
    end
    return kkde
end

function calckde(tsx=ktsx; len=-100000, gridcount=20, area=4)
    m = [Vector{NTuple{3,Float64}}() for _ in 1:gridcount, _ in 1:gridcount, _ in 1:gridcount]

    len_tot = length(tsx.ts)
    inds = len > 0 ? (1:len) : ((len_tot+len+1):len_tot)
    data = (tsx.ret[inds], tsx.logvol[inds], tsx.tex[inds])
    extents = map(d -> Extent2(extrema(d)...), data)
    tfs = VectorCalcUtil.fit_01.(data)

    tx = Transform3(map(tf -> tf.ad, tfs), map(tf -> tf.scale, tfs))

    width = 1.0 / (gridcount - 1)
    gridcs = [togrid(tfs[i].v, width) for i in eachindex(tfs)]

    # coord = zeros(3)
    for i in eachindex(tfs[1].v)
        coord = (tfs[1].v[i], tfs[2].v[i], tfs[3].v[i])
        for x in toarea(gridcs[1][i], area, gridcount), y in toarea(gridcs[2][i], area, gridcount), z in toarea(gridcs[3][i], area, gridcount)
            push!(m[x, y, z], coord)
        end
    end
    return KDENew7(m, tx, width, extents, Xs2([], 0, 0.0))
end

@inline togrid(x, width) = 1 .+ round.(Int, x ./ width)
@inline toarea(mid::Integer, width::Integer, gridcount::Integer) = (max(1, mid - width)):(min(gridcount, mid + width))

function kde_pdf(kde::KDENew7, x, k)
    xin = txin(kde, x) # (x .+ kde.txs.ad) .* kde.txs.scale
    coord = togrid.(xin, kde.width)
    # @show coord
    obs = kde.m[coord...]
    dens = kernel(obs, xin, k)
    return dens
end

txin(kde::KDENew7, x) = (x .+ kde.txs.ad) .* kde.txs.scale
txout(kde::KDENew7, x) = (x ./ kde.txs.scale) .- kde.txs.ad

function kernel(obs, x, k)
    # e^(-(obs - x)^2/k)
    # TODO: multikde uses prod() instead of sum?
    global kobs = obs
    return sum(map(obs) do o
        exp.(-dot2(o .- x) / k)
    end; init=0.0)
    # return sum(exp.(-dot2.(tsub.(obs, x)) / k))
end

@inline dot2(v::NTuple{3,Float64}) = return v[1]^2 + v[2]^2 + v[3]^2
# @inline dot2(v::Vector{Float64}) = return v[1]^2 + v[2]^2 + v[3]^2
# @inline tsub(o, x) = o .- x

function kde_xs(kde::KDENew7, numbins)
    if kde.xs[].numbins != numbins
        kde.xs[] = calcxs(kde, numbins)
    end
    return kde.xs[]
    # return Xs2(kde.xs[].xs, numbins, kde.xs[].binwidth)
end

function calcxs(kde::KDENew7, numbins)
    mn, mx = kde_xtrem(kde)
    binwidth = (mx - mn) / (numbins - 1)
    xs = collect(map(i -> mn + i * binwidth, 0:(numbins-1)))
    return Xs2(xs, numbins, binwidth)
end

kde_xtrem(kde::KDENew7) = ( ex = kde.extents[1] ; return ex.mn, ex.mx )

function drawfield(m)
    inds = reshape(CartesianIndices(m), prod(size(m)))
    ps = [length(m[ind]) for ind in inds]
    xyzs = [Tuple(i) for i in inds]
    global kxyzs = xyzs
    plot = Makie.scatter(xyzs; color=ps, markersize=5, colormap=:viridis, axis=(type=Makie.Axis3,))
    #  camera=Makie.cam3d_cad!
    display(plot)
    # return Makie.camera(plot.figure.scene)
    scene = plot.figure.scene
    Makie.cam3d_cad!(scene)
end

function drawrandfield(kde::KDENew7, num=100; k=0.001)
    # pts = Vector{Vector{Float64}}()
    pts = Vector{NTuple{3,Float64}}()
    denss = Vector{Float64}()
    skipped = 0
    while length(pts) < num
        txin = Tuple(rand(3))
        x = txout(kde, txin)
        # @show txin x
        dens = kde_pdf(kde, x, k)
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
#endregion

#region testing kde
# using MultiKDE, Distributions, Random
# import VectorCalcUtil
# function multikde2(data::Vector{Vector{Float64}}=kdedata()
#             ; coordinit::Vector{Float64}, xs, durs, vols)
#     kde = make_kde(data)
#     # function KDEMulti(dims::Vector, bws::Union{Vector, Nothing}, mat_observations::Matrix, candidates::Union{Dict{Int, Vector}, Nothing})
#     # kde = KDEMulti(dims, bws, data, nothing)
#     global kkde = kde

#     display(DrawUtil.draw(:scatter, [0.0], [0.0]; label="Zero", showlegend=true))
#     vals = Vector{Float64}(undef, length(xs))
#     coord = copy(coordinit)
#     for dur in durs
#         for vol in vols
#             coord[2] = vol
#             coord[3] = dur
#             # vals = [( coord[1] = x ; MultiKDE.pdf(kde, coord) ) for x in xs]
#             for i in eachindex(xs)
#                 coord[1] = xs[i]
#                 vals[i] = MultiKDE.pdf(kde, coord)
#             end
#             VectorCalcUtil.normalize!(vals)
#             DrawUtil.draw!(:scatter, xs, vals; label="$(r2(vol))-$(r2(dur))")
#             # println("done: $(dur)")
#             # yield()
#         end
#     end
#     DrawUtil.updateLegend()
# end

# import StatsBase, DataFiles
# using DataFrames
# # function setuptestkde(len)
# #     df = DataFrame(DataFiles.tstoxpirtable(); copycols=false)
# #     dff = dropmissing(df, [:ret, :logret])
# #     testkde(dff, len)
# #     return df, dff
# # end
# function testkde(dff, len)
#     len_tot = length(dff.ret)
#     inds = len > 0 ? (1:len) : ((len_tot+len):len_tot)
#     # data = [dff.ret[1:len], dff.logvol[1:len], dff.tex[1:len]]
#     # data = collect(hcat(dff.ret[1:len], dff.logvol[1:len], dff.tex[1:len])')
#     data = [[dff.ret[i], dff.logvol[i], dff.tex[i]] for i in inds]
#     xmin, xmax = extrema(dff.ret[inds])
#     xs = xmin:((xmax-xmin)/500):xmax
#     volmin, volmax = extrema(dff.logvol[inds])
#     volmean = StatsBase.mean(dff.logvol[inds])
#     durmin, durmax = extrema(dff.tex[inds])
#     durs = durmin:((durmax-durmin)/10):durmax
#     @show volmin volmax
#     vols = volmin:((volmax-volmin)/10):volmax
#     @show xs vols durs
#     # multikde2(data; xs, coordinit=[0.0, volmean, 0.0], durs=[20], vols)
#     multikde2(data; xs, coordinit=[0.0, volmean, 0.0], durs, vols=[volmean])
# end
#endregion

end