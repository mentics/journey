module Explore
using Dates
using ThreadPools
using BaseTypes, LegTypes
import SH, DateUtil
import CollUtil.sortuple

const CONFIG3 = Ref((;
    adjustprices = -0.005,
    kelprobadjust = 0.25
))

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
    segs = LL.toSegments(lms, CONFIG3[].adjustprices)
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

function findkel(inc)
    ts = now(UTC)
    date = Date(ts)
    curp = market().curp
    println("Finding kel for curp: $(curp)")
    xpirs = Expirations.xpirsinrange(1, 2)
    ress = [[] for _ in 1:Threads.nthreads()]
    global kprobs = Dict{Any, Any}()
    for xpir in xpirs
        println("Searching xpir: $(xpir)")
        prob = Between.makeprob(xpir, curp)
        riskrat = calcriskrat(date, xpir)
        ctx = (;curp, prob, riskrat, xpir)
        kprobs[xpir] = prob
        oqss = ChainUtil.oqssEntry(chain(xpir).chain, curp)
        println("num calls $(length(oqss.call.long))")
        println("num puts $(length(oqss.put.long))")
        1 in inc && findkel1!(ress, ctx, oqss)
        2 in inc && findkel2!(ress, ctx, oqss)
        3 in inc && findkel3!(ress, ctx, oqss)
    end
    println("Finished searching")
    res = vcat(ress...)
    sort!(res; rev=true, by=r->r.evrate)
    return res
end

function filt(res)
    filter(res) do r
        # r.probprofit > 0.55 && r.risk <= 5.0 &&
        DateUtil.bdays(today(), SH.getExpir(r.lms)) >= 3
    end
end

# function sortres!(date, res)
#     sort!(res; by=r -> calcret(r, date))
# end

calcevrate(evret, xpir, date) = evret * DateUtil.timult(date, xpir)

# calcrate(r, date=today()) = DateUtil.calcRate(date, r.xpir, r.kel * r.ev, r.risk)
# calcret(r, date=today()) = r.kel * calcrate(r, date)

function findkel1!(res, ctx, oqss)
    Threads.@threads for call in oqss.call.long
        check!(res, ctx, (LegMetaOpen(call, Side.long),))
    end
    Threads.@threads for put in oqss.put.long
        check!(res, ctx, (LegMetaOpen(put, Side.long),))
    end
end

import GenCands
function findkel2!(res, ctx, oqss)
    GenCands.paraSpreads(oqss.call, 10, (_...) -> true) do thid, lms
        check!(res, ctx, lms)
    end
    GenCands.paraSpreads(oqss.put, 10, (_...) -> true) do thid, lms
        check!(res, ctx, lms)
    end
end

function findkel3!(res, ctx, oqss)
    println("Searching calls")
    GenCands.paraSpreads(oqss.call, 4, (_...) -> true) do thid, lms
        # println("findkel3 checking spread $(SH.getStrike(lms[1])) $(SH.getStrike(lms[2]))")
        # Threads.@threads
        for oq in oqss.call.long
            lms2 = sortuple(SH.getStrike, lms..., LegMetaOpen(oq, Side.long))
            check!(res, ctx, lms2)
        end
        # Threads.@threads
        for oq in oqss.put.long
            lms2 = sortuple(SH.getStrike, lms..., LegMetaOpen(oq, Side.long))
            check!(res, ctx, lms2)
        end
        return true
    end
    println("Searching puts")
    GenCands.paraSpreads(oqss.put, 4, (_...) -> true) do thid, lms
        # Threads.@threads
        for oq in oqss.call.long
            lms2 = sortuple(SH.getStrike, lms..., LegMetaOpen(oq, Side.long))
            check!(res, ctx, lms2)
        end
        # Threads.@threads
        for oq in oqss.put.long
            lms2 = sortuple(SH.getStrike, lms..., LegMetaOpen(oq, Side.long))
            check!(res, ctx, lms2)
        end
        return true
    end
    println("Searching done.")
end
#endregion

#region Common
function check!(res, ctx, lms)
    yield()
    ss = tosegsz(ctx.curp, lms)
    !isnothing(ss) || ( println("ss was nothing") ; return true )
    try
        kel, evret, ev, risk = calckel(ctx.prob, ctx.riskrat, ss)
        if isfinite(kel) && kel > 0
            # println("$(i): Long on strike $(SH.getStrike(lms[1])) -> $(kel)")
            xpir = ctx.xpir
            evrate = calcevrate(evret, xpir, today())
            if evrate > 0.5
                probprofit = Between.calcprobprofit(ctx.prob, ss.segsz)
                if probprofit > 0.7
                    push!(res[Threads.threadid()], (;ev, kel, evret, evrate, risk, xpir, lms, probprofit))
                end
            end
        end
    catch e
        global kcheck = (;ctx, lms, ss)
        rethrow(e)
    end
    return true
end

# function fix(lm1, lm2, lm3)
#     s1 = getStrike(lm1)
#     if s1 == getStrike(lm2)
#         if getSide(lm1) == getSide(lm2)
#         else
#         end
#     else s1 == getStrike(lm3)
#     else
#         return sortuple(getStrike, lm1, lm2, lm3)
#     end
# end
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
    adjustprices = CONFIG3[].adjustprices
    segs = LL.toSegments(lms, adjustprices)
    LL.canprofit(segs) || return nothing
    segsz = LL.toSegmentsWithZeros(segs; extent=(0.5*curp, 1.5*curp))
    return (;segs, segsz)
end

calckel(prob::Prob, riskrat::Real, lms::Coll{<:LegLike}) = calckel(prob, riskrat, tosegsz(prob.center, lms))

function calckel(prob::Prob, riskrat::Real, (;segs, segsz))
    probadjust = CONFIG3[].kelprobadjust
    risk = riskrat * max(Pricing.calcMarg(prob.center, segs))
    try
        return (Kelly.calckel(prob, risk, segsz; probadjust)..., risk)
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
import Distributions, KernelDensityEstimate, DrawUtil
function kdedata()
    nspy = Distributions.Normal(1.0, 0.1)
    numsamples = 10000
    maxdur = 10
    basevix = 10
    vixspread = 2
    return [(dur = 1 + i % maxdur ; spy = log(rand(nspy)) ; [spy*dur, basevix + vixspread*spy + rand() - 0.5, dur]) for i in 0:numsamples]
end
function testkde()
    bws = [0.1, 0.1, 1.01]
    data = stack(kdedata())
    kdem1 = KernelDensityEstimate.kde!(data, bws)
    range1 = -0.49:0.01:0.49
    vals = map(x -> x[1], [kdem1(reshape([x, basevix, maxdur / 2], 3, 1)) for x in range1])
    display(DrawUtil.draw(:scatter, range1, vals))
    for k in 0.5:0.1:1.5
        sleep(0.5)
        vals = map(x -> x[1], [kdem1(reshape([x, basevix, k * maxdur / 2], 3, 1)) for x in range1])
        DrawUtil.draw!(:scatter, range1, vals)
    end
end

using MultiKDE, Distributions, Random
function multikde()
    data = kdedata()
    dims = [ContinuousDim(), ContinuousDim(), ContinuousDim()]
    bws = [0.1, 0.1, 1.01]
    kde = KDEMulti(dims, bws, data)

    xs = -0.49:0.01:0.49
    display(DrawUtil.draw(:scatter, [0.0], [0.0]))
    for dur in 1.0:1.0:10.0
        vals = [MultiKDE.pdf(kde, [x, 10.0, dur]) for x in xs]
        DrawUtil.draw!(:scatter, xs, vals)
        println("done: $(dur)")
        yield()
    end
end

using MultiKDE, Distributions, Random
function multikde2()
    data = kdedata()
    dims = [ContinuousDim(), ContinuousDim(), ContinuousDim()]
    bws = [0.1, 0.1, 1.01]
    kde = KDEMulti(dims, bws, data)

    xs = -0.49:0.01:0.49
    display(DrawUtil.draw(:scatter, [0.0], [0.0]))
    vals = Vector{Float64}(undef, length(xs))
    coord = [0.0, 10.0, 0.0]
    for dur in 1.0:1.0:10.0
        coord[3] = dur
        # vals = [( coord[1] = x ; MultiKDE.pdf(kde, coord) ) for x in xs]
        for i in eachindex(xs)
            coord[1] = xs[i]
            vals[i] = MultiKDE.pdf(kde, coord)
        end
        DrawUtil.draw!(:scatter, xs, vals)
        # println("done: $(dur)")
        # yield()
    end
end
#endregion
end