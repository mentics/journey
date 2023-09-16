module ExploreExtra

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
#endregion testing kde

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

#region test
function testCalcKel()
    curp = market().curp
    oqs = filter!(isCall, Chains.chain(expir(1)).chain)
    sort!(oqs; by=oq -> abs(curp - SH.getStrike(oq)))
    lms = sortuple(SH.getStrike, LegQuoteOpen(oqs[1], Side.short), LegQuoteOpen(oqs[2], Side.long))
    # lms = discount.(lms, 0.1)
    DrawUtil.drawprob(ProbLup[SH.getExpir(lms)]; color=Makie.RGBA(.1, .1, .5, 0.5))
    display(DrawUtil.draw!(:lines, SH.toDraw(lms)))
    return calckel(lms)
end

using LegQuoteTypes, QuoteTypes
discount(lm::LegQuoteOpen, r=0.75) = LegQuoteOpen(SH.getLeg(lm), discount(SH.getQuote(lm), r), SH.getMeta(lm))
discount(q::Quote, r=0.75) = Quote(P(q.bid + r), P(q.ask + r))
#endregion test

#region TestMemoizePerf
import ThreadSafeDicts
import BenchmarkTools
function testdicts()
    pairs = [:a => 1, :b => 2]
    td = ThreadSafeDicts.ThreadSafeDict(pairs)
    d = Dict(pairs)
    BenchmarkTools.@btime get($td, :b, nothing)
    BenchmarkTools.@btime get($d, :b, nothing)
    BenchmarkTools.@btime get($td, :c, nothing)
    BenchmarkTools.@btime get($d, :c, nothing)
end

import Memoization
function testbdays()
    d1 = Date(2022,1,1)
    d2 = Date(2022,10,3)
    d3 = Date(2022,11,17)
    d4 = Date(2022,12,23)
    f_test = DateUtil.bdays_t
    exp12 = f_test(d1, d2)
    exp13 = f_test(d1, d3)
    exp14 = f_test(d1, d4)
    exp23 = f_test(d2, d3)
    exp24 = f_test(d2, d4)
    exp43 = f_test(d4, d3)
    Memoization.empty_all_caches!()
    Threads.@threads for i in 1:100
        @assert f_test(d1, d2) === exp12
        @assert f_test(d1, d3) === exp13
        @assert f_test(d1, d4) === exp14
        @assert f_test(d2, d3) === exp23
        @assert f_test(d2, d4) === exp24
        @assert f_test(d4, d3) === exp43
    end
    Threads.@threads for i in 1:100
        Memoization.empty_all_caches!()
        Threads.@threads for i in 1:100
            @assert f_test(d1, d2) === exp12
            @assert f_test(d1, d3) === exp13
            @assert f_test(d1, d4) === exp14
            @assert f_test(d2, d3) === exp23
            @assert f_test(d2, d4) === exp24
            @assert f_test(d4, d3) === exp43
        end
        Memoization.empty_all_caches!()
    end
end
#endregion TestMemoizePerf

end