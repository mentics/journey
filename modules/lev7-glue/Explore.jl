module Explore
using Dates
import SH

#region LookForBestKelly
using SmallTypes, LegMetaTypes
using Markets, Expirations, Chains
import Kelly, ChainUtil, Between

function findkel1()
    xpir = expir(2)
    curp = market().curp
    println("For curp: $(curp)")
    oqss = ChainUtil.oqssAll(chain(xpir).chain)
    puts = oqss.put.long
    prob = Between.makeprob(now(UTC), xpir, curp)
    i = 1
    res = map(puts) do put
        lms = (LegMetaOpen(put, Side.long),)
        kel = Kelly.calcKel(prob, lms)
        println("$(i): Long on strike $(SH.getStrike(lms[1])) -> $(kel)")
        i += 1
        return (;put, lms, kel)
    end
    return res
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

end