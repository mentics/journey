module BlacksPricing
using Distributions
using DateUtil
using SmallTypes, OptionTypes

export priceOption #, priceOptionF

# Black Scholes implementation copied originally from https://github.com/JuliaQuant/FinancialDerivatives.jl

# # Arguments
# - `s`: underlying price
# - `k`: strike price
# - `r`: risk-free interest rate
# - `σ`: volatility
# - `t`: time to expiration
# - `call`: 1 if call, -1 if put
# struct EuropeanOption{T<:Number} <: Option
#     s::T
#     k::T
#     r::T
#     σ::T
#     t::T
#     call::Int64
# end

# priceOptionF(opt::Option, vty::Real, expFrom::Date, qtyDir::Float64=1.0, neto::Float64=0.0)::Function =
#         priceOptionF(style(opt), strike(opt), vty, timeToExpir(expiration(opt), expFrom), qtyDir, neto)
# function priceOptionF(styl::StyleEnum, strik::Real, vty::Real, toExpY::Real, qtyDir::Float64=1.0, neto::Float64=0.0)::Function
#     # @info "priceOptionF" styl strik vty toExpY qtyDir neto
#     # asdf(u2::Real) = priceOption(opt, u2, vty, from)
#     # asdf(underlying::Real, toExpY2::Real) = neto + qtyDir * priceOption(styl, strik, toExpY2, underlying, vty)
#     # asdf(u2::Real, vty2::Real) = priceOption(opt, u2, vty2, from)
#     asdf(underlying::Real, vty2::Real=vty, toExpY2::Real=toExpY) = neto + qtyDir * priceOption(styl, strik, toExpY2, underlying, vty2)
#     return asdf
# end


# priceOptionF(opt::Option, vty::Real, fromExp::Date, qtyDir::Float64=1.0)::Function = priceOptionF(opt, vty, qtyDir, timeToExpir(expiration(opt), fromExp))
# function priceOptionF(opt::Option, vty::Real, msFrom::Int, qtyDir::Float64)::Function
#     # asdf(u2::Real) = priceOption(opt, u2, vty, from)
#     asdf(underlying::Real, msFrom2::Int) = qtyDir * priceOption(opt, underlying, vty, msFrom2)
#     # asdf(u2::Real, vty2::Real) = priceOption(opt, u2, vty2, from)
#     asdf(underlying::Real, vty2::Real=vty, msFrom2::Int=msFrom) = qtyDir * priceOption(opt, underlying, vty2, msFrom2)
#     return asdf
# end
# priceOptionF(opt::Option, vty::Real, fromExp::Date)::Function = priceOptionF(opt, vty, timeToExpir(expiration(opt), fromExp))
# function priceOptionF(opt::Option, vty::Real, msFrom::Int)::Function
#     # asdf(u2::Real) = priceOption(opt, u2, vty, from)
#     asdf(underlying::Real, msFrom2::Int) = priceOption(opt, underlying, vty, msFrom2)
#     # asdf(u2::Real, vty2::Real) = priceOption(opt, u2, vty2, from)
#     asdf(underlying::Real, vty2::Real=vty, msFrom2::Int=msFrom) = priceOption(opt, underlying, vty2, msFrom2)
#     return asdf
# end

# TODO: benchmark using local var and direct Normal() call
# priceOption(opt::Option, underlying::Real, vty::Real, fromExp::Date)::Float64 = priceOption(opt, timeToExpir(expiration(opt), fromExp), underlying, vty)
# priceOption(opt::Option, underlying::Real, vty::Real, toExp::Int)::Float64 = priceOption(style(opt), strike(opt), toExp, underlying, vty)
function priceOption(style::Style.T, strike::Real, toExpYear::Float64, vty::Float64, under::Float64)::Float64
    # TODO: it returns NaN if underlying == strike when toExpY = 0
    # @info "priceOption" styl strik toExpY under vty
    rfrate = 0.0
    # div = dividend rate
    # FinancialToolbox had -d in here I think, but I'm not using that
    # d1 = (log(pri / strik) + (rfrate - div + O.σ * O.σ / 2) * O.t) / (O.σ * √O.t)
    rt = sqrt(toExpYear)
    d1 = (log(under / strike) + (rfrate + vty * vty / 2.0) * toExpYear) / (vty * rt)
    d2 = d1 - vty * rt

    if style == Style.put
        r = cdf(NORM, -d2) * strike * exp(-rfrate * toExpYear) - cdf(NORM, -d1) * under
    else
        r = under * cdf(NORM, d1) - exp(-rfrate * toExpYear) * strike * cdf(NORM, d2)
    end
    # (isfinite(r) && (-100.0 < r < 100.0)) || error("afterExp: calced nan ", r, " ", (1,2))
    return r
end

#region Local
const NORM = Normal()
#endregion

end