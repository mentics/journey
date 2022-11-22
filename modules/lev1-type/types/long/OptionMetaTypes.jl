module OptionMetaTypes
using SH

export Greeks, GreeksType, OptionMeta
export getGreeks, greeksAdd, greeksSum, greeksMult, GreeksZero

const Greeks = (:delta, :theta, :phi, :vega, :rho, :gamma)
const GreeksZero = (; delta=0.0, theta=0.0, phi=0.0, vega=0.0, rho=0.0, gamma=0.0)
const GreeksType = NamedTuple{Greeks,NTuple{6,Float64}}

struct OptionMeta
    delta::Float64
    theta::Float64
    phi::Float64
    vega::Float64
    rho::Float64
    gamma::Float64
    bid_iv::Float64
    ask_iv::Float64
    mid_iv::Float64
end
OptionMeta(;delta=0.0, theta=0.0, phi=0.0, vega=0.0, rho=0.0, gamma=0.0, bid_iv=0.0, ask_iv=0.0, mid_iv=0.0, kws...) = OptionMeta(delta, theta, phi, vega, rho, gamma, bid_iv, ask_iv, mid_iv)
OptionMeta(d::Dict{String,Any}) = OptionMeta(; (Symbol(x[1]) => x[2] for x in d)...)
SH.getIv(m::OptionMeta) = m.mid_iv

# SH.getTheta(m::OptionMeta) = m.theta
# SH.getDelta(m::OptionMeta) = m.delta
# SH.getGamma(m::OptionMeta) = m.gamma
# SH.getVega(m::OptionMeta) = m.vega
# SH.getRho(m::OptionMeta) = m.rho
getGreeks(m::OptionMeta)::GreeksType = (; m.delta, m.theta, m.phi, m.vega, m.rho, m.gamma)
greeksAdd(g1::GreeksType, g2::GreeksType)::GreeksType = ntadd(g1, g2) # (; delta=k*m.delta, theta=k*m.theta, phi=k*m.phi, vega=k*m.vega, rho=k*m.rho, gamma=k*m.gamma)
ntadd(nt1::NamedTuple, nt2::NamedTuple) = NamedTuple{fieldnames(nt1)}(values(nt1) .+ values(nt2))

greeksMult(k::Real, m)::GreeksType = (; delta=k*m.delta, theta=k*m.theta, phi=k*m.phi, vega=k*m.vega, rho=k*m.rho, gamma=k*m.gamma)
function greeksSum(itr)::GreeksType
    delta = 0.0; theta = 0.0; phi = 0.0; vega = 0.0; rho = 0.0; gamma = 0.0
    for x in itr
        delta += x.delta
        theta += x.theta
        phi += x.phi
        vega += x.vega
        rho += x.rho
        gamma += x.gamma
    end
    return (; delta, theta, phi, vega, rho, gamma)
end

getGreeks(itr)::GreeksType = greeksSum(map(getGreeks, itr))

# getTheta(x) = greeks(x).theta
# sumDelta(itr) = sum(getDelta, itr)
# sumTheta(itr) = sum(getTheta, itr)
# sumPhi(itr) = sum(getPhi, itr)
# sumVega(itr) = sum(getVega, itr)
# sumRho(itr) = sum(getRho, itr)
# sumGamma(itr) = sum(getGamma, itr)

end