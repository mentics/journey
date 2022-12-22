module OptionMetaTypes
using SH, SmallTypes

export Greeks, GreeksZero, OptionMeta, newOptionMeta
export addGreeks, sumGreeks, multGreeks

# const GreekSyms = (:delta, :theta, :phi, :vega, :rho, :gamma)
# const GreeksType = NamedTuple{Greeks,NTuple{6,Float64}}

struct Greeks
    delta::Float64
    theta::Float64
    phi::Float64
    vega::Float64
    rho::Float64
    gamma::Float64
end
const GreeksZero = Greeks(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
Base.show(io::IO, x::Greeks) = print(io, "Greeks(delta=$(x.delta), gamma=$(x.gamma), theta=$(x.theta), vega=$(x.vega), phi=$(x.phi), rho=$(x.rho))")
Base.:(*)(mult::Float64, g::Greeks) = Greeks(mult*g.delta, mult*g.theta, mult*g.phi, mult*g.vega, mult*g.rho, mult*g.gamma)

struct OptionMeta
    greeks::Greeks
    bid_iv::Float64
    ask_iv::Float64
    mid_iv::Float64
end
def(::Missing) = NaN
def(x) = x
function OptionMeta(delta, theta, phi, vega, rho, gamma, bid_iv, ask_iv, mid_iv)
    return OptionMeta(Greeks(def(delta), def(theta), def(phi), def(vega), def(rho), def(gamma)), def(bid_iv), def(ask_iv), def(mid_iv))
end
OptionMeta(;delta=NaN, theta=NaN, phi=NaN, vega=NaN, rho=NaN, gamma=NaN, bid_iv=NaN, ask_iv=NaN, mid_iv=NaN, kws...) = OptionMeta(delta, theta, phi, vega, rho, gamma, bid_iv, ask_iv, mid_iv)
OptionMeta(d::Dict{String,Any}) = OptionMeta(; (Symbol(x[1]) => x[2] for x in d)...)
function newOptionMeta(om::OptionMeta, dir::DirSQ)
    OptionMeta(multGreeks(dirMult(dir), om.greeks), om.bid_iv, om.ask_iv, om.mid_iv)
end
SH.getIv(m::OptionMeta) = m.mid_iv
SH.getGreeks(m::OptionMeta) = m.greeks

addGreeks(g1::Greeks, g2::Greeks)::Greeks = Greeks(g1.delta + g2.delta, g1.theta + g2.theta, g1.phi + g2.phi, g1.vega + g2.vega, g1.rho + g2.rho, g1.gamma + g2.gamma)

multGreeks(k::Real, g2::Greeks)::Greeks = Greeks(k * g2.delta, k * g2.theta, k * g2.phi, k * g2.vega, k * g2.rho, k * g2.gamma)
function sumGreeks(itr)::Greeks
    delta = 0.0; theta = 0.0; phi = 0.0; vega = 0.0; rho = 0.0; gamma = 0.0
    for x in itr
        delta += x.delta
        theta += x.theta
        phi += x.phi
        vega += x.vega
        rho += x.rho
        gamma += x.gamma
    end
    return Greeks(delta, theta, phi, vega, rho, gamma)
end

SH.getGreeks(itr)::Greeks = sumGreeks(map(getGreeks, itr))

Base.:(*)(mult::Float64, om::OptionMeta) = OptionMeta(mult*om.greeks, om.bid_iv, om.ask_iv, om.mid_iv)

end