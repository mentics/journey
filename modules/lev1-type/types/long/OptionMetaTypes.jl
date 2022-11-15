module OptionMetaTypes
using SH

export OptionMeta

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
SH.getTheta(m::OptionMeta) = m.theta
SH.getDelta(m::OptionMeta) = m.delta

end