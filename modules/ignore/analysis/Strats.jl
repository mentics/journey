module Strats
# using CalcUtil
# using SH, StratTypes, ProbTypes
using SH, BaseTypes, StratTypes
using RetTypes, Rets

# TODO: put this somewhere else and get rid of this file?

SH.getVals!(buf::Vector{Float64}, s::Strat)::Vector{Float64} = ( combineRetVals!(buf, tos(Ret, s)) ; return buf )
# SH.getVals!(buf::Vector{Float64}, s::Strat, extra::Vector{LegRet})::Nothing = ( combineRetVals!(buf, getRets(s), getindex.(extra, 2)) ; return )
SH.getVals!(buf::Vector{Float64}, s::Strat, extra::Vector{Float64})::Vector{Float64} = ( combineRetVals!(buf, tos(Ret, s), extra) ; return buf )
# SH.getVals(s::Strat)::Vector{Float64} = combineRetVals(getRets(s))
# SH.getVals(s::Strat, extra::Vector{LegRet})::Vector{Float64} = combineRetVals(vcat(collect(tos(Ret, s)), tos(Ret, extra)))
# SH.getVals(lrs::Vector{LegRet})::Vector{Float64} = combineRetVals(getindex.(lrs, 2))

end