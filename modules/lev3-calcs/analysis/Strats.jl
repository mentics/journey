module Strats
# using CalcUtil
# using SH, StratTypes, ProbTypes
using SH, BaseTypes, StratTypes
using RetTypes, Rets

# TODO: put this somewhere else and get rid of this file?
SH.combineTo(::Type{Ret}, lrs::Coll{LegRet,4})::Ret = combineRets(map(to(Ret), lrs))
SH.combineTo(::Type{Vals}, lrs::Coll{LegRet,4})::Vals = combineRetVals(tos(Ret, lrs))

SH.getVals!(buf::Vector{Float64}, s::Strat)::Nothing = ( combineRetVals!(buf, tos(Ret, s)) ; return )
# SH.getVals!(buf::Vector{Float64}, s::Strat, extra::Vector{LegRet})::Nothing = ( combineRetVals!(buf, getRets(s), getindex.(extra, 2)) ; return )
SH.getVals!(buf::Vector{Float64}, s::Strat, extra::Vector{Float64})::Nothing = ( combineRetVals!(buf, tos(Ret, s), extra) ; return )
# SH.getVals(s::Strat)::Vector{Float64} = combineRetVals(getRets(s))
# SH.getVals(s::Strat, extra::Vector{LegRet})::Vector{Float64} = combineRetVals(vcat(collect(tos(Ret, s)), tos(Ret, extra)))
# SH.getVals(lrs::Vector{LegRet})::Vector{Float64} = combineRetVals(getindex.(lrs, 2))

end