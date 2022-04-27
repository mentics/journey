module CmdUtil
using StatusTypes
using StoreTrade
using Expirations

export tradesToClose

tradesToClose(ex=1) = findTrades(expir(ex; td=true), Filled,Closing,PartialClosed)

# TODO: must be moved, wrong level
export calcPosStrat
using Dates
using SH, BaseTypes, RetTypes, LegMetaTypes, StratTypes
using StoreTrade
function calcPosStrat(forDate::Date, sp::Currency, vtyRatio::Float64, extra::Union{Nothing,Vector{LegMeta}}=nothing)::Vector{LegRet}
    trades = findTrades(forDate, Filled)
    if !isempty(trades)
        lms = tos(Vector{LegMeta}, trades)
        isnothing(extra) || append!(lms, extra)
        sort!(lms; by=getStrike)
        return [(lm, to(Ret, lm, forDate, sp, vtyRatio)) for lm in lms]
    else
        return Vector{LegRet}()
    end
end

end