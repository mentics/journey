module CmdUtil
using Dates
using StatusTypes
using StoreTrade
using Expirations

export tradesToClose

tradesToClose(ex::Int=1) = tradesToClose(expir(ex; td=true)) # findTrades(expir(ex; td=true), Filled,Closing,PartialClosed)
tradesToClose(exp::Date) = findTrades(exp, Filled, Closing, PartialClosed)

# TODO: must be moved, wrong level
export calcPosStrat
using SH, BaseTypes, RetTypes, LegMetaTypes, StratTypes
using StoreTrade
function calcPosStrat(forDate::Date, sp::Currency, vtyRatio::Float64, extra::Union{Nothing,Vector{LegMeta}}=nothing)::Vector{LegRet}
    trades = findTrades(forDate, Filled)
    if !isempty(trades)
        lms = tos(Vector{LegMeta}, trades)
        isnothing(extra) || append!(lms, extra)
        sort!(lms; by=getStrike)
        # return [(lm, to(Ret, lm, forDate, sp, vtyRatio)) for lm in lms]
        return tos(LegRet, lms, forDate, sp, vtyRatio)
    else
        return Vector{LegRet}()
    end
end

end