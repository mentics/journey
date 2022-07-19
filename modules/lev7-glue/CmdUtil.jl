module CmdUtil
using Dates
using StatusTypes
using StoreTrade
using Expirations

export tradesToClose, xlms, xlmsv

tradesToClose(ex::Int=0) = tradesToClose(expir(ex)) # findTrades(expir(ex; td=true), Filled,Closing,PartialClosed)
tradesToClose(exp::Date) = findTrades(exp, Filled, Closing, PartialClosed)

# TODO: must be moved, wrong level
export calcPosStrat
using SH, BaseTypes, RetTypes, LegMetaTypes, StratTypes
using StoreTrade
function calcPosStrat(forDate::Date, sp::Currency, vtyRatio::Float64, extra::Union{Nothing,Vector{LegMeta}}=nothing)::Vector{LegRet}
    lms = findLmsPos(forDate)
    if !isempty(lms)
        isnothing(extra) || append!(lms, extra)
        sort!(lms; by=getStrike)
        # return [(lm, to(Ret, lm, forDate, sp, vtyRatio)) for lm in lms]
        return tos(LegRet, lms, forDate, sp, vtyRatio)
    else
        return Vector{LegRet}()
    end
end

xlmsv(ex=0) = tos(Vector{LegMeta}, tradesToClose(ex))
function xlms(when=0)::Vector{LegMeta}
    combineTo(Vector{LegMeta}, tradesToClose(when))
    # trades = findTrades(forDate, Filled)
    # if !isempty(trades)
    #     lms = tos(Vector{LegMeta}, trades)
    #     sort!(lms; by=getStrike)
    #     return lms
    # else
    #     return Vector{LegMeta}()
    # end
end

end