module Construct
using SH, BaseTypes, SmallTypes, ChainTypes, LegTypes, LegMetaTypes, RetTypes
using Globals, CollUtil
using Expirations, Chains, Markets

export bflys

PROFIT_MIN = 0.04

function bflys(ex::Int, width1::Currency=C(1), width2::Currency=C(1))
    vr = getvr()
    sp = market().startPrice
    expr = expir(ex)
    oqs = chains()[expr].chain
    @assert isempty(filter(oq -> !isinteger(getStrike(oq)), chains()[expir(3)].chain))
    style = Style.call
    combis = Vector{NTuple{4,LegMeta}}()
    calls = filter(oq -> style == getStyle(oq) && getBid(oq) >= 0.02, oqs)
    @assert issorted(calls; by=getStrike)
    for i in eachindex(calls)
        oqLeft = calls[i]
        strikeLeft = getStrike(oqLeft)
        strikeMid = strikeLeft + width1
        oqMid = findFrom(i, calls) do oq
            s = getStrike(oq)
            s == strikeMid && return 1
            s > strikeMid && return -1
            return 0
        end
        !isnothing(oqMid) || continue
        strikeRight = strikeMid + width2
        oqRight = findFrom(i, calls) do oq
            s = getStrike(oq)
            s == strikeRight && return 1
            s > strikeRight && return -1
            return 0
        end
        !isnothing(oqRight) || continue
        lms = bfly(oqLeft, oqMid[2], oqRight[2])
        # TODO: optimize, don't need all vals, just the top line
        vals = getVals(combineTo(Ret, lms, expr, sp, vr))
        if vals[1] > PROFIT_MIN
            push!(combis, lms)
        end
    end
    return combis
end

function bfly(oqLeft::OptionQuote, oqMid::OptionQuote, oqRight::OptionQuote, side::Side.T=Side.short)::NTuple{4,LegMeta}
    otherSide = toOther(side)
    # TODO: check if exactly the same if use qty 2 for mid
    lms = (
        LegMeta(Leg(getOption(oqLeft), 1.0, side), getQuote(oqLeft, side), getMeta(oqLeft)),
        LegMeta(Leg(getOption(oqMid), 1.0, otherSide), getQuote(oqMid, otherSide), getMeta(oqMid)),
        LegMeta(Leg(getOption(oqMid), 1.0, otherSide), getQuote(oqMid, otherSide), getMeta(oqMid)),
        LegMeta(Leg(getOption(oqRight), 1.0, side), getQuote(oqRight, side), getMeta(oqRight))
    )
    return lms
end

end