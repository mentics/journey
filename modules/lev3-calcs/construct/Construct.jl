module Construct
using SH, BaseTypes, SmallTypes, ChainTypes, LegTypes, LegMetaTypes, RetTypes
using Globals, CollUtil
using Expirations, Chains, Markets

using DrawStrat, DrawStrat

export tr
function tr(ex=1)
    vr = getvr()
    sp = market().startPrice
    expr = expir(ex)
    oqs = filter(oq -> abs(getStrike(oq) - sp) < 20, chains()[expr].chain)
    # oqs = chains()[expr].chain
    calls = filter(oq -> Style.call == getStyle(oq) && getBid(oq) >= 0.02, oqs)
    puts = filter(oq -> Style.put == getStyle(oq) && getBid(oq) >= 0.02, oqs)
    shouldLong = Vector{OptionQuote}()
    shouldShort = Vector{OptionQuote}()
    ptsBid = [(getStrike(c), getExtrinsic(c, sp)[1]) for c in calls]
    ptsAsk = [(getStrike(c), getExtrinsic(c, sp)[2]) for c in calls]
    pptsBid = [(getStrike(c), getExtrinsic(c, sp)[1]) for c in puts]
    pptsAsk = [(getStrike(c), getExtrinsic(c, sp)[2]) for c in puts]

    fig = draw(ptsBid)
    # DataInspector(fig)
    display(fig)
    draw!(ptsAsk)
    draw!(pptsBid)
    draw!(pptsAsk)
    return ptsBid
end


export bflys, fly

PROFIT_MIN = 0.04

fly(ex, w) = bflys(ex, C(w), C(w))
function bflys(ex::Int, width1::Currency=C(2), width2::Currency=C(2))
    vr = getvr()
    sp = market().startPrice
    expr = expir(ex)
    oqs = chains()[expr].chain
    @assert isempty(filter(oq -> !isinteger(getStrike(oq)), chains()[expir(3)].chain))
    calls = filter(oq -> Style.call == getStyle(oq) && getBid(oq) >= 0.02, oqs)
    puts = filter(oq -> Style.put == getStyle(oq) && getBid(oq) >= 0.02, oqs)
    combiCalls = flys(expr, calls, width1, width2)
    combiPuts = flys(expr, puts, width1, width2)
    lmsCalls = collect(Iterators.flatten(combiCalls))
    lmsPuts = collect(Iterators.flatten(combiPuts))
    lmsAll = vcat(lmsCalls, lmsPuts)
    drawRet(combineTo(Ret, lmsAll, expr, sp, vr), nothing, sp, "")
end

function flys(expr, oqs::Vector{OptionQuote}, width1::Currency, width2::Currency)
    vr = getvr()
    sp = market().startPrice
    combis = Vector{NTuple{4,LegMeta}}()
    @assert issorted(oqs; by=getStrike)
    prof = 0.0
    strikeRightPrev = C(0)
    for i in eachindex(oqs)
        oqLeft = oqs[i]
        strikeLeft = getStrike(oqLeft)
        strikeLeft >= strikeRightPrev || continue
        strikeMid = strikeLeft + width1
        oqMid = findFrom(i, oqs) do oq
            s = getStrike(oq)
            s == strikeMid && return 1
            s > strikeMid && return -1
            return 0
        end
        !isnothing(oqMid) || continue
        strikeRight = strikeMid + width2
        oqRight = findFrom(i, oqs) do oq
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
            prof += vals[1]
            println("profit: ", vals[1])
            push!(combis, lms)
            strikeRightPrev = strikeRight
        end
    end
    println("total prof: ", prof)
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

lm(oq, side) = LegMeta(Leg(getOption(oq), 1.0, side), getQuote(oq, side), getMeta(oq))

end