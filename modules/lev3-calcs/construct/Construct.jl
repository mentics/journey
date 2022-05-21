module Construct
using SH, BaseTypes, SmallTypes, ChainTypes, LegTypes, LegMetaTypes, RetTypes
using Globals, CollUtil
using Expirations, Chains, Markets

using DrawStrat, DrawStrat

export tr, tr2, tr3

using StratGen, StratTypes
function tr3(ex=1)
    vr = getvr()
    sp = market().startPrice
    expr = expir(ex)
    callSprs, putSprs = allSpreads(chains(), (x,y)->false, marketPrices(), [expr])
    allSprs = vcat(callSprs, putSprs)
    lsprs = filter(spreadFilt1(Side.long), allSprs)
    ssprs = filter(spreadFilt1(Side.short), putSprs)
    sort!(lsprs; rev=true, by=spreadSort1)
    sort!(ssprs; rev=true, by=spreadSort1)
    c = (ssprs[1][1], ssprs[1][2], lsprs[1][1], lsprs[1][2])
    drawRet(combineTo(Ret, c), nothing, sp, "")
    return (lsprs, ssprs)
end

function spreadFilt1(side::Side.T)
    function(spr::Spread)
        lr1, lr2 = spr
        v1 = lr1[2].vals
        v2 = lr2[2].vals
        left = v1[1] + v2[1]
        right = v1[end] + v2[end]
        res = left + right
        return res > 0.1 && (Side.long == side ? right > left : left > right)
    end
end

function spreadSort1(spr::Spread)
    lr1, lr2 = spr
    v1 = lr1[2].vals
    v2 = lr2[2].vals
    # str1 = getStrike(lr1[1])
    # str2 = getStrike(lr1[1])
    left = v1[1] + v2[1]
    right = v1[end] + v2[end]
    res = left + right
    return res # / (-min(left, right))
end


function tr2(ex=1)
    vr = getvr()
    sp = market().startPrice
    expr = expir(ex)
    oqs = filter(oq -> abs(getStrike(oq) - sp) < 20, chains()[expr].chain)
    # oqs = chains()[expr].chain
    calls = filter(oq -> Style.call == getStyle(oq) && getBid(oq) >= 0.02, oqs)
    puts = filter(oq -> Style.put == getStyle(oq) && getBid(oq) >= 0.02, oqs)
    callsLong, callsShort = extrinsicDiffs(calls, sp)
    putsLong, putsShort = extrinsicDiffs(puts, sp)

    toLong = sort!(vcat(callsLong, putsLong); rev=true, by=(t -> t[1]))
    toShort = sort!(vcat(callsShort, putsShort); rev=true, by=(t -> t[1]))
    # lms = [
    #     lm(toShort[1][3], Side.short),
    #     lm(toShort[1][4], Side.long),
    #     lm(toLong[1][3], Side.long),
    #     lm(toLong[1][4], Side.short)
    #     # lm(toShort[1][3], Side.long),
    #     # lm(toShort[1][4], Side.short),
    #     # lm(toLong[1][3], Side.short),
    #     # lm(toLong[1][4], Side.long)
    # ]
    # oqsUse = sort!(collect((toShort[1][3:4]..., toLong[1][3:4]...)); by=getStrike)
    # lms = [lm(oq, side) for (oq, side) in zip(oqsUse, [Side.short, Side.long, Side.long, Side.short])]

    # lms = Vector{LegMeta}()
    # oq1, oq2 = toShort[1][3:4]
    # if getStrike(oq1) > sp
    #     push!(lms, lm(oq1, Side.long))
    #     push!(lms, lm(oq2, Side.short))
    # else
    #     push!(lms, lm(oq1, Side.short))
    #     push!(lms, lm(oq2, Side.long))
    # end

    # oq1, oq2 = toShort[2][3:4]
    # if getStrike(oq1) > sp
    #     push!(lms, lm(oq1, Side.long))
    #     push!(lms, lm(oq2, Side.short))
    # else
    #     push!(lms, lm(oq1, Side.short))
    #     push!(lms, lm(oq2, Side.long))
    # end

    drawRet(combineTo(Ret, lms, expr, sp, vr), nothing, sp, "")
    # return lms

    # display(draw([(t[2], t[1]) for t in sort(toLong; by=x->x[2])]))
    # draw!([(t[2], t[1]) for t in sort(toShort; by=x->x[2])])
    # draw!(putsBids)
    # draw!(putsAsks)
    return (toLong, toShort, lms)
end
-(a::Tuple{Currency, Currency}, b::Tuple{Currency, Currency}) = (a[1] - b[1], a[2] - b[2])
abs(t::Tuple{Currency, Currency}) = map(abs, t)

# TODO: simplify with OptionUtil netLong/netShort
function extrinsicDiffs(oqs1::Vector{OptionQuote}, sp)
    toLong = [] # Tuple{Float64, Float64, OptionQuote, OptionQuote}[]
    toShort = [] # Tuple{Float64, Float64, OptionQuote, OptionQuote}[]
    for i in 1:(length(oqs1)-1)
        oq1, oq2 = (oqs1[i], oqs1[i+1])
        x1b, x1a = getExtrinsic(oq1, sp)
        x2b, x2a = getExtrinsic(oq2, sp)
        # x1b, x1a = (getBid(oq1), getAsk(oq1))
        # x2b, x2a = (getBid(oq2), getAsk(oq2))
        diffLong, diffShort = (x2b - x1a, x1b - x2a)
        strikeDiff = (getStrike(oq2) - getStrike(oq1))
        strikeMid = (getStrike(oq2) + getStrike(oq1))/2
        push!(toLong, (diffLong / strikeDiff, strikeMid, oq1, oq2, x2b, -x1a))
        push!(toShort, (diffShort / strikeDiff, strikeMid, oq1, oq2, x1b, -x2a))
    end
    return (toLong, toShort)
end

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

PROFIT_MIN = 0.02

fly(ex, w) = bflys(ex, C(w), C(w))
function bflys(ex::Int, width1::Currency=C(1), width2::Currency=C(1))
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

export lm
lm(oq, side) = LegMeta(Leg(getOption(oq), 1.0, side), getQuote(oq, side), getMeta(oq))

end