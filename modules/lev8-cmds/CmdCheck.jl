module CmdCheck
using SH, BaseTypes, SmallTypes, StratTypes
using CalcUtil
using CmdStrats

export pvals, rstats, rkel, rmet

rmet(i::Int) = calcMetrics(pvals(1), arv(i))

using Kelly, Bins
rkelly(i::Int) = rkelly(ar(i))
function rkelly(lr::Strat)
    buf = Bins.empty()
    getVals!(buf, lr)
    # risk = abs(minimum(buf))
    # buf ./= risk
    # @assert minimum(buf) == -1
    # Kelly.optimize(getVals(probs()[1]), buf)
    calcKelly(pvals(1), buf)
end

# restk(i::Int) = restk(pvals(1), arv(i))
# function restk(pv, v)
#     probWin = 0.0
#     valWin = 0.0
#     valLose = 0.0
#     pvs = probsVals(probs)
#     for i in binItr()
#         val = valAt(scss, i)
#         if val > 0
#             probWin += pvs[i]
#             valWin += val * pvs[i]
#         else
#             valLose += val * pvs[i]
#         end
#     end
#     multWin = -valWin / valLose
#     k = singleKelly(probWin, multWin)
#     @info "estKelly" k probWin multWin valWin valLose
# end

function rstats()
    alls = vcat(CmdStrats.lastSpreads2[][1], CmdStrats.lastSpreads2[][1])
    sc = 0 ; lc = 0 ; v = 0 ; ss = 0 ; ds = 0
    for spr in alls
        isVert(spr) && (v += 1)
        isCalLong(spr) && (lc += 1)
        isCalShort(spr) && (sc += 1)
        isSideSame(spr) && (ss += 1)
        isStyleDiff(spr) && (ds += 1)
    end
    @info "All spreads" v lc sc ss ds
    # @test v > 1000
    # @test lc > 1000
    # @test sc === 0
    # @test ss === 0
    # @test ds === 0
end

# isShortCal(lr1, lr2) =
#     (getExpiration(lr1) > getExpiration(lr2) && getSide(lr1) == Side.short) ||
#     (getExpiration(lr2) > getExpiration(lr1) && getSide(lr2) == Side.short)
# isLongCal(lr1, lr2) =
#     (getExpiration(lr1) > getExpiration(lr2) && getSide(lr1) == Side.long) ||
#     (getExpiration(lr2) > getExpiration(lr1) && getSide(lr2) == Side.long)
# isVert(lr1, lr2) = getExpiration(lr1) == getExpiration(lr2)
# isSameSide(lr1, lr2) = getSide(lr1) == getSide(lr2)
# isDiffStyle(lr1, lr2) = getStyle(lr1) != getStyle(lr2)


end