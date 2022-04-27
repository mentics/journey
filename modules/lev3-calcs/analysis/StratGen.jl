module StratGen
using Dates
using SH, BaseTypes, SmallTypes, OptionTypes, StratTypes, LegTypes, ChainTypes, LegMetaTypes, Rets
using Globals

export allSpreads

#==
Run with:
using Strats, Positions, Expirations, Markets, Chains
allSpreads(chains(), isConflict, marketPrices(), expirs()[1:2])
returns (callSpreads, putSpreads)
==#
allSpreads(chs::Dict{Date,OptionChain}, isConflict, (sp, cp)::NTuple{2,Currency}, exps)::Spreads2 = allToSpreads(allLegs(chs, isConflict, (sp, cp), exps))

#region Local
getCfg(sym::Symbol) = Globals.get(:Strats)[sym]

validOption(maxStrikeDist::Int, cp::Currency) = oq -> getBid(oq) >= 0.03 && abs(getStrike(oq) - cp) <= maxStrikeDist

# isConflict(oq, side), usually Positions.isConflict
fleg(isConflict, cp::Currency) = validLeg(isConflict, getCfg(:maxPutHeight), getCfg(:maxCallHeight), cp)
validLeg(isConflict, maxPutHeight::Int, maxCallHeight::Int, cp::Currency) =
    # ((oq::OptionQuote, side::Side.T),) -> begin
    ((oq, side),) -> begin
        if side == Side.short
            (getStyle(oq) == Style.put && getStrike(oq) > cp + maxPutHeight) && return false
            (getStyle(oq) == Style.call && getStrike(oq) < cp - maxCallHeight) && return false
        end
        isConflict(getOption(oq), side) && return false
        return true
    end

makeLegRet(oq::OptionQuote, side::Side.T, forDate::Date, sp::Currency, vtyRatio::Float64)::LegRet =
        ( lm = LegMeta(Leg(getOption(oq), 1.0, side), getQuote(oq, side), getMeta(oq)) ; (lm, makeRet(getLeg(lm), getMeta(lm), bap(lm), forDate, sp, vtyRatio)) )

forDates(chs::Dict{Date,OptionChain}, dates) = Iterators.flatten(Iterators.map(x->x.chain, chs[dt] for dt in dates))
function spreadItems!(fill, itr, funcs...)
    for x in itr
        for i in eachindex(funcs)
            if funcs[i](x)
                push!(fill[i], x)
                @goto next
            end
        end
        throw("No matching function in split")
        @label next
    end
    return fill
end

legCats() = (Vector{LegRet}(), Vector{LegRet}(), Vector{LegRet}(), Vector{LegRet}())
filtShortCals(forDate, opts) = Globals.get(:incShortCals) ? opts : Iterators.filter(o -> getExpiration(o) == forDate, opts)
itrOptSide(forDate::Date, opts) = Iterators.flatten((((o, Side.long) for o in opts), ((o, Side.short) for o in filtShortCals(forDate, opts))))
optSideToLeg(forDate::Date, sp::Currency, vtyRatio::Float64) = ((oq, side),) -> makeLegRet(oq, side, forDate, sp, vtyRatio)

# returns iterator of LegRet
itrLegRets(fleg, sp::Currency, forDate::Date, opts) = Iterators.map(optSideToLeg(forDate, sp, Globals.get(:vtyRatio)), Iterators.filter(fleg, itrOptSide(forDate, opts)))

# returns: (callsLong, putsLong, callsShort, putsShort)
legsFor(fleg, sp::Currency, forDate::Date, opts)::AllLegs4 =
    spreadItems!(legCats(),
                itrLegRets(fleg, sp, forDate, opts),
                isCallLong, isPutLong, isCallShort, isPutShort)

validOptions(cp::Currency, chs::Dict{Date,OptionChain}, exps) = Iterators.filter(validOption(getCfg(:maxStrikeDist), cp), forDates(chs, exps))

allLegs(chs::Dict{Date,OptionChain}, isConflict, (sp, cp)::NTuple{2,Currency}, exps)::AllLegs4 = legsFor(fleg(isConflict, cp), sp, minimum(exps), validOptions(cp, chs, exps))

# (callSpreads, putSpreads)
allToSpreads(all::AllLegs4)::Spreads2 = (spreads(all[1], all[3]), spreads(all[2], all[4]))

function spreads(v1, v2)
    # incShortCals = Globals.get(:incShortCals)
    estCnt = length(v1) * length(v2)
    res = Vector{Spread}(undef, estCnt)
    i = 0
    for x1 in v1, x2 in v2
        # TODO: this is messy
        # !incShortCals && isShortCal(x1, x2) && continue
        if !doLegsConflict(x1, x2)
            i += 1
            res[i] = getStrike(x1) < getStrike(x2) ? (x1, x2) : (x2, x1)
        end
    end
    # @info "spreads estCnt" estCnt i
    resize!(res, i)
    return res
end
# isShortCal(lr1, lr2) =
#     (getExpiration(lr1) > getExpiration(lr2) && getSide(lr1) == Side.short) ||
#     (getExpiration(lr2) > getExpiration(lr1) && getSide(lr2) == Side.short)

SH.getSide(lr::LegRet) = getSide(first(lr))
SH.getStyle(lr::LegRet) = getStyle(first(lr))
SH.getStrike(lr::LegRet) = getStrike(first(lr))
SH.getOption(lr::LegRet) = getOption(first(lr))
SH.getExpiration(lr::LegRet) = getExpiration(first(lr))

isCallLong(lr::LegRet) = getStyle(lr[1]) == Style.call && getSide(lr[1]) == Side.long
isCallShort(lr::LegRet) = getStyle(lr[1]) == Style.call && getSide(lr[1]) == Side.short
isPutLong(lr::LegRet) = getStyle(lr[1]) == Style.put && getSide(lr[1]) == Side.long
isPutShort(lr::LegRet) = getStyle(lr[1]) == Style.put && getSide(lr[1]) == Side.short
#endregion

end



# function ranges(lower::Vector{Spread}, upper::Vector{Spread})
#     res = Vector{UnitRange{Int64}}(undef, length(lower))
#     lenup = length(upper)
#     cnt = 0
#     start = 1
#     for i in 1:length(lower)
#         while getStrike(upper[start][1]) < getStrike(lower[i][2]) ; start += 1 end
#         push!(start:lenup)
#         cnt += lenup - start
#     end
#     return (res, cnt)
# end

# function divideWork(nthreads, num)
#     each = num / nthreads
#     res = Vector{UnitRange{Int64}}(undef, nthreads)
#     prev = 0
#     for i in 1:nthreads
#         next = round(Int, i * each)
#         res[i] = (prev+1):next
#         prev = next
#     end
#     return res
# end
