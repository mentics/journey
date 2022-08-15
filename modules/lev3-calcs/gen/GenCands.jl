module GenCands
using SH, BaseTypes, SmallTypes, LegMetaTypes
using ChainTypes

function iterSingle(f::Function, oqss::Oqss, args...)
    for oq in oqss.call.long
        f([to(LegMeta, oq, Side.long)], args...)
    end

    for oq in oqss.put.long
        f([to(LegMeta, oq, Side.long)], args...)
    end
end

function iterSpreads(f::Function, maxSpreadWidth::Currency, oqss::Oqss, args...)
    iterSpreads(f, oqss.call, maxSpreadWidth, args...)
    iterSpreads(f, oqss.put, maxSpreadWidth, args...)
end

function paraSpreads(f::Function, oqss::Oqss, maxSpreadWidth::Currency, args...)
    paraSpreads(f, oqss.call, maxSpreadWidth, args...)
    paraSpreads(f, oqss.put, maxSpreadWidth, args...)
end

function iterSpreads(f::Function, oqss::Oqss, strikeMin::Currency, args...)::Bool
    iterSpreads(f, oqss.call, strikeMin, args...) || return false
    iterSpreads(f, oqss.put, strikeMin, args...) || return false
    return true
end

function iterCondors(f::Function, oqss::Oqss, maxSpreadWidth::Currency, args...)
    # paraSpreads(oqss, maxSpreadWidth, args...) do (leg1, leg2), args...
    paraSpreads(oqss, maxSpreadWidth, args...) do (leg1, leg2), args...
            iterSpreads(oqss, maxSpreadWidth, maxStrike(leg1, leg2), args...) do (leg3, leg4), args...
    # iterSpreads(oqss, args...) do l12, args...
    #     iterSpreads(oqss, maxStrike(leg1, leg2), args...) do l34, args...
            legs = (leg1, leg2, leg3, leg4)
            max(condorExtrema(legs)...) > 0.0 || return true

            @assert issorted(legs; by=getStrike) "Not sorted $(legs)"
            return f(legs, args...)
        end
        return true
    end
end

#region Local
function iterSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, maxSpreadWidth::Currency, strikeMin::Currency, args...)::Bool
    for oq1 in Iterators.filter(oq -> getStrike(oq) >= strikeMin, oqs.long), oq2 in Iterators.filter(oq -> getStrike(oq) >= strikeMin, oqs.short)
        strikeWidth(oq1, oq2) <= maxSpreadWidth || continue
        (getStrike(oq1) >= strikeMin && getStrike(oq2) >= strikeMin && oq1 != oq2) || continue
        legLong = to(LegMeta, oq1, Side.long)
        legShort = to(LegMeta, oq2, Side.short)
        _, mx = spreadExtrema(legLong, legShort)
        mx > 0.0 || continue
        spr = getStrike(legLong) < getStrike(legShort) ? (legLong, legShort) : (legShort, legLong)
        f(spr, args...) || return false
    end
    return true
end

function iterSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, maxSpreadWidth::Currency, args...)::Bool
    for oq1 in oqs.long, oq2 in oqs.short
        strikeWidth(oq1, oq2) <= maxSpreadWidth || continue
        oq1 != oq2 || continue
        legLong = to(LegMeta, oq1, Side.long)
        legShort = to(LegMeta, oq2, Side.short)
        _, mx = spreadExtrema(legLong, legShort)
        mx > 0.0 || continue
        spr = getStrike(legLong) < getStrike(legShort) ? (legLong, legShort) : (legShort, legLong)
        f(spr, args...) || return false
    end
    return true
end

using ThreadPools
function paraSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, maxSpreadWidth::Currency, args...)::Bool
    finish = false
    @qbthreads for oq1 in oqs.long
        for oq2 in oqs.short
            strikeWidth(oq1, oq2) <= maxSpreadWidth || continue
            # (getStrike(oq1) >= strikeMin && getStrike(oq2) >= strikeMin && oq1 != oq2) || continue
            oq1 != oq2 || continue
            legLong = to(LegMeta, oq1, Side.long)
            legShort = to(LegMeta, oq2, Side.short)
            _, mx = spreadExtrema(legLong, legShort)
            mx > 0.0 || continue
            spr = getStrike(legLong) < getStrike(legShort) ? (legLong, legShort) : (legShort, legLong)
            f(spr, args...) || ( finish = stop ; break )
        end
        finish || break
    end
    return finish
end

# function iterSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, args...)::Bool
#     for oq1 in oqs.long, oq2 in oqs.short
#         oq1 != oq2 || continue
#         legLong = to(LegMeta, oq1, Side.long)
#         legShort = to(LegMeta, oq2, Side.short)
#         # global lms = [legLong, legShort]
#         # netOpen = bap(leg1) + bap(leg2)
#         # # strikeDiff = abs(getStrike(oq1) - getStrike(oq2)) + netOpen
#         # left = netOpen
#         # # right side is strike(short - long) + netopen
#         # right = getStrike(oq2) - getStrike(oq1) + netOpen
#         # if sign(getStrike(oq2) - getStrike(oq1)) * sign(netOpen) > 0
#         #     @info "iterSpreads sign mismatch" left right getStrike(oq2) getStrike(oq1) netOpen sign(getStrike(oq2) - getStrike(oq1)) sign(netOpen) getQuote(leg1) getQuote(leg2)
#         # end
#         # # if max(left, right) < 0.0
#         # #     @info "iterSpreads too low" left right
#         # # end
#         _, mx = spreadExtrema(legLong, legShort)
#         mx > 0.0 || continue
#         # max(left, right) > 0.0 || continue

#         # TODO: optimize?
#         spr = getStrike(legLong) < getStrike(legShort) ? (legLong, legShort) : (legShort, legLong)
#         f(spr, args...) || return false
#     end
#     return true
# end

spreadExtrema(legLong, legShort) = minmax(spreadLevels(legLong, legShort)...)
function spreadLevels(legLong, legShort)
    netOpen = bap(legLong) + bap(legShort)
    if getStyle(legLong) == Style.call
        left = netOpen
        sd = getStrike(legShort) - getStrike(legLong)
        right = sd + netOpen
        # if sign(sd) * sign(netOpen) > 0
        #     @info "iterSpreads call sign mismatch" left right getStrike(legLong) getStrike(legShort) netOpen sign(sd) sign(netOpen) getQuote(legLong) getQuote(legShort)
        # end
        # if max(left, right) < 0.0
        #     @info "iterSpreads too low" left right
        # end
    else
        sd = getStrike(legLong) - getStrike(legShort)
        left = sd + netOpen
        right = netOpen
        # if sign(sd) * sign(netOpen) > 0
        #     @info "iterSpreads put sign mismatch" left right getStrike(legLong) getStrike(legShort) netOpen sign(sd) sign(netOpen) getQuote(legLong) getQuote(legShort)
        # end
        # if max(left, right) < 0.0
        #     @info "iterSpreads too low" left right
        # end
    end
    return (left, right)
end

function condorExtrema(legs::Coll)
    @assert issorted(legs; by=getStrike)
    levLeft = spreadLevels(longShort(legs[1], legs[2])...)
    levRight = spreadLevels(longShort(legs[3], legs[4])...)
    left = levLeft[1] + levRight[1]
    mid = levLeft[2] + levRight[1]
    right = levLeft[2] + levRight[2]
    # @info "condorExtrema" levLeft levRight left mid right
    return (left, mid, right)
end

function maxStrike(hasStrike1, hasStrike2)
    s1 = getStrike(hasStrike1)
    s2 = getStrike(hasStrike2)
    return max(s1, s2)
end

function strikeWidth(hasStrike1, hasStrike2)
    s1 = getStrike(hasStrike1)
    s2 = getStrike(hasStrike2)
    return abs(s1 - s2)
end

longShort(leg1, leg2) = isLong(leg1) ? (leg1, leg2) : (leg2, leg1)
#endregion

end