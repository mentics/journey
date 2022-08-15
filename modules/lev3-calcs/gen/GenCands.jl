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

function iterSpreads(f::Function, oqss::Oqss, args...)
    res = iterSpreads(f, oqss.call, args...)
    res = iterSpreads(f, oqss.put, args...)
end

function iterSpreads(f::Function, oqss::Oqss, strikeMin::Currency, args...)
    res = iterSpreads(f, oqss.call, strikeMin, args...)
    res = iterSpreads(f, oqss.put, strikeMin, args...)
end

function iterCondors(f::Function, oqss::Oqss, args...)
    @sync iterSpreads(oqss, args...) do (leg1, leg2), args...
        Threads.@spawn iterSpreads(oqss, maxStrike(leg1, leg2), args...) do (leg3, leg4), args...
    # iterSpreads(oqss, args...) do l12, args...
    #     iterSpreads(oqss, maxStrike(leg1, leg2), args...) do l34, args...
            legs = (leg1, leg2, leg3, leg4)
            # legs = (l12[1], l12[2], l34[1], l34[2])
            # TODO: ?
            # _, mx = condorExtrema(legs)
            # mx > 0.0 || return

            @assert issorted(legs; by=getStrike)
            f(legs, args...)
        end
    end
end

#region Local
function iterSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, strikeMin::Currency, args...)
    for oq1 in oqs.long, oq2 in oqs.short
        (getStrike(oq1) >= strikeMin && getStrike(oq2) >= strikeMin && oq1 != oq2) || continue
        legLong = to(LegMeta, oq1, Side.long)
        legShort = to(LegMeta, oq2, Side.short)
        _, mx = spreadExtrema(legLong, legShort)
        mx > 0.0 || continue
        spr = getStrike(legLong) < getStrike(legShort) ? (legLong, legShort) : (legShort, legLong)
        f(spr, args...)
    end
end

function iterSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, args...)
    for oq1 in oqs.long, oq2 in oqs.short
        oq1 != oq2 || continue
        legLong = to(LegMeta, oq1, Side.long)
        legShort = to(LegMeta, oq2, Side.short)
        # global lms = [legLong, legShort]
        # netOpen = bap(leg1) + bap(leg2)
        # # strikeDiff = abs(getStrike(oq1) - getStrike(oq2)) + netOpen
        # left = netOpen
        # # right side is strike(short - long) + netopen
        # right = getStrike(oq2) - getStrike(oq1) + netOpen
        # if sign(getStrike(oq2) - getStrike(oq1)) * sign(netOpen) > 0
        #     @info "iterSpreads sign mismatch" left right getStrike(oq2) getStrike(oq1) netOpen sign(getStrike(oq2) - getStrike(oq1)) sign(netOpen) getQuote(leg1) getQuote(leg2)
        # end
        # # if max(left, right) < 0.0
        # #     @info "iterSpreads too low" left right
        # # end
        _, mx = spreadExtrema(legLong, legShort)
        mx > 0.0 || continue
        # max(left, right) > 0.0 || continue

        # TODO: optimize?
        spr = getStrike(legLong) < getStrike(legShort) ? (legLong, legShort) : (legShort, legLong)
        f(spr, args...)
    end
end

function spreadExtrema(legLong, legShort)
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
        return minmax(left, right)
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
        return minmax(left, right)
    end
end

function condorExtrema(legs::Coll)
# TODO: ?
end

function maxStrike(hasStrike1, hasStrike2)
    s1 = getStrike(hasStrike1)
    s2 = getStrike(hasStrike2)
    return max(s1, s2)
end
#endregion

end