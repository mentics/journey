module GenCands
using ThreadPools
using SH, BaseTypes, SmallTypes, LegMetaTypes
using OptionUtil, LogUtil, ThreadUtil
import CalcUtil
using ChainTypes
using Rets, StratTypes

const MinSpreadMx = 0.03

# TODO: consider including cash secured puts?
function iterSingle(f::Function, oqss::Oqss, args...)
    for oq in oqss.call.long
        f([to(LegMeta, oq, Side.long)], args...)
    end

    for oq in oqss.put.long
        f([to(LegMeta, oq, Side.long)], args...)
    end
end

function iterSpreads(f::Function, oqss::Oqss, maxSpreadWidth::Real, args...)
    iterSpreads(f, oqss.call, maxSpreadWidth, args...)
    iterSpreads(f, oqss.put, maxSpreadWidth, args...)
end

function paraSpreads(f::Function, oqss::Oqss, maxSpreadWidth::Real, isLegAllowed, args...)
    paraSpreads(f, oqss.call, maxSpreadWidth, isLegAllowed, args...)
    paraSpreads(f, oqss.put, maxSpreadWidth, isLegAllowed, args...)
end

# function iterSpreads(f::Function, oqss::Oqss, strikeMin::Currency, args...)::Bool
#     iterSpreads(f, oqss.call, strikeMin, args...) || return false
#     iterSpreads(f, oqss.put, strikeMin, args...) || return false
#     return true
# end

function iterCondors(f::Function, oqss::Oqss, maxSpreadWidth::Real, curp::Currency, isLegAllowed, args...)
    # @warn "Still using left > 0.0 || continue"
    spreads = Vector{Spread}()
    iterSpreads(oqss, maxSpreadWidth, isLegAllowed, args...) do (lm1, lm2), args...
        ret1 = makeRet(lm1, bap(lm1), curp)
        ret2 = makeRet(lm2, bap(lm2), curp)
        push!(spreads, ((lm1, ret1), (lm2, ret2)))
        return true
    end
    # TODO: investigate?
    if length(spreads) <= 4
        return true
    end
    @assert length(spreads) > 4 string("Not enough spreads: ", length(spreads), ' ', getExpir(first(SmallTypes.iter(oqss))))
    # widths = strikeWidth.(map(x -> (x[1][1], x[2][1]), spreads))
    # @error "iterCondors" maxSpreadWidth curp
    # error(maximum(widths))
    @log debug "Number of spreads" length(spreads)
    MaxSpreads = 99999999

    lok = ReentrantLock()

    twith(ThreadPools.QueuePool(2, Threads.nthreads()-1)) do pool
        @tthreads pool for i in 1:min(MaxSpreads, length(spreads))
            for j in (i+1):min(MaxSpreads, length(spreads))
                s1 = spreads[i]
                s2 = spreads[j]
                str12 = getStrike(s1[2])
                str21 = getStrike(s2[1])
                str12 <= str21 || continue

                # (str12 != str21 || getSide(s1[2]) == getSide(s2[1]))
                if !(str12 != str21 || getSide(s1[2]) == getSide(s2[1]))
                    # runSync(lok) do
                    #     println("Found legs that cancel out")
                    #     @show s1[2][1] s2[1][1]
                    # end
                    # TODO: is there some way to change spread gen to avoid this case?
                    continue
                end

                cond = (spreads[i], spreads[j])
                left, mid, right = OptionUtil.legsExtrema(map(x -> x[1], (cond[1]..., cond[2]...)))
                max(left, mid, right) > 0.02 || continue
                # left > 0.0 || continue

                # @assert issorted(legs; by=getStrike) "Not sorted $(legs)" # already checked in condorExtrema
                # d1 = abs(-(getStrike.(spreads[i])...))
                # d2 = abs(-(getStrike.(spreads[j])...))
                # if d1 > 10 || d2 > 10
                #     @error "too wide" d1 d2 lms(cond)
                #     error("What happened?")
                # end
                f(cond, args...) || return false
            end
        end
    end
    return true

    #         iterSpreads(oqss, maxSpreadWidth, args...) do (leg1, leg2), args...
    # # paraSpreads(oqss, maxSpreadWidth, args...) do (leg1, leg2), args...
    #         iterSpreads(oqss, maxSpreadWidth, maxStrike(leg1, leg2), args...) do (leg3, leg4), args...
    # # iterSpreads(oqss, args...) do l12, args...
    # #     iterSpreads(oqss, maxStrike(leg1, leg2), args...) do l34, args...
    #         legs = (leg1, leg2, leg3, leg4)
    #         max(condorExtrema(legs)...) > 0.0 || return true

    #         @assert issorted(legs; by=getStrike) "Not sorted $(legs)"
    #         return f(legs, args...)
    #     end
    #     return true
    # end
end

#region Local
# function iterSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, maxSpreadWidth::Currency, strikeMin::Currency, args...)::Bool
#     for oq1 in Iterators.filter(oq -> getStrike(oq) >= strikeMin, oqs.long), oq2 in Iterators.filter(oq -> getStrike(oq) >= strikeMin, oqs.short)
#         strikeWidth(oq1, oq2) <= maxSpreadWidth || continue
#         (getStrike(oq1) >= strikeMin && getStrike(oq2) >= strikeMin && oq1 != oq2) || continue
#         legLong = to(LegMeta, oq1, Side.long)
#         legShort = to(LegMeta, oq2, Side.short)
#         _, mx = spreadExtrema(legLong, legShort)
#         mx > 0.0 || continue
#         spr = getStrike(legLong) < getStrike(legShort) ? (legLong, legShort) : (legShort, legLong)
#         f(spr, args...) || return false
#     end
#     return true
# end

function iterSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, maxSpreadWidth::Real, isLegAllowed, args...)::Bool
    for oq1 in oqs.long, oq2 in oqs.short
        strikeWidth(oq1, oq2) <= maxSpreadWidth || continue
        oq1 != oq2 || continue
        # println("check ", isLegAllowed(oq1, Side.long))
        (isLegAllowed(oq1, Side.long) && isLegAllowed(oq2, Side.short)) || continue
        legLong = to(LegMeta, oq1, Side.long)
        legShort = to(LegMeta, oq2, Side.short)
        _, mx = OptionUtil.spreadExtrema(legLong, legShort)
        mx >= MinSpreadMx || continue
        spr = getStrike(legLong) < getStrike(legShort) ? (legLong, legShort) : (legShort, legLong)
        f(spr, args...) || return false
    end
    return true
end

function paraSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, maxSpreadWidth::Real, isLegAllowed, args...)::Bool
    !isempty(oqs.long) || return true
    finish = false
    Threads.@threads for oq1 in oqs.long
        thid = Threads.threadid()
        for oq2 in oqs.short
            strikeWidth(oq1, oq2) <= maxSpreadWidth || continue
            # (getStrike(oq1) >= strikeMin && getStrike(oq2) >= strikeMin && oq1 != oq2) || continue
            oq1 != oq2 || continue
            (isLegAllowed(oq1, Side.long) && isLegAllowed(oq2, Side.short)) || continue
            legLong = to(LegMetaOpen, oq1, Side.long)
            legShort = to(LegMetaOpen, oq2, Side.short)
            # _, mx = OptionUtil.spreadExtrema(legLong, legShort)
            _, mx = oextrema(legLong, legShort)
            mx >= MinSpreadMx || continue
            spr = getStrike(legLong) < getStrike(legShort) ? (legLong, legShort) : (legShort, legLong)
            # f(spr, args...) || ( finish = stop ; break )
            f(thid, spr, args...) || return false
        end
        finish || break
    end
    return finish
end

function oextrema(os...)
    extrema(os) do o
        getStrike(o)
    end
end

# function iterSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, args...)::Bool
#     for oq1 in oqs.long, oq2 in oqs.short
#         oq1 != oq2 || continue
#         legLong = to(LegMeta, oq1, Side.long)
#         legShort = to(LegMeta, oq2, Side.short)
#         # lms = [legLong, legShort]
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

function maxStrike(hasStrike1, hasStrike2)
    s1 = getStrike(hasStrike1)
    s2 = getStrike(hasStrike2)
    return max(s1, s2)
end

strikeWidth(s::NTuple{2,LegMeta}) = strikeWidth(s...)
function strikeWidth(hasStrike1, hasStrike2)
    s1 = getStrike(hasStrike1)
    s2 = getStrike(hasStrike2)
    return abs(s1 - s2)
end
#endregion

end