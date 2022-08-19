module OptionUtil
using SH, BaseTypes, SmallTypes, ChainTypes, QuoteTypes

# export getMid
export calcNetLong, calcNetShort
export calcNetLongPerWidth, calcNetShortPerWidth
export calcExtrin, calcExtrins
export calcNetLongExtrin, calcNetShortExtrin
export ivTexToStdDev, texToYear


calcWidth(oq1, oq2) = abs(getStrike(oq2) - getStrike(oq1))
calcNetLong(oq1::OptionQuote, oq2::OptionQuote) = -max(0.01, getAsk(oq1)) + max(0., getBid(oq2))
calcNetShort(oq1::OptionQuote, oq2::OptionQuote) = max(0., getBid(oq1)) - max(0.01, getAsk(oq2))
calcNetLongPerWidth(oq1::OptionQuote, oq2::OptionQuote) = calcNetLong(oq1, oq2) / calcWidth(oq1, oq2)
calcNetShortPerWidth(oq1::OptionQuote, oq2::OptionQuote) = calcNetShort(oq1, oq2) / calcWidth(oq1, oq2)
function calcNetLongExtrin(oq1::OptionQuote, oq2::OptionQuote, curp::Real)::Currency
    _, a1, m1 = calcExtrin(oq1, curp)
    b2, _, m2 = calcExtrin(oq2, curp)
    # return -max(.01, (a1 + m1)/2) + max(0., (b2 + m2)/2) # TODO: always using 25%, should consider other improvs?
    return -max(.01, m1) + max(0., m2)
    # return -max(.01, a1) + max(0., b2)
end
# function calcNetShortExtrin(oq1::OptionQuote, oq2::OptionQuote, curp::Currency)::Currency
#     b1, _, m1 = calcExtrin(oq1, curp)
#     _, a2, m2 = calcExtrin(oq2, curp)
#     # return max(.0, (b1 + m1)/2) - max(0., (a2 + m2)/2) # TODO: always using 25%, should consider other improvs?
#     return max(.0, m1) - max(0., m2) # TODO: always using 25%, should consider other improvs?
# end

# getMid(q::Quote) = max(0., (q.bid + q.ask)/2)

function calcExtrin(oq::OptionQuote, curp::Real)::Tuple{Currency,Currency,Currency}
    bid = getBid(oq)
    ask = getAsk(oq)
    s = getStrike(oq)
    dist = abs(curp - s)
    return !xor(Style.call == getStyle(oq), s >= curp) ?
            (bid, ask, max(0., (bid + ask)/2)) : (bid - dist, ask - dist, max(0., (bid + ask)/2 - dist))
    # if Style.call == getStyle(oq)
    #     return s > curp ? (bid, ask, max(0., (bid + ask)/2)) : (bid - dist, ask - dist, max(0., (bid - dist + ask - dist))/2)
    # else
    #     return s < curp ? (bid, ask, max(0., (bid + ask)/2)) : (bid - dist, ask - dist, max(0., (bid - dist + ask - dist))/2)
    # end
end

function calcExtrins(oq::OptionQuote, curp::Real)::Tuple{Currency,Currency,Currency}
    bid = getBid(oq)
    ask = getAsk(oq)
    imp = bap(oq)
    dist = abs(curp - getStrike(oq))
    if extrinSub(getStyle(oq), getStrike(oq), curp)
        res = (bid - dist, ask - dist, imp - dist)
    else
        res = (bid, ask, imp)
    end
    foreach(res) do x
        if isnan(x) || x < 0.0
            @error "calcExtrins: unexpected < 0.0" x res
        end
    end
    return res
end

extrinSub(style::Style.T, strike::Real, curp::Real)::Bool = xor(Style.call == style, strike >= curp)

# ivToStdDev(iv::Float64, timeToExpY::Float64) = iv / sqrt(1.0/timeToExpY)
# ivTexToStdDevOld(iv::Float64, tex::Float64) = iv / sqrt(1.0/(tex/24/365))
ivTexToStdDev(iv::Float64, tex::Float64) = iv * sqrt(tex/texPerYear())
texPerYear() = 6.5 * 252 + .3 * (8760 - 6.5 * 252)
texToYear(tex) = tex / texPerYear()
# 9:30 am - 4 pm = 6.5 hours / day
# 252 days per year
# 8760 hours per year

legs2Extrema(legs::Coll) = spreadExtrema(longShort(legs[1], legs[2])...)
legs2Levels(legs::Coll) = spreadLevels(longShort(legs[1], legs[2])...)
spreadExtrema(legLong, legShort) = minmax(spreadLevels(legLong, legShort)...)
function spreadLevels(legLong, legShort)
    @assert getSide(legLong) == Side.long && getSide(legShort) == Side.short
    netOpen = getNetOpen(legLong) + getNetOpen(legShort)
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

function legs4Extrema(legs::Coll)
    # @assert getStrike(cond[1][2]) <= getStrike(cond[2][1]) "$(getStrike.(cond[1])) $(getStrike.(cond[2]))" # issorted(legs; by=getStrike)
    @assert issorted(legs; by=getStrike)
    levLeft = spreadLevels(longShort(legs[1], legs[2])...)
    levRight = spreadLevels(longShort(legs[3], legs[4])...)
    left = levLeft[1] + levRight[1]
    mid = levLeft[2] + levRight[1]
    right = levLeft[2] + levRight[2]
    # @info "condorExtrema" levLeft levRight left mid right
    return (left, mid, right)
end

longShort(leg1, leg2) = isLong(leg1) ? (leg1, leg2) : (leg2, leg1)

end