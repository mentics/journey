module OptionUtil
using SH, BaseTypes, SmallTypes, OptionQuoteTypes

# calcNetLong(oq1::OptionQuote, oq2::OptionQuote) = -max(0.01, getAsk(oq1)) + max(0., getBid(oq2))
# calcNetShort(oq1::OptionQuote, oq2::OptionQuote) = max(0., getBid(oq1)) - max(0.01, getAsk(oq2))
# calcNetLongPerWidth(oq1::OptionQuote, oq2::OptionQuote) = calcNetLong(oq1, oq2) / calcWidth(oq1, oq2)
# calcNetShortPerWidth(oq1::OptionQuote, oq2::OptionQuote) = calcNetShort(oq1, oq2) / calcWidth(oq1, oq2)
# function calcNetLongExtrin(oq1::OptionQuote, oq2::OptionQuote, curp::Real)::Currency
#     _, a1, m1 = calcExtrin(oq1, curp)
#     b2, _, m2 = calcExtrin(oq2, curp)
#     # return -max(.01, (a1 + m1)/2) + max(0., (b2 + m2)/2) # TODO: always using 25%, should consider other improvs?
#     return -max(.01, m1) + max(0., m2)
#     # return -max(.01, a1) + max(0., b2)
# end
# # function calcNetShortExtrin(oq1::OptionQuote, oq2::OptionQuote, curp::Currency)::Currency
# #     b1, _, m1 = calcExtrin(oq1, curp)
# #     _, a2, m2 = calcExtrin(oq2, curp)
# #     # return max(.0, (b1 + m1)/2) - max(0., (a2 + m2)/2) # TODO: always using 25%, should consider other improvs?
# #     return max(.0, m1) - max(0., m2) # TODO: always using 25%, should consider other improvs?
# # end

# # function calcExtrin(oq::OptionQuote, curp::Real)::Tuple{Currency,Currency,Currency}
# function calc_extrin(oq::OptionQuote, curp::Real)::Tuple{Currency,Currency}
#     bid = getBid(oq)
#     ask = getAsk(oq)
#     s = getStrike(oq)
#     dist = abs(curp - s)
#     # return !xor(Style.call == getStyle(oq), s >= curp) ?
#     #         (bid, ask, max(0., (bid + ask)/2)) : (bid - dist, ask - dist, max(0., (bid + ask)/2 - dist))
#     return !xor(Style.call == getStyle(oq), s >= curp) ?
#             (bid, ask) : (bid - dist, ask - dist)
#     # if Style.call == getStyle(oq)
#     #     return s > curp ? (bid, ask, max(0., (bid + ask)/2)) : (bid - dist, ask - dist, max(0., (bid - dist + ask - dist))/2)
#     # else
#     #     return s < curp ? (bid, ask, max(0., (bid + ask)/2)) : (bid - dist, ask - dist, max(0., (bid - dist + ask - dist))/2)
#     # end
# end

calc_extrin(oq::OptionQuote, curp::Real)::Tuple{Currency,Currency} = calc_extrin(getStyle(oq), getStrike(oq), getBid(oq), getAsk(oq), curp)

import LegQuoteTypes
function calc_extrin(lq::LegQuoteTypes.LegQuote, curp::Real)::Tuple{Currency,Currency}
    q = LegQuoteTypes.getOrigQuote(lq)
    bid = getBid(q)
    ask = getAsk(q)
    @assert bid > 0
    @assert ask > 0
    calc_extrin(getStyle(lq), getStrike(lq), bid, ask, curp)
end

function calc_extrin(style::Style.T, strike::Currency, bid::Currency, ask::Currency, curp::Real)::Tuple{Currency,Currency}
    dist = abs(curp - strike)
    return xor(Style.call == style, strike >= curp) ?
                (bid - dist, ask - dist) : (bid, ask)
end

# function extrin_call2(curp, strike, bid, ask)
#     dist = (strike .<= curp) .* abs.(curp .- strike)
#     return (bid .- dist, ask .- dist)
# end

function calc_extrin(style, curp::Real, strike::Real, bid::Real, ask::Real)
    s = extrin_sub_dist(style, strike, curp)
    return ((bid + ask) / 2) - (s * abs(strike - curp))
end

function calc_extrin(style, curps, strikes, bids, asks)
    ss = extrin_sub_dist.(style, strikes, curps)
    return ((bids .+ asks) ./ 2) .- (ss .* abs.(strikes .- curps))
end

function extrin_call(curp, strike, bid, ask)
    return ((bid .+ ask) ./ 2) .- ((strike .<= curp) .* abs.(curp .- strike))
end

function extrin_put(curp, strike, bid, ask)
    return ((bid .+ ask) ./ 2) .- ((strike .>= curp) .* abs.(curp .- strike))
end

# function calcExtrins(oq::OptionQuote, curp::Real)::Tuple{Currency,Currency,Currency}
#     bid = getBid(oq)
#     ask = getAsk(oq)
#     imp = bap(oq, 0.0) # TODO: is 0.0 right here?
#     dist = abs(curp - getStrike(oq))
#     if extrinSub(getStyle(oq), getStrike(oq), curp)
#         res = (bid - dist, ask - dist, imp - dist)
#     else
#         res = (bid, ask, imp)
#     end
#     foreach(res) do x
#         if isnan(x) || x < 0.0
#             @error "calcExtrins: unexpected < 0.0" x res
#         end
#     end
#     return res
# end

extrin_sub_dist(style::Style.T, strike::Real, curp::Real)::Bool = xor(Style.call == style, strike >= curp)

#region OldExtrema
# legsExtrema(neto, legs::NTuple{2}) = spreadExtrema(neto, longShort(legs[1], legs[2])...)
# legs2Levels(neto, legs::Coll) = spreadLevels(neto, longShort(legs[1], legs[2])...)
# spreadExtrema(neto, legLong, legShort) = minmax(spreadLevels(neto, legLong, legShort)...)
# function spreadLevels(neto, legLong, legShort)
#     @assert getSide(legLong) == Side.long && getSide(legShort) == Side.short
#     @assert getStyle(legLong) == getStyle(legShort)
#     # neto = getNetOpen(legLong) + getNetOpen(legShort)
#     if getStyle(legLong) == Style.call
#         left = neto
#         sd = getStrike(legShort) - getStrike(legLong)
#         right = sd + neto
#         # if sign(sd) * sign(netOpen) > 0
#         #     @info "iterSpreads call sign mismatch" left right getStrike(legLong) getStrike(legShort) netOpen sign(sd) sign(netOpen) getQuote(legLong) getQuote(legShort)
#         # end
#         # if max(left, right) < 0.0
#         #     @info "iterSpreads too low" left right
#         # end
#     else
#         sd = getStrike(legLong) - getStrike(legShort)
#         left = sd + neto
#         right = neto
#         # if sign(sd) * sign(netOpen) > 0
#         #     @info "iterSpreads put sign mismatch" left right getStrike(legLong) getStrike(legShort) netOpen sign(sd) sign(netOpen) getQuote(legLong) getQuote(legShort)
#         # end
#         # if max(left, right) < 0.0
#         #     @info "iterSpreads too low" left right
#         # end
#     end
#     return (left, right)
# end

# function legsExtrema(neto, legs::NTuple{4})
#     # @assert getStrike(cond[1][2]) <= getStrike(cond[2][1]) "$(getStrike.(cond[1])) $(getStrike.(cond[2]))" # issorted(legs; by=getStrike)
#     @assert issorted(legs; by=getStrike)
#     levLeft = spreadLevels(neto/2, longShort(legs[1], legs[2])...)
#     levRight = spreadLevels(neto/2, longShort(legs[3], legs[4])...)
#     # println(levLeft, levRight)
#     left = levLeft[1] + levRight[1]
#     mid = levLeft[2] + levRight[1]
#     right = levLeft[2] + levRight[2]
#     # @info "condorExtrema" levLeft levRight left mid right
#     return (left, mid, right)
# end
# legsExtrema(neto, l1, l2) = legsExtrema(neto, (l1, l2))
# legsExtrema(neto, l1, l2, l3, l4) = legsExtrema(neto, (l1, l2, l3, l4))
#endregion

# longShort(leg1, leg2) = isLong(leg1) ? (leg1, leg2) : (leg2, leg1)

end