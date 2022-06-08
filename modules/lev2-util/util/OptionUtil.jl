module OptionUtil
using SH, BaseTypes, SmallTypes, ChainTypes, QuoteTypes

# export getMid
export calcNetLong, calcNetShort
export calcNetLongPerWidth, calcNetShortPerWidth
export calcExtrin
export calcNetLongExtrin, calcNetShortExtrin

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

end