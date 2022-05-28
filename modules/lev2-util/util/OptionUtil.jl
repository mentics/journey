module OptionUtil
using SH, BaseTypes, SmallTypes, ChainTypes, QuoteTypes

export getMid, netLong, netShort, getExtrinsic

netLong(oq1::OptionQuote, oq2::OptionQuote) = -max(0.01, getAsk(oq1)) + max(0., getBid(oq2))
netShort(oq1::OptionQuote, oq2::OptionQuote) = max(0., getBid(oq1)) - max(0.01, getAsk(oq2))

getMid(q::Quote) = max(0., (q.bid + q.ask)/2)

function getExtrinsic(oq::OptionQuote, curp::Currency)::Tuple{Currency,Currency,Currency}
    bid = getBid(oq)
    ask = getAsk(oq)
    s = getStrike(oq)
    dist = abs(curp - s)
    if Style.call == getStyle(oq)
        return s > curp ? (bid, ask, max(0., (bid + ask)/2)) : (bid - dist, ask - dist, max(0., (bid - dist + ask - dist))/2)
    else
        return s < curp ? (bid, ask, max(0., (bid + ask)/2)) : (bid - dist, ask - dist, max(0., (bid - dist + ask - dist))/2)
    end
end

end