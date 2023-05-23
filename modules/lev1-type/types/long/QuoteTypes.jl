module QuoteTypes
using BaseTypes, SH, SmallTypes

export Quote, newQuote, sumQuotes

struct Quote
    bid::Currency
    ask::Currency
end
Quote(;bid=C(1.17), ask=C(1.19)) = Quote(bid, ask)
Quote(v::Currency) = Quote(v, v)
function newQuote(q::Quote, dir::DirSQA)::Quote # side::Side.T, qty::Integer, action::Action.T)::Quote
    bid = q.bid
    ask = q.ask
    # @assert getBid(q) >= 0 && getAsk(q) >= 0
    m = dirMult(dir)
    nq = m > 0 ? Quote(m * bid, m * ask) : Quote(m * ask, m * bid)
    # @assert getBid(nq) <= getAsk(nq) "Quote assert: bid was > ask $(q) -> $(nq), $(m)"
    if bid > ask
        nq = Quote(nq.ask, nq.bid)
        # TODO: do something about this?
        # println("WARN: QuoteTypes.newQuote: bid was > ask, swapping them: $(q) -> $(nq), $(m)")
        @assert nq.bid < nq.ask
    end
    return nq
end

# Quote(quot::Quote, side::Side.T) = Quote(getAction(quot), side, getBid(quot), getAsk(quot))
# Quote(quot::Quote, action::Action.T, side::Side.T) = Quote(action, side, getBid(quot), getAsk(quot))
# function Quote(action::Action.T, side::Side.T, bid::Currency, ask::Currency)
#     (mn, mx) = minmax(abs(bid), abs(ask))
#     return Int(action) * Int(side) == 1 ? Quote(action, -mx, -mn) : Quote(action, mn, mx)
# end
SH.getBid(q::Quote) = q.bid
SH.getAsk(q::Quote) = q.ask
# Base.abs(q::Quote) = Quote(q.action, abs(q.bid), abs(q.ask))
Base.show(io::IO, q::Quote) = print(io, q.bid === q.ask ? "($(q.bid))" : "($(q.bid), $(q.ask))")
# Base.:(+)(q::Quote, addend::Real) = Quote(q.action, q.bid + addend, q.ask + addend)
# Base.:(+)(addend::Real, q::Quote) = Quote(q.bid + addend, q.ask + addend)
# Base.:(-)(q1::Quote, q2::Quote) = Quote(q1.bid - q2.bid, q1.ask - q2.ask)
# Base.:(*)(mult::Float64, q::Quote) = Quote(mult*q.bid, mult*q.ask)
SH.getQuote(qt::Quote) = qt

# SH.getQuote(itr) = sumQuotes(map(getQuote, itr))

function sumQuotes(qs)
    b = 0.0
    a = 0.0
    for q in qs
        b += getBid(q)
        a += getAsk(q)
    end
    return Quote(b, a)
end

function sumQuotes(qs::NTuple{4,Quote})
    return Quote(qs[1].bid + qs[2].bid + qs[3].bid + qs[4].bid, qs[1].ask + qs[2].ask + qs[3].ask + qs[4].ask)
end

end