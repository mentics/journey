module QuoteTypes
using BaseTypes, SH, SmallTypes

export Quote

struct Quote
    action::Action.T
    bid::Currency
    ask::Currency
end
Quote(;action=Action.open, bid=C(1.17), ask=C(1.19)) = Quote(action, bid, ask)
Quote(action::Action.T, v::Currency) = Quote(action, v, v)
Quote(c1::Currency, c2::Currency) = Quote(Action.open, c1, c2)

Quote(quot::Quote, side::Side.T) = Quote(getAction(quot), side, getBid(quot), getAsk(quot))
Quote(quot::Quote, action::Action.T, side::Side.T) = Quote(action, side, getBid(quot), getAsk(quot))
function Quote(action::Action.T, side::Side.T, bid::Currency, ask::Currency)
    (mn, mx) = minmax(abs(bid), abs(ask))
    return Int(action) * Int(side) == 1 ? Quote(action, -mx, -mn) : Quote(action, mn, mx)
end
SH.getAction(q::Quote) = q.action
SH.getBid(q::Quote) = q.bid
SH.getAsk(q::Quote) = q.ask
Base.show(io::IO, q::Quote) = print(io, (q.action == Action.open ? "Qo" : "Qc") * (q.bid === q.ask ? "($(q.bid))" : "($(q.bid), $(q.ask))"))

# TODO: Move these
export sumQuotes, improve
improve(q::Quote, r::Float64)::Currency = getBid(q) + r * (getAsk(q) - getBid(q))

function sumQuotes(qs)
    b = 0.0
    a = 0.0
    act = getAction(first(qs))
    for q in qs
        @assert getAction(q) == act
        b += getBid(q)
        a += getAsk(q)
    end
    return Quote(act, b, a)
end

end