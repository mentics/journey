module TypeOutputs
using BaseTypes
using OutputUtil

#region BaseTypes
function Base.show(io::IO, x::Currency)
    print(io, x)
end
function Base.show(io::IO, x::PT)
    print(io, x)
end
function Base.show(io::IO, ::Type{Currency})
    print(io, "Currency")
end
function Base.show(io::IO, ::Type{PT})
    print(io, "Price")
end
#endregion BaseTypes

#region
import OptionTypes
Base.show(io::IO, o::OptionTypes.Option) = print(io, "Opt($(o.expiration) $(o.style) $(o.strike))")

import QuoteTypes
Base.show(io::IO, q::QuoteTypes.Quote) = print(io, q.bid === q.ask ? "Q($(q.bid))" : "Q($(q.bid), $(q.ask))")
#endregion

#region Legs
import LegTypes
Base.show(io::IO, leg::LegTypes.Leg) = print(io, "Leg($(disp(leg.quantity)) $(leg.side) $(leg.option))")

import LegQuoteTypes
Base.show(io::IO, lm::LegQuoteTypes.LegQuote{T}) where T = print(io, "LM($(T) $(lm.leg) $(lm.quot))")
Base.show(io::IO, ::Type{LegQuoteTypes.Open}) = print(io, "Open")
Base.show(io::IO, ::Type{LegQuoteTypes.Close}) = print(io, "Close")
#endregion Legs

#region Collections
Base.show(io::IO, lms::NTuple{1,LegQuoteTypes.LegQuote{T}}) where {T} = show_legs(io, lms)
Base.show(io::IO, lms::NTuple{2,LegQuoteTypes.LegQuote{T}}) where {T} = show_legs(io, lms)
Base.show(io::IO, lms::NTuple{3,LegQuoteTypes.LegQuote{T}}) where {T} = show_legs(io, lms)
Base.show(io::IO, lms::NTuple{4,LegQuoteTypes.LegQuote{T}}) where {T} = show_legs(io, lms)
function show_legs(io, lms)
    if N == 0
        println(io, "ODD EMPTY TUPLE")
        return
    end
    println(io, "( ", lms[1])
    for lm in lms[2:end-1]
        println(io, "  ", lm)
    end
    if N > 1
        print(io, "  ", lms[end])
    end
    print(io, " )")
end
#endregion Collections

#region Other
import Lines
Base.show(io::IO, x::Lines.Segment) = print(io, "Seg{$(x.left) -> $(x.right)}")
Base.show(io::IO, x::Lines.Point) = print(io, "Pt($(r5(x.x)),$(r5(x.y)))")

import OptionMetaTypes
Base.show(io::IO, x::OptionMetaTypes.Greeks) = print(io, "Greeks(delta=$(x.delta), gamma=$(x.gamma), theta=$(x.theta), vega=$(x.vega), phi=$(x.phi), rho=$(x.rho))")

import StatusTypes
Base.show(io::IO, x::Type{<:StatusTypes.Status}) = print(io, x.name.name)
#endregion Other

end