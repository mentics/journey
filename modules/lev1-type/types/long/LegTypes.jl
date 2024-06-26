module LegTypes
using SH, BaseTypes, SmallTypes, OptionTypes

export Leg, LegLike

abstract type LegLike end

# The quantity in leg is only to represent the qty in an option strategy and doesn't reflect transaction sizing. Transaction sizing (buy/sell multiple contracts at once) is handled separately.
struct Leg <: LegLike
    option::Option
    quantity::Float64
    side::Side.T
end
Leg(opt::Option, dir::DirSQ) = Leg(opt, dir.quantity, dir.side)
Leg(row::NamedTuple) = Leg(Option(row), row.quantity, Side.T(row.side))
Leg(; option=Option(), quantity=1.0, side=Side.long) = Leg(option, quantity, side)
Leg(leg::Leg; option=leg.option, quantity=leg.quantity, side=leg.side) = Leg(option, quantity, side)

SH.random(::Type{Leg}, style, side) = Leg(random(Option, style), rand(), side)

const LEGS_EMPTY = Leg[]
switchSide(leg::Leg) = Leg(leg; side=toOther(getSide(leg)))
SH.getOption(l::Leg) = l.option
SH.getQuantity(l::Leg) = l.quantity
SH.getSide(l::Leg) = l.side

SH.getStyle(l::Leg) = getStyle(l.option)
SH.getExpir(l::Leg) = getExpir(l.option)
SH.getStrike(l::Leg) = getStrike(l.option)

SH.getQuantityDir(l::Leg) = Int(getSide(l)) * l.quantity

# SH.addQuantity(leg::Leg, addend::Real) = Leg(leg.option, leg.quantity + addend, leg.side)

isConflict(l1, l2) = getOption(l1) == getOption(l2) && getSide(l1) != getSide(l2)
# isConflict(legs::Coll, leg) = !isnothing(findfirst(check -> isConflict(leg, check), legs))

isConflict(leg, opt::Option, side::Side.T) = side != getSide(leg) && opt == getOption(leg)
isConflict(leg, opt, side::Side.T) = side != getSide(leg) && getOption(opt) == getOption(leg)
isConflict(legs::CollT{<:LegLike}) = (opt, side::Side.T) -> !isnothing(findfirst(leg -> isConflict(leg, opt, side), legs))
# isConflict(legs::Coll, opt, side::Side.T) = !isnothing(findfirst(leg -> isConflict(leg, opt, side), legs))
isConflict(legs::CollT{<:LegLike}, side::Side.T) = opt -> !isnothing(findfirst(leg -> isConflict(leg, opt, side), legs))
function hasConflict(legs)
    for i in eachindex(legs)[1:end-1]
        for j in i+1:lastindex(legs)
            !isConflict(legs[i], legs[j]) || return true
        end
    end
    return false
end

end