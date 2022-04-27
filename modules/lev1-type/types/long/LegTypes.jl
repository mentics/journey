module LegTypes
using SH, SmallTypes, OptionTypes

export Leg, doLegsConflict, switchSide

struct Leg
    option::Option
    quantity::Float64
    side::Side.T
end
Leg(; option=Option(), quantity=1.0, side=Side.long) = Leg(option, quantity, side)
Leg(leg::Leg; option=leg.option, quantity=leg.quantity, side=leg.side) = Leg(option, quantity, side)
switchSide(leg::Leg) = Leg(leg; side=toOther(getSide(leg)))
SH.getOption(l::Leg) = l.option
SH.getQuantity(l::Leg) = l.quantity
SH.getSide(l::Leg) = l.side

SH.getStyle(l::Leg) = getStyle(l.option)
SH.getExpiration(l::Leg) = getExpiration(l.option)
SH.getStrike(l::Leg) = getStrike(l.option)

SH.getQuantityDir(l::Leg) = Int(getSide(l)) * l.quantity

# SH.addQuantity(leg::Leg, addend::Real) = Leg(leg.option, leg.quantity + addend, leg.side)

doLegsConflict(l1, l2) = getOption(l1) == getOption(l2) && getSide(l1) != getSide(l2)

end