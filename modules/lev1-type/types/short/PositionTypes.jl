module PositionTypes
using SH, BaseTypes, SmallTypes, OptionTypes, LegTypes

export Position

# struct Position
#     option::Option
#     side::Side.T
#     quantityDir::Float64
#     tsAcquired::Int
#     basis::Currency
# end
struct Position
    leg::Leg
    tsAcquired::Int
    basis::Currency
end
SH.getLeg(p::Position) = p.leg
SH.getOption(p::Position) = getOption(p.leg)
SH.getSide(p::Position) = getSide(p.leg)
SH.getQuantity(p::Position) = getQuantity(p.leg)
SH.getStrike(p::Position) = getStrike(p.leg)

end