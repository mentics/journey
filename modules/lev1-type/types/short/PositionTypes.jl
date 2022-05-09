module PositionTypes
using Dates
# using SH, BaseTypes, SmallTypes, OptionTypes, LegTypes
using SH, BaseTypes, LegTypes

export Position, tsAcquired

# struct Position
#     option::Option
#     side::Side.T
#     quantityDir::Float64
#     tsAcquired::Int
#     basis::Currency
# end
struct Position
    leg::Leg
    basis::Currency
    tsAcquired::DateTime
end
SH.getLeg(p::Position) = p.leg
SH.getOption(p::Position) = getOption(p.leg)
SH.getSide(p::Position) = getSide(p.leg)
SH.getQuantity(p::Position) = getQuantity(p.leg)
SH.getStrike(p::Position) = getStrike(p.leg)
tsAcquired(p::Position) = p.tsAcquired

end