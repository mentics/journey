module LinesLeg
using SH, AbstractTypes, BaseTypes, SmallTypes, Lines

export Segments

# toPoint(leg::LegType, neto::Float64)::Point = Point(getStrike(leg), neto)
# slopeLeft(style::Style.T, side::Side.T, qty::Float64)::Float64 =
#         style == Style.call ? 0.0 : (side == Side.long ? -qty : qty)
# slopeRight(style::Style.T, side::Side.T, qty::Float64)::Float64 =
#         style == Style.put ? 0.0 : (side == Side.long ? qty : -qty)
# toLines(leg::LegType, neto::Float64)::Segments{1} = Segments(
#         slopeLeft(getStyle(leg), getSide(leg), Float64(getQuantity(leg))),
#         toPoint(leg, neto),
#         slopeRight(getStyle(leg), getSide(leg), Float64(getQuantity(leg)))
#     )
segCall(strike::Currency, side::Side.T, qty::Float64, neto::PT)::Right = Right(Point(strike, F(neto)), side == Side.long ? qty : -qty)
segPut(strike::Currency, side::Side.T, qty::Float64, neto::PT)::Left = Left(side == Side.long ? -qty : qty, Point(strike, F(neto)))
# TODO: clean up after moving style/side into types
function toSeg(leg::LegType, neto::PT)::SegSide
    strike = getStrike(leg)
    side = getSide(leg)
    qty = getQuantity(leg)
    return getStyle(leg) == Style.call ? setCall(strike, side, qty, neto) : segPut(strike, side, qty, neto)
end
function toSegments(legs::NTuple{N,LegType}, netos::NTuple{N,PT})::Segments{N} where N
    # @assert issorted(legs; by=getStrike)
    segs = toSeg.(legs, netos)
    return Lines.combine(segs)
end

using OptionTypes, LegTypes
import Dates
function test()
    xpir = Dates.Date(0)
    legs = (Leg(Option(Style.put, xpir, C(400.0)), 1.0, Side.long),
        Leg(Option(Style.put, xpir, C(401.0)), 1.0, Side.long),
        Leg(Option(Style.put, xpir, C(402.0)), 1.0, Side.short))
    segments = toSegments(legs, P.((-0.22, -0.31, 0.44)))
    @assert segments == Segments{3}((-1.0, 0.0, 1.0, 0.0), (Point(400.0, -1.09), Point(401.0, -1.09), Point(402.0, -0.09000000000000002))) "$(segments)"
end

end