module LinesLeg
using SH, BaseTypes, SmallTypes
using LineTypes
import Lines:Lines, Segments, SegSide, Left, Right, at, atsegs, combine, toLineTuples, findZeros, segmentsWithZeros
import LegTypes:LegLike

export Segments, Section, toLineTuples

# TODO: get rid of Section?
# I think I just did it as a simplification/estimation, but I think it's too inaccurate
struct Section
    x1::Float64
    x2::Float64
    y::Float64
end

# toPoint(leg::LegLike, neto::Float64)::Point = Point(getStrike(leg), neto)
# slopeLeft(style::Style.T, side::Side.T, qty::Float64)::Float64 =
#         style == Style.call ? 0.0 : (side == Side.long ? -qty : qty)
# slopeRight(style::Style.T, side::Side.T, qty::Float64)::Float64 =
#         style == Style.put ? 0.0 : (side == Side.long ? qty : -qty)
# toLines(leg::LegLike, neto::Float64)::Segments{1} = Segments(
#         slopeLeft(getStyle(leg), getSide(leg), Float64(getQuantity(leg))),
#         toPoint(leg, neto),
#         slopeRight(getStyle(leg), getSide(leg), Float64(getQuantity(leg)))
#     )
segCall(strike::Currency, side::Side.T, qty::Float64, neto::PT)::Right = Right(Point(strike, F(neto)), side == Side.long ? qty : -qty)
segPut(strike::Currency, side::Side.T, qty::Float64, neto::PT)::Left = Left(side == Side.long ? -qty : qty, Point(strike, F(neto)))
# TODO: clean up after moving style/side into types
function toSeg(leg::LegLike, neto::PT)::SegSide
    # println("toSeg ", (;leg, neto))
    strike = getStrike(leg)
    side = getSide(leg)
    qty = getQuantity(leg)
    return getStyle(leg) == Style.call ? segCall(strike, side, qty, neto) : segPut(strike, side, qty, neto)
end

import Lines:Seg3,DirRight,DirLeft
@inline seg3Call(strike::Currency, side::Side.T, qty::Float64, neto::PT)::Seg3 = Seg3(Point(strike, F(neto)), side == Side.long ? qty : -qty, DirRight)
@inline seg3Put(strike::Currency, side::Side.T, qty::Float64, neto::PT)::Seg3 = Seg3(Point(strike, F(neto)), side == Side.long ? -qty : qty, DirLeft)
@inline function toSeg3(leg::LegLike, neto::PT)::Seg3
    # println("toSeg ", (;leg, neto))
    strike = getStrike(leg)
    side = getSide(leg)
    qty = getQuantity(leg)
    return getStyle(leg) == Style.call ? seg3Call(strike, side, qty, neto) : seg3Put(strike, side, qty, neto)
end

# toSegments(legs::CollT{<:LegLike}, netos::CollT{Currency}) = toSegments(tuple(legs...), tuple(netos...))
# function toSegments(legs::Tuple{<:LegLike}, netos::Tuple{PT})
function toSegments(legs::CollT{<:LegLike}, netos::CollT{PT})
    # @assert issorted(legs; by=getStrike)
    segs = toSeg3.(legs, netos)
    return combine(segs)
end

# function toSegments(legs::Coll{N,T}, netos::Coll{N,PT})::Segments where {N,T<:LegLike}
#     @assert issorted(legs; by=getStrike)
#     segs = toSeg.(legs, netos)
#     return combine(segs)
# end

function testsegs(segs, netos)
    s = 0.0
    for _ in 1:1000000
        x = toSegments(segs, netos)
        s += first(x.slopes)
    end
    return s
end

# function toSegments(legs::Coll{N,T}, netos::Coll{N,PT}) where {N,T<:LegLike}
#     # @assert issorted(legs; by=getStrike)
#     segs = toSeg3.(legs, netos)
#     return Lines.combine(segs)
# end

(slopeprofit(s::Segments)) = s.slopes[1] < 0 || s.slopes[end] > 0
canprofit(s::Segments) = slopeprofit(s) || !isnothing(findfirst(p -> p.y > 0, s.points))
function maxprofit(s::Segments, curp)
    # TODO: Use particular cdf points (eg. 20%, 50%, 80%) on the prob distribution?
    p = Float64(curp)
    return max(atsegs(s, 0.9 * p), atsegs(s, p), atsegs(s, 1.1 * p))
end

# canprofit(s::Segments{1}) = slopeprofit(s) || s.points[1].y > 0
# canprofit(s::Segments{2}) = slopeprofit(s) || s.points[1].y > 0 || s.points[2].y > 0
# canprofit(s::Segments{3}) = slopeprofit(s) || s.points[1].y > 0 || s.points[2].y > 0 || s.points[3].y > 0
# canprofit(s::Segments{4}) = slopeprofit(s) || s.points[1].y > 0 || s.points[2].y > 0 || s.points[3].y > 0 || s.points[4].y > 0
# function extrema(s::Segments)
# end

toSegmentsWithZeros(legs::NTuple{N,LegLike}, netos::NTuple{N,PT}) where N = toSegmentsWithZeros(toSegments(legs, netos))
toSegmentsWithZeros(segs::Segments; extent=(100.0, 600.0)) = segmentsWithZeros(segs; extent)

toSections(legs::NTuple{N,LegLike}, netos::NTuple{N,PT}) where N = toSections(toSegments(legs, netos))
function toSections(segs::Segments)
    zs = findZeros(segs)
    x1 = first(segs.points).x
    x1 -= abs(x1) + 1 # subtract an extra 1 in case x1 == 0
    xend = last(segs.points).x
    xend += xend + 1 # add an extra 1 in case x1 == 0
    zprev = x1
    return Iterators.map(Iterators.flatten((zs, xend))) do z
        zp = zprev
        zprev = z
        y = at(segs, (zp + z)/2)
        return Section(zp, z, y)
    end
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