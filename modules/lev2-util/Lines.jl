module Lines
using BaseTypes, LineTypes

export Segments, SegSide, Left, Right, Point

function scale(s::Segments, k)::Segments
    return Segments(k .* s.slopes, scale.(s.points, k))
end

scale(p::Point, k) = Point(p.x, k * p.y)

abstract type SegSide end

struct Left <: SegSide
    slope::Float64
    point::Point
end

struct Right <: SegSide
    point::Point
    slope::Float64
end

@enum Dir2::Int8 DirLeft=-1 DirRight=1

struct Seg3
    point::Point
    slope::Float64
    dir::Dir2
end
export Seg3, Dir2

slopeleft(s::Right, x::Float64)::Float64 = x <= s.point.x ? 0.0 : s.slope
sloperight(s::Right, x::Float64)::Float64 = x < s.point.x ? 0.0 : s.slope
slopeleft(s::Left, x::Float64)::Float64 = x <= s.point.x ? s.slope : 0.0
sloperight(s::Left, x::Float64)::Float64 = x < s.point.x ? s.slope : 0.0

@inline slopeleft2(s::Seg3, x::Float64)::Float64 = xor(x <= s.point.x, s.dir == DirLeft) ? 0.0 : s.slope
@inline sloperight2(s::Seg3, x::Float64)::Float64 = xor(x < s.point.x, s.dir == DirRight) ? s.slope : 0.0

@inline slopeleft(s::Seg3, x::Float64)::Float64 = xor(x > s.point.x, s.dir == DirLeft) * s.slope
@inline sloperight(s::Seg3, x::Float64)::Float64 = xor(x < s.point.x, s.dir == DirRight) * s.slope

Base.convert(::Type{Tuple{Float64,Float64}}, p::Point) = (p.x, p.y)

function toLineTuples(s::Segments; mn=100.0, mx=600.0)::NTuple{N+2,Tuple{Float64,Float64}}
    w = s.points[end].x - s.points[1].x
    x0 = max(0.0, min(mn, s.points[1].x - w))
    y0 = at(s, x0)
    xend = max(mx, s.points[end].x + w)
    yend = at(s, xend)
    return ((x0, y0), s.points..., (xend, yend))
end

function toLineTuples(ss::Vector{Segment})
    res = Vector{NTuple{2,Float64}}() # (undef, length(ss)+1)
    sizehint!(res, length(ss) + 1)
    for s in ss
        push!(res, (s.left.x, s.left.y))
    end
    s = ss[end]
    push!(res, (s.right.x, s.right.y))
    return res
end

# function toLineTuples(s::Segments{3})::NTuple{5,Tuple{Float64,Float64}}
#     w = s.points[3].x - s.points[1].x
#     x0 = s.points[1].x - w
#     y0 = at(s, x0)
#     x4 = s.points[3].x + w
#     y4 = at(s, x4)
#     return ((x0, y0), s.points..., (x4, y4))
# end

@inline getx(p) = p.x

function atsegs(s, x::Float64)::Float64
    # i = findfirst(p -> p.x >= x, s.points)
    i = searchsortedfirst(s.points, Point(x, 0.0); by=getx)
    # if isnothing(i)
    if i > length(s.points)
        pleft = last(s.points)
        return pleft.y + s.slopes[end] * (x - pleft.x)
    else
        pright = s.points[i]
        return pright.y - s.slopes[i] * (pright.x - x)
    end
end

at(s::Left, x::Float64)::Float64 = x >= s.point.x ? s.point.y : s.point.y - s.slope * (s.point.x - x)
at(s::Right, x::Float64)::Float64 = x <= s.point.x ? s.point.y : s.point.y + s.slope * (x - s.point.x)
@inline at(s::Seg3, x::Float64)::Float64 = xor(x >= s.point.x, s.dir == DirRight) ? s.point.y : s.point.y - s.slope * (s.point.x - x)
# @inline at(s::Seg3, x::Float64)::Float64 = s.point.y - xor(x < s.point.x, s.dir == DirRight) * s.slope * (s.point.x - x)
# @inline at(s::Seg3, x::Float64)::Float64 = s.point.y - (xor(x >= s.point.x, s.dir == DirRight) ? 0.0 : s.slope * (s.point.x - x))


using StructArrays
function map_multi(f, v)
    sa = StructArrays.collect_structarray(f(x) for x in v)
    return StructArrays.components(sa)
end

# combine(args...) = combine(args)
# function combine(segsides)::Segments
#     slopes1, points = map_multi(segsides) do segside
#         x::Float64 = segside.point.x
#         slope::Float64 = sum(slopeleft.(segsides, x))
#         y = sum(at.(segsides, x))
#         point = Point(x, y)
#         return (slope, point)
#     end
#     slope_last = sum(sloperight.(segsides, segsides[end].point.x))
#     slopes = (slopes1..., slope_last)
#     return Segments{length(segsides)}(slopes, Tuple(points))
# end

# combine(args...) = combine(args)
# TODO: why not ::Segments at the end?
# function combine(segsides::Coll{N,<:SegSide})
#     slopes1 = map(segsides) do segside
#         sum(slopeleft.(segsides, segside.point.x))
#     end
#     points = map(segsides) do segside
#         x = segside.point.x
#         y = sum(at.(segsides, x))
#         Point(x, y)
#     end
#     slope_last = sum(sloperight.(segsides, segsides[end].point.x))
#     slopes = (slopes1..., slope_last)
#     return Segments{length(segsides)}(slopes, Tuple(points))
# end

# function combine(segsides::Coll{N,<:SegSide})::Segments
#     error("not this one")
#     slopes1 = map(segsides) do segside
#         sum(slopeleft.(segsides, segside.point.x))
#     end
#     points = map(segsides) do segside
#         x = segside.point.x
#         y = sum(at.(segsides, x))
#         Point(x, y)
#     end
#     slope_last = sum(sloperight.(segsides, segsides[end].point.x))
#     slopes = (slopes1..., slope_last)
#     return Segments(slopes, Tuple(points))
# end

# function combine3(seg2s::Coll{N,Seg3})::Segments
#     slopes1 = map(seg2s) do segside
#         sum(slopeleft.(seg2s, segside.point.x))
#     end
#     points = map(seg2s) do segside
#         x = segside.point.x
#         y = sum(at.(seg2s, x))
#         Point(x, y)
#     end
#     slope_last = sum(sloperight.(seg2s, seg2s[end].point.x))
#     slopes = vcat(slopes1, slope_last)
#     return Segments(slopes, points)
# end

# using LoopVectorization
# function combine2(segs::Coll{N,Seg3})::Segments
#     slopes1 = map(segs) do seg
#         slopeat(segs, seg.point.x)
#     end
#     points = map(segs) do seg
#         s = 0.0
#         x = seg.point.x
#         @inbounds for i in eachindex(segs)
#             s += at(segs[i], x)
#         end
#         return Point(x, s)
#     end

#     slope_last = slopeat(segs, segs[end].point.x)
#     slopes = vcat(slopes1, slope_last)

#     return Segments(slopes, points)
# end

# @inline function slopeat(segs, x)
#     s = 0.0
#     @inbounds for i in eachindex(segs)
#         s += slopeleft(segs[i], x)
#     end
#     return s
# end

# using StaticArrays
# function combine_good(segs::Coll{N,Seg3})::Segments
#     slopes = MVector{N+1,Float64}(undef)
#     points = MVector{N,Point}(undef)
#     @inbounds for i in eachindex(segs)
#         seg1 = segs[i]
#         x = seg1.point.x

#         s = 0.0
#         y = 0.0
#         @inbounds for j in eachindex(segs)
#             seg2 = segs[j]
#             s += slopeleft(seg2, x)
#             y += at(seg2, x)
#         end

#         slopes[i] = s
#         points[i] = Point(x, y)
#     end

#     s = 0.0
#     x = segs[end].point.x
#     @inbounds for i in eachindex(segs)
#         s += sloperight(segs[i], x)
#     end

#     slopes[end] = s

#     return Segments(slopes, points)
# end

# function combine(segs::Coll{N,Seg3})
#     slopes = MVector{N+1,Float64}(undef)
#     points = MVector{N,Point}(undef)
#     prevx = -1.0
#     i = 1
#     for seg1 in segs
#         # seg1 = segs[i]
#         x = seg1.point.x
#         if x == prevx
#             continue
#         end

#         s = 0.0
#         y = 0.0
#         @inbounds for j in eachindex(segs)
#             seg2 = segs[j]
#             s += slopeleft(seg2, x)
#             y += at(seg2, x)
#         end

#         slopes[i] = s
#         points[i] = Point(x, y)

#         prevx = x
#         i += 1
#     end

#     s = 0.0
#     x = segs[end].point.x
#     @inbounds for i in eachindex(segs)
#         s += sloperight(segs[i], x)
#     end

#     slopes[i] = s

#     ps2 = points[1:(i-1)]
#     ss2 = slopes[1:i]
#     return Segments{i-1}(ss2, ps2)
#     # return Segments(slopes, points)
# end

# function combine(segs::CollT{Seg3})
#     N = length(segs)
#     slopes = MVector{N+1,Float64}(undef)
#     points = MVector{N,Point}(undef)
#     prevx = -1.0
#     i = 1
#     @inbounds for seg1 in segs
#         # seg1 = segs[i]
#         x = seg1.point.x
#         if x == prevx
#             continue
#         end

#         s = 0.0
#         y = 0.0
#         @inbounds for j in eachindex(segs)
#             seg2 = segs[j]
#             s += slopeleft(seg2, x)
#             y += at(seg2, x)
#         end

#         @inbounds slopes[i] = s
#         @inbounds points[i] = Point(x, y)

#         prevx = x
#         i += 1
#     end

#     s = 0.0
#     x = segs[end].point.x
#     @inbounds for i in eachindex(segs)
#         s += sloperight(segs[i], x)
#     end

#     slopes[i] = s

#     ps2 = points[1:(i-1)]
#     ss2 = slopes[1:i]
#     return Segments{i-1}(ss2, ps2)
#     # return Segments(slopes, points)
# end

function combine(segs::CollT{Seg3})
    N = length(segs)
    slopes = Vector{Float64}(undef, N+1)
    points = Vector{Point}(undef, N)
    prevx = -1.0
    i = 1
    @inbounds for seg1 in segs
        # seg1 = segs[i]
        x = seg1.point.x
        if x == prevx
            continue
        end

        s = 0.0
        y = 0.0
        @inbounds for j in eachindex(segs)
            seg2 = segs[j]
            s += slopeleft(seg2, x)
            y += at(seg2, x)
        end

        @inbounds slopes[i] = s
        @inbounds points[i] = Point(x, y)

        prevx = x
        i += 1
    end

    s = 0.0
    x = segs[end].point.x
    @inbounds for i in eachindex(segs)
        s += sloperight(segs[i], x)
    end

    slopes[i] = s

    ps2 = points[1:(i-1)]
    ss2 = slopes[1:i]
    return Segments(ss2, ps2)
    # return Segments(slopes, points)
end

# using StaticArrays
# function combine(segs::Segments, segsides::Coll{A,<:SegSide})::Segments{N+A} where {N,A}
#     slopes = MVector{N+A+1,Float64}(undef)
#     points = MVector{N+A,Point}(undef)
#     return Segments{N+A}(slopes, points)
# end

#region SegmentsWithZeros
findZeros(s::Segments) = SegmentsZeros(s)
# Base.IteratorSize(::Type{SegmentsZeros}) = Base.SizeUnknown()
Base.IteratorSize(::Type{SegmentsZeros}) = Base.SizeUnknown()
# Base.eltype(::Type{SegmentsZeros}) = Float64
Base.eltype(::Type{SegmentsZeros}) = Float64

function Base.iterate(z::SegmentsZeros)::Union{Nothing,Tuple{Float64,Int}}
    s = z.s
    if signbit(s.slopes[1]) == signbit(s.points[1].y)
        # s.points[1].y - s.slopes[1] * (s.points[1].x - x) = 0
        x = (s.slopes[1] * s.points[1].x - s.points[1].y) / s.slopes[1]
        return (x, 1)
    else
        return iterate(z, 1)
    end
end

function Base.iterate(z::SegmentsZeros, i::Int)::Union{Nothing,Tuple{Float64,Int}}
    error("check this code")
    s = z.s
    @assert i > 0
    lasti = lastindex(s.points)
    while i <= lasti
        if i == lasti
            # TODO: is this broken beacuse if slope == 0 so will make NaN/Inf?
            if signbit(s.slopes[end]) != signbit(s.points[end].y)
                # s.points[end].y + s.slopes[end] * (x - s.points[end].x) = 0
                x = (s.points[end].y - s.slopes[end] * s.points[end].x) / s.slopes[end]
                return (x, i+1)
            end
        else
            if signbit(s.points[i].y) != signbit(s.points[i+1].y)
                # s.points[i].y + s.slopes[i+1] * (x - s.points[i].x) = 0
                x = (s.slopes[i+1] * s.points[i].x - s.points[i].y) / s.slopes[i+1]
                return (x, i+1)
            end
        end

        i += 1
    end
    return nothing
end

segmentsWithZeros(s::Segments; extent) = SegmentsWithZeros(s, extent)
# Base.IteratorSize(::Type{SegmentsWithZeros}) = Base.SizeUnknown()
Base.IteratorSize(::Type{SegmentsWithZeros}) = Base.SizeUnknown()
# Base.eltype(::Type{SegmentsWithZeros}) = Segment
Base.eltype(::Type{SegmentsWithZeros}) = Segment
const SegWZIterType3 = Tuple{Segment,Tuple{Int,Union{Nothing,Point}}}

function Base.iterate(z::SegmentsWithZeros)::Union{Nothing,SegWZIterType3}
    s = z.s
    slope0 = s.slopes[1]
    pt1 = s.points[1]

    if s.slopes[1] * s.points[1].y > 1e-8 # signbit(s.slopes[1]) == signbit(s.points[1].y)
        segx2 = (slope0 * pt1.x - pt1.y) / slope0
        segpt2 = Point(segx2, 0.0)
        segx1 = min(z.extent[1], 0.9*segx2)

        segy1 = 0.0 - slope0 * (segx2 - segx1)
        segpt1 = Point(segx1, segy1)

        # println("1b1: ", (;segx1, segy1, segx2, pt1.y, slope0, segpt2.x))
        return (Segment(segpt1, segpt2, slope0), (0, segpt2))
    else
        segx1 = min(z.extent[1], 0.9*pt1.x)
        segy1 = pt1.y - slope0 * (pt1.x - segx1)
        segpt1 = Point(segx1, segy1)

        # println("1b2: ", (;segpt1, pt1, slope0))
        return (Segment(segpt1, pt1, slope0), (1, nothing))
    end
end

# if s.slopes[1] * s.points[1].y > 0 # signbit(s.slopes[1]) == signbit(s.points[1].y)
#     # s.points[1].y - s.slopes[1] * (s.points[1].x - x) = 0
#     segx2 = (slope0 * pt1.x - pt1.y) / slope0
#     segpt2 = Point(segx2, 0.0)

#     segy1 = pt1.y - slope0 * segpt2.x
#     segpt1 = Point(segx1, segy1)
#     @show pt1.y slope0 segpt2.x pt1.x
#     # @show segx1 segy1 segx2 0.0
#     return (Segment(segpt1, segpt2, slope0), (0, segpt2))
# else
#     segy1 = pt1.y - slope0 * width
#     segpt1 = Point(segx1, segy1)

#     return (Segment(segpt1, pt1, slope0), (1, nothing))
# end

function Base.iterate(z::SegmentsWithZeros, (i, pt1)::Tuple{Int,Point})::Union{Nothing,SegWZIterType3}
    s = z.s
    if i < lastindex(s.points)
        pt2 = s.points[i+1]
        # println("2b1: ", (;pt1, pt2, ss=s.slopes[i+1]))
        return (Segment(pt1, pt2, s.slopes[i+1]), (i+1, nothing))
    else
        # Final segment
        slopeEnd = s.slopes[end]
        x2 = max(z.extent[2], 1.1*pt1.x)
        y2 = s.points[end].y + slopeEnd * (x2 - pt1.x)
        pt2 = Point(x2, y2)
        # println("2b2: ", (;pt1, x2, y2, slopeEnd))
        return (Segment(pt1, pt2, slopeEnd), (i+1, nothing))
    end
end

function Base.iterate(z::SegmentsWithZeros, (i, _)::Tuple{Int, Nothing})::Union{Nothing,SegWZIterType3}
    s = z.s
    # @assert i > 0
    lasti = lastindex(s.points)
    i <= lasti || return nothing # All done iterating through them

    if i == lasti
        ptlast = s.points[i]
        slopeEnd = s.slopes[end]
        if slopeEnd * ptlast.y < 0 # slopeend != 0 && signbit(slopeend) != signbit(ptlast.y)
            # s.points[end].y + s.slopes[end] * (x - s.points[end].x) = 0
            segx2 = ptlast.x - ptlast.y / slopeEnd
            segpt2 = Point(segx2, 0.0)
            # println("3b1b1: ", (;ptlast, segpt2, slopeEnd))
            return (Segment(ptlast, segpt2, slopeEnd), (i, segpt2))
        else
            # Final segment
            x2 = max(z.extent[2], 1.1*ptlast.x)
            y2 = ptlast.y + slopeEnd * (x2 - ptlast.x)
            segpt2 = Point(x2, y2)
            # println("3b1b2: ", (;ptlast, segpt2, slopeEnd))
            return (Segment(ptlast, segpt2, slopeEnd), (i+1, nothing))
        end
    else
        pt1 = s.points[i]
        pt2 = s.points[i+1]
        slope = s.slopes[i+1]
        if pt1.y * pt2.y < -1e-8 # signbit(pt1.y) != signbit(pt2.y)
            # s.points[i].y + s.slopes[i+1] * (x - s.points[i].x) = 0
            segx2 = (slope * pt1.x - pt1.y) / slope
            segpt2 = Point(segx2, 0.0)
            # println("3b2b1: ", (;pt1, pt2, segpt2, slope))
            return (Segment(pt1, segpt2, slope), (i, segpt2))
        else
            # println("3b2b2: ", (;pt1, pt2, slope))
            return (Segment(pt1, pt2, slope), (i+1, nothing))
        end
    end
end
#endregion SegmentsWithZeros

#region Test
function test()
    p1 = Point(10.0, 1.0)
    l1 = Left(1.0, p1)
    r1 = Right(p1, -1.0)
    @assert at(l1, 9.0) == 0.0
    @assert at(r1, 11.0) == 0.0

    c1 = combine(Right(Point(1.0, -1.0), 1.0), Left(-1.0, Point(2.0, -1.0)))
    c2 = combine(Left(-1.0, Point(400.0, -1.52)), Left(1.0, Point(401.0, 1.76)))
    @assert c2 == Segments{2}((0.0, 1.0, 0.0), (Point(400.0, -0.76), Point(401.0, 0.24)))
    c3 = combine(Left(-1.0, Point(400.0, -0.22)), Left(-1.0, Point(401.0, -0.31)), Left(1.0, Point(402.0, 0.44)))
    @assert c3 == Segments{3}((-1.0, 0.0, 1.0, 0.0), (Point(400.0, -1.09), Point(401.0, -1.09), Point(402.0, -0.09000000000000002)))

    tups = toLineTuples(c3)
    # @assert tups ≈ ((398.0, -1.09 + 2.0), (400.0, -1.09), (401.0, -1.09), (402.0, -0.09000000000000002), (404.0, -0.09000000000000002)) "$(tups)"
    @assert tups ≈ ((100.0, -1.09 + 300.0), (400.0, -1.09), (401.0, -1.09), (402.0, -0.09000000000000002), (600.0, -0.09000000000000002)) "$(tups)"
end
function Base.:≈(tup1::Tuple, tup2::Tuple)::Bool
    for i in eachindex(tup1)
        tup1[i] ≈ tup2[i] || return false
    end
    return true
end
#endregion Test

#region CombineOld
# point(s::SegSide, x::Float64) = Point(s.point.x, at(s, x))
# point(pt::Point, l1, l2, l3) = Point(pt.x, at(l1, pt.x) + at(l2, pt.x) + at(l3, pt.x))
# points(l1, l2, l3) = (
#     point(l1.point, l1, l2, l3),
#     point(l2.point, l1, l2, l3),
#     point(l3.point, l1, l2, l3)
# )

# combine(ls::Tuple{Left}) = combine(ls...)
# function combine(l::Left)::Segments{1}
#     return Segments{1}((l.slope, 0.0), (Point(l.point.x, l.point.y),))
# end

# combine(ls::Tuple{Right}) = combine(ls...)
# function combine(r::Right)::Segments{1}
#     return Segments{1}((0.0, r.slope), (Point(r.point.x, r.point.y),))
# end

# function combine(l::Left, r::Right)::Segments{2}
#     @assert l.point.x < r.point.x
#     y = l.point.y + r.point.y
#     return Segments{2}((l.slope, 0.0, r.slope), (Point(l.point.x, y), Point(r.point.x, y)))
# end

# function combine(l1::Right, l2::Left)::Segments{2}
#     @assert l1.point.x < l2.point.x
#     yl = l1.point.y + l2.point.y - l2.slope * (l2.point.x - l1.point.x)
#     yr = l2.point.y + l1.point.y + l1.slope * (l2.point.x - l1.point.x)
#     segs = Segments{2}(
#         (l2.slope, l1.slope + l2.slope, l1.slope),
#         (Point(l1.point.x, yl), Point(l2.point.x, yr))
#     )
#     @assert segs.points[2].y ≈ segs.points[1].y + segs.slopes[2] * (segs.points[2].x - segs.points[1].x)
#     return segs
# end

# combine(ls::Tuple{Left,Left}) = combine(ls...)
# function combine(l1::Left, l2::Left)::Segments{2}
#     @assert l1.point.x < l2.point.x
#     y2 = l1.point.y + l2.point.y
#     y1 = y2 - l2.slope * (l2.point.x - l1.point.x)
#     segs = Segments{2}(
#         (l1.slope + l2.slope, l2.slope, 0.0),
#         (Point(l1.point.x, y1), Point(l2.point.x, y2))
#     )
#     @assert segs.points[2].y ≈ segs.points[1].y + segs.slopes[2] * (segs.points[2].x - segs.points[1].x)
#     return segs
# end

# combine(ls::Tuple{Right,Right}) = combine(ls...)
# function combine(l1::Right, l2::Right)::Segments{2}
#     @assert l1.point.x < l2.point.x
#     y1 = l1.point.y + l2.point.y
#     y2 = y1 + l1.slope * (l2.point.x - l1.point.x)
#     segs = Segments{2}(
#         (0.0, l1.slope, l1.slope + l2.slope),
#         (Point(l1.point.x, y1), Point(l2.point.x, y2))
#     )
#     @assert segs.points[2].y ≈ segs.points[1].y + segs.slopes[2] * (segs.points[2].x - segs.points[1].x)
#     return segs
# end

# combine(tup) = combine(tup...)
# function combine(l1::SegSide, l2::SegSide, l3::SegSide)::Segments{3}
#     return Segments{3}(slopes(l1, l2, l3), points(l1, l2, l3))
# end

# combine(ls::Tuple{Left,Right,Right,Right}) = combine(ls...)
# function combine(l1::Left, l2::Right, l3::Right, l4::Right)::Segments{4}
#     @assert l1.point.x < l2.point.x < l3.point.x < l4.point.x
#     slope23 = l2.slope + l3.slope
#     y1 = l1.point.y + l2.point.y + l3.point.y + l4.point.y
#     y2 = y1
#     y3 = y2 + l2.slope * (l3.point.x - l2.point.x)
#     y4 = y3 + (slope23) * (l4.point.x - l3.point.x)
#     segs = Segments{4}(
#         (l1.slope, 0.0, l2.slope, slope23, slope23 + l4.slope),
#         (Point(l1.point.x, y1), Point(l2.point.x, y2), Point(l3.point.x, y3), Point(l4.point.x, y4))
#     )
#     return segs
# end

# combine(ls::Tuple{Left,Left,Right,Right}) = combine(ls...)
# function combine(l1::Left, l2::Left, l3::Right, l4::Right)::Segments{4}
#     @assert l1.point.x <= l2.point.x <= l3.point.x <= l4.point.x
#     y2 = l1.point.y + l2.point.y + l3.point.y + l4.point.y
#     y3 = y2
#     y1 = y2 - l2.slope * (l2.point.x - l1.point.x)
#     y4 = y3 + l3.slope * (l4.point.x - l3.point.x)
#     segs = Segments{4}(
#         (l1.slope + l2.slope, l2.slope, 0.0, l3.slope, l3.slope + l4.slope),
#         (Point(l1.point.x, y1), Point(l2.point.x, y2), Point(l3.point.x, y3), Point(l4.point.x, y4))
#     )
#     return segs
# end

# combine(ls::Tuple{Left,Left,Left,Left}) = combine(ls...)
# function combine(l1::Left, l2::Left, l3::Left, l4::Left)::Segments{4}
#     @assert l1.point.x <= l2.point.x <= l3.point.x <= l4.point.x
#     slope34 = l3.slope + l4.slope
#     slope234 = l2.slope + slope34
#     y4 = l1.point.y + l2.point.y + l3.point.y + l4.point.y
#     y3 = y4 - l4.slope * (l4.point.x - l3.point.x)
#     y2 = y3 - (slope34) * (l3.point.x - l2.point.x)
#     y1 = y2 - (slope234) * (l2.point.x - l1.point.x)
#     segs = Segments{4}(
#         (slope234 + l1.slope, slope234, slope34, l4.slope, 0.0),
#         (Point(l1.point.x, y1), Point(l2.point.x, y2), Point(l3.point.x, y3), Point(l4.point.x, y4))
#     )
#     return segs
# end

# combine(ls::Tuple{Right,Right,Left,Left}) = combine(ls...)
# function combine(l1::Right, l2::Right, l3::Left, l4::Left)::Segments{4}
#     @assert l1.point.x <= l2.point.x <= l3.point.x <= l4.point.x @str l1 l2 l3 l4
#     y12 = l1.point.y + l2.point.y
#     y34 = l3.point.y + l4.point.y
#     y2 = y12 + y34
#     y1 = y2 - l2.slope * (l2.point.x - l1.point.x)
#     y3 = y2
#     y4 = y3 + l3.slope * (l4.point.x - l3.point.x)
#     segs = Segments{4}(
#         (0.0, l2.slope, 0.0, l3.slope, 0.0),
#         (Point(l1.point.x, y1), Point(l2.point.x, y2), Point(l3.point.x, y3), Point(l4.point.x, y4))
#     )
#     return segs
# end

# combine(ls::Tuple{Left,Right,Left,Right}) = combine(ls...)
# function combine(l1::Left, l2::Right, l3::Left, l4::Right)::Segments{4}
#     error("TODO")
# end

# slopeleft(x::Float64, l1, l2, l3)::Float64 = slopeleft(l1, x) + slopeleft(l2, x) + slopeleft(l3, x)
# sloperight(x::Float64, l1, l2, l3)::Float64 = sloperight(l1, x) + sloperight(l2, x) + sloperight(l3, x)

# slopes(l1, l2, l3) = (
#     slopeleft(l1.point.x, l1, l2, l3),
#     sloperight(l1.point.x, l1, l2, l3),
#     sloperight(l2.point.x, l1, l2, l3),
#     sloperight(l3.point.x, l1, l2, l3)
# )

# # TODO: test above

# # combine(ls::Tuple{Left,Left,Left}) = combine(ls...)
# # function combine(l1::Left, l2::Left, l3::Left)::Segments{3}
# #     @assert l1.point.x < l2.point.x < l3.point.x
# #     y3 = l1.point.y + l2.point.y + l3.point.y
# #     y2 = y3 - l3.slope * (l3.point.x - l2.point.x)
# #     y1 = y2 - (l2.slope + l3.slope) * (l2.point.x - l1.point.x)
# #     slope23 = l2.slope + l3.slope
# #     segs = Segments{3}(
# #         (l1.slope + slope23, slope23, l3.slope, 0.0),
# #         (Point(l1.point.x, y1), Point(l2.point.x, y2), Point(l3.point.x, y3))
# #     )
# #     return segs
# # end

# # combine(ls::Tuple{Right,Right,Right}) = combine(ls...)
# # function combine(l1::Right, l2::Right, l3::Right)::Segments{3}
# #     @assert l1.point.x <= l2.point.x <= l3.point.x string("$(l1.point.x) < $(l2.point.x) < $(l3.point.x)")
# #     y1 = l1.point.y + l2.point.y + l3.point.y
# #     y2 = y1 + l1.slope * (l2.point.x - l1.point.x)
# #     y3 = y2 + (l1.slope + l2.slope) * (l3.point.x - l2.point.x)
# #     slope12 = l1.slope + l2.slope
# #     segs = Segments{3}(
# #         (0.0, l1.slope, slope12, slope12 + l3.slope),
# #         (Point(l1.point.x, y1), Point(l2.point.x, y2), Point(l3.point.x, y3))
# #     )
# #     return segs
# # end

# # combine(ls::Tuple{Left,Right,Right}) = combine(ls...)
# # function combine(l1::Left, l2::Right, l3::Right)::Segments{3}
# #     @assert l1.point.x <= l2.point.x <= l3.point.x string("$(l1.point.x) < $(l2.point.x) < $(l3.point.x)")
# #     y1 = l1.point.y + l2.point.y + l3.point.y
# #     y2 = y1
# #     y3 = y2 + l2.slope * (l3.point.x - l2.point.x)
# #     slope23 = l2.slope + l3.slope
# #     segs = Segments{3}(
# #         (l1.slope, 0.0, l2.slope, slope23),
# #         (Point(l1.point.x, y1), Point(l2.point.x, y2), Point(l3.point.x, y3))
# #     )
# #     return segs
# # end


# # #region New
# # combine(ls::Tuple{Right,Left,Right}) = combine(ls...)
# # function combine(l1::Right, l2::Left, l3::Right)::Segments{3}
# #     @assert l1.point.x <= l2.point.x <= l3.point.x string("$(l1.point.x) < $(l2.point.x) < $(l3.point.x)")
# #     y1, y2, y3 = atall(l1, l2, l3)
# #     segs = Segments{3}(
# #         (l2.slope, l1.slope + l2.slope, l1.slope, l1.slope + l3.slope),
# #         (Point(l1.point.x, y1), Point(l2.point.x, y2), Point(l3.point.x, y3))
# #     )
# #     return segs
# # end

# # combine(ls::Tuple{Right,Right,Left}) = combine(ls...)
# # function combine(l1::Right, l2::Right, l3::Left)::Segments{3}
# #     @assert l1.point.x <= l2.point.x <= l3.point.x string("$(l1.point.x) < $(l2.point.x) < $(l3.point.x)")
# #     y1, y2, y3 = atall(l1, l2, l3)
# #     segs = Segments{3}(
# #         (l2.slope, l1.slope + l2.slope, l1.slope, l1.slope + l3.slope),
# #         (Point(l1.point.x, y1), Point(l2.point.x, y2), Point(l3.point.x, y3))
# #     )
# #     return segs
# # end
# # #endregion

# # function combine(l::Right{T}, r::Left{T})::Segments{3} where T<:SlopeDir
# #     @assert l.point.x < r.point.x
# #     @assert sign(l.slope) != sign(r.slope)
# #     # ly + lm*(x - lx) = ry - rm(rx - x)
# #     # lm*x - rm*x = ry - rm*rx - ly + lm*lx
# #     # x = (ry - rm*rx - ly + lm*lx) / (lm - rm)
# #     yl = l.point.y + r.point.y - r.slope * (r.point.x - l.point.x)
# #     yr = r.point.y + l.point.y + l.slope * (r.point.x - l.point.x)
# #     xnew = (yr - r.slope * r.point.x - yl + l.slope * l.point.x) / (l.slope - r.slope)
# #     ynew = yl + l.slope * (xnew - l.point.x)
# #     @assert ynew ≈ (yr - r.slope * (r.point.x - xnew))
# #     return Segments{3}(
# #         (r.slope, l.slope + r.slope, l.slope + r.slope, l.slope),
# #         (Point(l.point.x, yl), Point(xnew, ynew), Point(r.point.x, yr))
# #     )
# # end
#endregion CombineOld

end