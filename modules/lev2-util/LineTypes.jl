module LineTypes
using ComputedFieldTypes

export Point, Segment, Segments, SegmentsZeros, SegmentsWithZeros

struct Point
    x::Float64
    y::Float64
end

# TODO: If we need slope, add it here
struct Segment
    left::Point
    right::Point
    slope::Float64
end

@computed struct Segments{N}
    slopes::NTuple{N+1,Float64}
    points::NTuple{N,Point}
end

@computed struct SegmentsZeros{N}
    s::fulltype(Segments{N})
end

# Segments plus extra ones around zeros
@computed struct SegmentsWithZeros{N}
    s::fulltype(Segments{N})
end

end