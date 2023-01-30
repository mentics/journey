module LineTypes
using ComputedFieldTypes

export Point, Segments

struct Point
    x::Float64
    y::Float64
end

@computed struct Segments{N}
    slopes::NTuple{N+1,Float64}
    points::NTuple{N,Point}
end

end