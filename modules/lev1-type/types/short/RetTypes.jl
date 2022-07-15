module RetTypes
using SH, Bins, BaseTypes

export Ret, retMid, valAt, valAtPrice

struct Ret
    vals::Vector{Float64}
    center::Float64
    numLegs::Int
end
# const RET_ZERO = Ret(Bins.with(0.0), 1.0, 0)
# Ret() = Ret(Bins.with(1.0), 1.0, 4)
Ret(center::Real) = Ret(Bins.with(0.0), center, 0)
SH.getCenter(r::Ret) = r.center
SH.getVals(r::Ret) = r.vals
# SH.valAt(r::Ret, i::Int) = r.vals[i]
Base.getindex(r::Ret, i::Int) = r.vals[i]
Base.lastindex(r::Ret) = lastindex(r.vals)
# SH.getValMid(r::Ret) = r.vals[binsMid()]
valFirst(r::Ret)::Float64 = r.vals[1]
valLast(r::Ret)::Float64 = r.vals[end]

valAtPrice(ret::Ret, x::Real) = valAt(ret, x/getCenter(ret))
# TODO: unit tests for this
function SH.valAt(ret::Ret, x::Real)
    Bins.isLeft(x) && return valFirst(ret)
    Bins.isRight(x) && return valLast(ret)

    bl = Bins.leftOf(x)
    br = Bins.rightOf(x)
    if bl == br
        return ret[bl]
    end
    # TODO: special calc for between 1-2 and end-1 end
    left = ret[bl]
    right = ret[br]
    ratio = (x - Bins.x(bl))/(Bins.x(br) - Bins.x(bl))
    # @info "" left right ratio bl br
    return left + ratio * (right - left)
end

end