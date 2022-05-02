module RetTypes
using SH, Bins

export Ret, retMid, valAt, valAtPrice

struct Ret
    center::Float64
    vals::Vector{Float64}
end
Ret() = Ret(1.0, Bins.with(1.0))
SH.getCenter(r::Ret) = r.center
SH.getVals(r::Ret) = r.vals
# SH.valAt(r::Ret, i::Int) = r.vals[i]
Base.getindex(r::Ret, i::Int) = r.vals[i]
# SH.getValMid(r::Ret) = r.vals[binsMid()]
valFirst(r::Ret)::Float64 = r.vals[1]
valLast(r::Ret)::Float64 = r.vals[end]

valAtPrice(ret::Ret, x::Float64) = valAt(ret, x/getCenter(ret))
function SH.valAt(ret::Ret, x::Float64)
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
    ratio = (x - binX(bl))/(br - bl)
    return left + ratio * (right - left)
end

end