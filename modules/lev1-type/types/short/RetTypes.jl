module RetTypes
using SH, Bins

export Ret, retMid, valAt, valAtPrice

struct Ret
    center::Float64
    vals::Vector{Float64}
end
Ret() = Ret(1.0, fill(1.0, numVals()))
SH.getCenter(r::Ret) = r.center
SH.getVals(r::Ret) = r.vals
# SH.valAt(r::Ret, i::Int) = r.vals[i]
Base.getindex(r::Ret, i::Int) = r.vals[i]
# SH.getValMid(r::Ret) = r.vals[binsMid()]

valAtPrice(ret::Ret, x::Float64) = valAt(ret, x/getCenter(ret))
function SH.valAt(ret::Ret, x::Float64)
    if x >= binsRight()
        return ret[numVals()]
    elseif x <= binsLeft()
        return ret[1]
    end
    bl = binLeftOf(x)
    br = binRightOf(x)
    if bl == br
        return ret[bl]
    end
    left = ret[bl]
    right = ret[br]
    ratio = (x - binX(bl))/(br - bl)
    return left + ratio * (right - left)
end

end