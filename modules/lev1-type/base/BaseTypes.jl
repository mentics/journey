module BaseTypes
using FixedPointDecimals

export AStr, Coll, AVec, Currency, C, PriceT, P, F

const AStr = AbstractString
const AVec = AbstractVector
const Coll{T,N} = Union{AVec{T},NTuple{N,T}}

const Currency = FixedDecimal{Int,3}
C(x::Real) = Currency(x)

const PriceT = FixedDecimal{Int,2}
P(x::Real) = PriceT(x)

F(x::Real) = Float64(x)

function Base.show(io::IO, x::Currency)
    print(io, x)
end
function Base.show(io::IO, ::Type{Currency})
    print(io, "Currency")
end

end