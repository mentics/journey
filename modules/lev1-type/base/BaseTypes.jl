module BaseTypes
using FixedPointDecimals, StaticArrays

export AStr, Coll, CollT, CollN, AVec, Currency, C, CZ, PriceT, PT, P, PZ, F, toPT

const AStr = AbstractString
const AVec = AbstractVector
const Coll{N,T} = Union{SVector{N,T},NTuple{N,T}}
const CollT{T} = Union{SVector{N,T},AVec{T},NTuple{N,T}} where N
const CollN{N} = Union{SVector{N,T},NTuple{N,T}} where T

const Currency = FixedDecimal{Int,3}
C(x::Real) = Currency(x)
Base.Int(x::Currency) = FixedPointDecimals.value(x)
const CZ = C(0)

const PriceT = FixedDecimal{Int,2}
const PT = PriceT
P(x::Real) = PriceT(x)
toPT(c::Real, r::RoundingMode=RoundNearest)::PT = PT(round(c, r; digits=2))
const PZ = P(0)

F(x::Real) = Float64(x)

# TODO: create @assert replacement that I can disable and that auto prints everything in expression
export @str
macro str(exs...)
    args = []
    for ex in exs
        push!(args, sprint(Base.show_unquoted, ex), " = ", esc(ex), '\n')
    end
    return :(string($(args...)))
end
# macro str(exs...)
#     blk = Expr(:block)
#     for ex in exs
#         push!(blk.args, :(string($(sprint(Base.show_unquoted,ex)*" = "),
#                                   repr(begin local value = $(esc(ex)) end))))
#     end
#     isempty(exs) || push!(blk.args, :value)
#     return blk
# end

end