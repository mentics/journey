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

# TODO: create a macro that calls a function with a string IO as firct arg and returns the string
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