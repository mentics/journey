module MathUtil
using BaseTypes

export VARIA_LEN
export sqrt0, log0, varia
export indCombos
export sigmoid, keepPos, checkRange

sqrt0(x::Float64)::Float64 = sqrt(x + 1.0) - 1.0
# sqrt0(v::Vector{Float64})::Vector{Float64} = sqrt0.(v)

log0(x::Float64)::Float64 = log(x + 1.0)
# log0(v::Vector{Float64})::Vector{Float64} = log0.(v)

varia(x::Float64)::Vector{Float64} = [x, sqrt0(x), log0(x), x^2]
varia(v::AVec{Float64})::Array{Float64,2} = hcat(v, sqrt0.(v), log0.(v), v.^2)
VARIA_LEN = length(varia(1.0))

# function linsum(xs, p, off)
#     sum = 0.0
#     for i in eachindex(xs)
#         sum += p[off+i-1] * xs[i]
#     end
#     return sum
# end
# multis(singles) = collect(Iterators.flatten(map(x -> x[1]*x[2], Iterators.product(singles, singles))))
using Combinatorics
# products(xs) = [(xs[i1]*xs[i2], i1, i2) for (i1, i2) in combinations(1:length(xs), 2)]
indCombos(len, num) = combinations(1:len, num)

function sigmoid(mn, mx, half)
    k = -log2(3) / half
    diff = mx - mn
    return x -> mn + diff / (1 + 2^(k * x))
end
function sigmoid(mn, mx, half, xoff)
    k = -log2(3) / half
    diff = mx - mn
    return x -> mn + diff / (1 + 2^(k * (x - xoff)))
end

keepPos(x) = x < 1.0 ? 2^x/2 : x

checkRange(mn, mx, itr::NamedTuple) = for (k,x) in pairs(itr)
    mn <= x <= mx || error("Value out of bounds for key $(k): $(mn) <= $(x) <= $(mx)")
end

end