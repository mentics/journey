module VectorCalcUtil
using BaseTypes

export vcu
const vcu = @__MODULE__

export avg, smooth, smooth!, fitIn01, fitIn01!
export normalize, normalize!, normalizeI, normalizeI!, normalize1, normalize1I!

avg(vs) = ( @assert length(vs) > 0 ; sum(vs) / length(vs) )

# NOTE: these assume elements > 0
(normalize!(v::T)::T) where T = v ./= sum(v)
(normalize(v::T)::T) where T = v ./ sum(v)

normalizeI!(v::T) where T = (s = sum(v) ; v ./= s ; return (v, 1/s))
normalizeI(v::T) where T = (v2 = copy(v) ; return normalize!(v2))

# NOTE: these shift all elements above 0 and then normalize
# TODO: can be optimized (less passes)
(normalize1I!(v::T)) where T = (mn = minimum(v) ; v .-= mn ; s = sum(v) ; v ./= s ; return (v, -mn, 1/s))
(normalize1(v::T)::T) where T = (v2 = copy(v) ; normalize1I!(v2) ; return v2)

fit_01(v::Vector{Float64}) = (cv = copy(v) ; (ad, scale) = fitIn01!(cv) ; return (;v=cv, ad, scale))
fitIn01(v::Vector{Float64}) = (cv = copy(v) ; fitIn01!(cv) ; return cv)
function fitIn01!(v::Vector{Float64})
    mn, mx = extrema(v)
    k = mx - mn
    k != 0 || return (-mn, 0.0)
    replace!(x -> (x - mn) / k, v)
    return (-mn, 1.0/k)
end

# fit_01(v::Vector{Float64}) = (cv = copy(v) ; fitIn01!(cv) ; return cv)
# function fit_01!(v::Vector{Float64})
#     mn, mx = extrema(v)
#     k = mx - mn
#     k != 0 || return (-mn, 0.0)
#     replace!(x -> (x - mn) / k, v)
#     return (-mn, 1.0/k)
# end

smooth(v::AVec{Float64}, cnt::Int=10)::AVec{Float64} = smooth!(copy(v), cnt)
function smooth!(v::AVec{Float64}, cnt::Int=10)::AVec{Float64}
    len = length(v)
    for _ in 1:cnt
        for j in 3:(len-1)
            nv = 0.5 * (v[j] + v[j-1])
            v[j-1] = nv
            v[j] = nv
        end
        for j in (len-1):-1:3
            nv = 0.5 * (v[j] + v[j-1])
            v[j-1] = nv
            v[j] = nv
        end
    end
    return v
end

function interleave!(out, vs)
    vs_count = length(vs)
    for (iv, v) in enumerate(vs)
        for (i, x) in enumerate(v)
            out[(i - 1) * vs_count + iv] = x
        end
    end
end

function interleave!(out, vs, scales)
    vs_count = length(vs)
    for (iv, v) in enumerate(vs)
        for (i, x) in enumerate(v)
            out[(i - 1) * vs_count + iv] = x / scales[iv]
        end
    end
end

# function conv(v::AbstractVector{T}, kernel::AbstractVector{T}) where T
#     vlen = length(v)
#     klen = length(kernel)
#     klen2 = klen ÷ 2
#     # klen2right = klen - klen2
#     res = Vector{T}(undef, vlen)
#     for i in eachindex(v)
#         # inds = i <= klen2 ? ((klen2 - i):(klen - klen2 - 1)) : (-klen2:(klen - klen2 - 1))
#         left = i <= klen2 ? (i - klen2 + 1) : -klen2
#         right = (i + klen2) > vlen ? (1 + vlen - (i + klen2)) : klen2
#         inds = left:right
#         @show i vlen klen2 inds
#         res[i] = zero(T)
#         for offset in inds
#             iv = i + offset
#             ik = offset + klen2 + 1
#             @show offset iv ik
#             res[i] += v[iv] * kernel[ik]
#             println("Added to res[$(i)] = $(v[iv]) * $(kernel[ik])")
#         end
#     end
#     return res
# end

function conv(v::AbstractVector{T}, kernel::AbstractVector{T}) where T
    vlen = length(v)
    offset = -length(kernel) ÷ 2 - 1 # -1 for 1-indexing
    res = Vector{T}(undef, vlen)
    for i in eachindex(v)
        res[i] = zero(T)
        for j in eachindex(kernel)
            ind = i + j + offset
            0 < ind <= vlen  || continue
            res[i] += v[ind] * kernel[j]
        end
    end
    return res
end

end
