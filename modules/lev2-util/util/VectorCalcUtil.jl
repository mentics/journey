module VectorCalcUtil
using BaseTypes

export avg, smooth, smooth!, fitIn01, fitIn01!
export normalize, normalize!, normalizeI, normalizeI!, normalize1, normalize1I!

avg(vs) = sum(vs) / length(vs)

# NOTE: these assume elements > 0
(normalize!(v::T)::T) where T = v ./= sum(v)
(normalize(v::T)::T) where T = v ./ sum(v)

normalizeI!(v::T) where T = (s = sum(v) ; v ./= s ; return (v, 1/s))
normalizeI(v::T) where T = (v2 = copy(v) ; return normalize!(v2))

# NOTE: these shift all elements above 0 and then normalize
# TODO: can be optimized (less passes)
(normalize1I!(v::T)) where T = (mn = minimum(v) ; v .-= mn ; s = sum(v) ; v ./= s ; return (v, -mn, 1/s))
(normalize1(v::T)::T) where T = (v2 = copy(v) ; normalize1I!(v2) ; return v2)

fitIn01(v::Vector{Float64}) = (cv = copy(v) ; fitIn01!(cv) ; return cv)
function fitIn01!(v::Vector{Float64})
    mn, mx = extrema(v)
    k = mx - mn
    k != 0 || return (-mn, 0.0)
    replace!(x -> (x - mn) / k, v)
    return (-mn, 1.0/k)
end

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

end