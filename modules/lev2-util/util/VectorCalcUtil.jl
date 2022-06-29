module VectorCalcUtil
using BaseTypes

export avg, normalize!, smooth, smooth!

avg(vs) = sum(vs) / length(vs)

(normalize!(vs::T)::T) where T = vs ./= sum(vs)

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