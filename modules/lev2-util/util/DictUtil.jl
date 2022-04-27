module DictUtil
using JSON3
using BaseUtil, CollUtil

export validKV, tryKey, tryKeys, getLastDict
export toDict
export parseJson, jsonToDict
export walkKeys

# validKV(d::Dict, k) = haskey(d, k) && (v = d[k] ; isSomething(v))

(tryKey(d::Dict{K,T}, key::K)::Union{Nothing,T}) where {K,T} = haskey(d, key) && (dk = d[key]; !isnothing(dk)) ? dk : nothing
(tryKey(d::Dict{K,T2}, key::K, els::T)::T) where {K,T2,T<:T2} = haskey(d, key) && (dk = d[key]; !isnothing(dk)) ? dk : els
tryKeys(d::Dict{<:Any,<:Any}, els, keys...) = (val = find(!isnothing, map(k->tryKey(d,k), keys)); return isnothing(val) ? els : val)

function getLastDict(d::Dict, keys...)
    cur = d
    for k in keys
        if haskey(cur, k)
            cur = cur[k]
            if !isa(cur, Dict)
                break
            end
        else
            break
        end
    end
    return cur
end

toDict(nt::NamedTuple)::Dict{Symbol,Any} = Dict(pairs(nt))

(parseJson(s::AbstractString, ::Type{T})::T) where T = JSON3.read(s, T)
jsonToDict(s::AbstractString) = JSON3.read(s, Dict)

function walkKeys(f, d::Dict)::Bool
    for k in keys(d)
        f(d, k) || return false
        walkKeys(f, d[k])
    end
    return true
end
function walkKeys(f, v::Union{AbstractVector,Tuple})::Bool
    for x in v
        walkKeys(f, x) || return false
    end
    return true
end
walkKeys(f, x) = return true

export jsonPretty
function jsonPretty(o)
    buf = IOBuffer()
    JSON3.pretty(buf, o)
    return String(take!(buf))
end

end