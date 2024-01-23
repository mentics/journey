module DictUtil
using JSON3
using BaseUtil, CollUtil

export getnn
function getnn(d::Dict{K,V}, key::K, els) where K where V
    val = get(d, key, els)
    return isnothing(val) ? els : val
end

export validKV, tryKeys, useKey, getLastDict
export toDict
export parseJson, jsonToDict
export walkKeys

# validKV(d::Dict, k) = haskey(d, k) && (v = d[k] ; isSomething(v))

# TODO: use get(coll, key, def) for many things
# (tryKey(d::Dict{K,T}, key::K)::Union{Nothing,T}) where {K,T} = haskey(d, key) && (dk = d[key]; !isnothing(dk)) ? dk : nothing
# (tryKey(d::Dict{K,V}, key::K, els::V2)::V2) where {K,V,V2<:V} = haskey(d, key) && (dk = d[key]; !isnothing(dk)) ? dk : els
# tryKeys(d::Dict{<:Any,<:Any}, els, keys...) = (val = find(!isnothing, map(k->tryKey(d,k), keys)); return isnothing(val) ? els : val)
# function (useKey(finit::Function, d::Dict{K,V}, key::K)::V2) where {K,V,V2<:V}

function useKey(finit::Union{Function,Type}, d::Dict, key)
    haskey(d, key) || (d[key] = finit())
    return d[key]
end
function useKey(d::Dict, key, els)
    haskey(d, key) || (d[key] = els)
    return d[key]
end

function safeKeys(d::Dict, els, keys...)
    cur = d
    for k in keys
        if haskey(cur, k)
            cur = cur[k]
        else
            return els
        end
    end
    return cur
end

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

export incKey
function incKey(d::Dict, k)
    curval = useKey(d, k, 0)
    d[k] = curval + 1
end

export addToKey
function addToKey(d::Dict, k, a)
    valcur = useKey(d, k, zero(a))
    valnew = valcur + a
    d[k] = valnew
    return valnew
end

export incDictKey
function incDictKey(d::Dict, k1, k2)
    d2 = useKey(Dict, d, k1)
    curval = useKey(d2, k2, 0)
    d2[k2] = curval + 1
end

export dictFromVals, dictFromKeys
pairsFromVals(f, vals) = (f(x) => x for x in vals)
dictFromVals(f, vals) = Dict(f(x) => x for x in vals)
dictFromKeys(f, keys) = Dict(x => f(x) for x in keys)
# function toDict(f, v)
#     d = Dict()
#     for x in v
#         @assert !haskey(d, x) "Duplicate key found"
#     end
#     return d
# end

function push_to_key!(d::Dict, k, a::Any)
    v = get!(d, k) do
        Vector{Any}()
    end
    push!(v, a)
    return nothing
end

function to_dict_of_v(f::Function, v::AbstractArray)
    # val = v[1]
    # key = f(v[1])
    # d = Dict{typeof(key),Any}()
    # return d
    d = Dict()
    for x in v
        push_to_key!(d, f(x), x)
    end
    return d
end

setindexes!(d::Dict, pairs) = foreach(pairs) do pair
    d[first(pair)] = last(pair)
end

set_from_vals!(f, d::Dict, vals) = setindexes!(d, pairsFromVals(f, vals))

end