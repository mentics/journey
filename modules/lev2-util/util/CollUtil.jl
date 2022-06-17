module CollUtil

export find, findFrom, ensureVector, sortExp!, del!, prinsert!
# uniqueSortTuple

function del!(pred, v)::Bool
    ind = findfirst(pred, v)
    isnothing(ind) && return false
    deleteat!(v, ind)
    return true
end
(del!(v::Vector{T}, search::T)::Bool) where T = del!(x -> x == search, v)

function find(f, itr)
    for x in itr
        f(x) && return x
    end
    return nothing
end

# function argument f must return trinary: (-1: not found stop, 0: not found continue, 1: found)
function findFrom(f, start::Int, v::Vector{T})::Union{Nothing,Tuple{Int,T}} where T
    for i in start:length(v)
        x = v[i]
        res = f(x)
        res == -1 && return nothing
        res == 1 && return (i, x)
    end
    return nothing
end

ensureVector(o) = isnothing(o) ? [] : (isa(o, Array) ? o : [o])

# sortExp(f, v; kws...) = v[sortperm(f.(v))]
# sortExp!(f, v; kws...) = (sort!(StructArray((f.(v), v)); kws..., by=first); return)
sortExp!(f, v; kws...) = Base.permute!!(v, sortperm(f.(v); kws...))

# # TODO: optimize this
# sortTuple(by, tup) = Tuple(sort!(collect(tup); by))
# # uniqueSortTuple(tup; kws...) = Tuple(unique!(sort!(collect(tup); kws...)))

# sortuple(t::NTuple{4,T})::NTuple{4,T} where T =

function prinsert!(v, newVal)::Bool
    newVal > v[1] || return false
    v[1] = newVal
    @inbounds for i in 2:length(v)
        newVal > v[i] || break
        (v[i-1], v[i]) = (v[i], newVal)
    end
    return true
end

end