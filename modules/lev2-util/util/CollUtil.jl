module CollUtil

export find, ensureVector, sortExp!, sortTuple, del!, prinsert!
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

ensureVector(o) = isnothing(o) ? [] : (isa(o, Array) ? o : [o])

# sortExp(f, v; kws...) = v[sortperm(f.(v))]
# sortExp!(f, v; kws...) = (sort!(StructArray((f.(v), v)); kws..., by=first); return)
sortExp!(f, v; kws...) = Base.permute!!(v, sortperm(f.(v); kws...))

# TODO: optimize this
sortTuple(by, tup) = Tuple(sort!(collect(tup); by))
# uniqueSortTuple(tup; kws...) = Tuple(unique!(sort!(collect(tup); kws...)))

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