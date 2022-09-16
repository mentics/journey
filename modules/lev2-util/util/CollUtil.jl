module CollUtil
using BaseTypes

export concat, find, findFrom, ensureVector, sortExp!, del!, prinsert!
# uniqueSortTuple

simEmpty(m) = similar(m, eltype(m), tupZeroLast(size(m)))
tupZeroLast(tup::Tuple) = (tup[1:end-1]..., 0)

(concat(a::AVec{T}, b::AVec{T})::Vector{T}) where T = vcat(a, b)
# (concat(a::NTuple{N,T}, b::NTuple{N,T})::Vector{T}) where N,T = collect(Iterators.flatten((a, b)))
(concat(a::Coll{T}, b::Coll{T})::Vector{T}) where T = collect(Iterators.flatten((a, b)))

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

export simNt, ntvFromNt, ntvFromVnt
simNt(f::Union{Function,Type}, nt::NamedTuple)::NamedTuple = ( ks = keys(nt) ; NamedTuple{ks}(map(_->f(), ks)) )
# simNt(val, nt::NamedTuple)::NamedTuple = ( ks = keys(nt) ; NamedTuple{ks}(fill(val, length(ks))) )
ntvFromNt(nt::NamedTuple, elt=Float64)::NamedTuple = simNt(Vector{elt}, nt)
function ntvFromVnt(vnt::Vector{NamedTuple})::NamedTuple
    # TODO: could optimize with size hint
    ntv = ntvFromNt(vnt[1])
    ks = keys(vnt[1])
    for nt in vnt
        for k in ks
            push!(ntv[k], nt[k])
        end
    end
    return ntv
end

export roll2
function roll2(f, iter)
    prev, rest = Iterators.peel(iter)
    for x in rest
        f(prev, x)
        prev = x
    end
end

export mapRoll2
function mapRoll2(f, iter)
    prev, rest = Iterators.peel(iter)
    return map(rest) do x
        res = f(prev, x)
        prev = x
        return res
    end
end

function tupsToMat(tupVec::T) where T<:AVec
    return Tuple(forTupi(tupVec, i) for i in eachindex(tupVec[1]))
end

function forTupi(tupVec, tupi)
    tup = tupVec[1][tupi]
    numRows = length(tup)
    mat = Array{eltype(tup)}(undef, numRows, length(tupVec))
    for i in eachindex(tupVec)
        mat[:,i] .= tupVec[i][tupi]
    end
    return mat
end

end