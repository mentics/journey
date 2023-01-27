module CollUtil
using BaseTypes

export cu
const cu = @__MODULE__

export concat, find, findFrom, ensureVector, sortExp!, del!, prinsert!
# uniqueSortTuple

simEmpty(m) = similar(m, eltype(m), tupSetLast(size(m), 0))
# tupZeroLast(tup::Tuple) = (tup[1:end-1]..., 0)
tupSetLast(tup::Tuple, val) = (tup[1:end-1]..., val)
tupSetFirst(tup::Tuple, val) = (val, tup[2:end]...)

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

function (find(f, v::AVec{T}, inds)::Tuple{Int,Union{Nothing,T}}) where T
    for i in inds
        x = v[i]
        res = f(x)
        res == -1 && break
        res == 1 && return (i, x)
    end
    return (0, nothing)
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

# ntadd(nt1::NamedTuple, nt2::NamedTuple) = NamedTuple{fieldnames(nt1)}(values(nt1) .+ values(nt2))

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

function findDupes!(x::AbstractArray{T}; by=identity) where T
    sort!(x; by)
    dupes = Set{T}()
    for i in eachindex(x)[2:end]
        # if (isequal(by(x[i]), by(x[i-1])) && (isempty(dupes) || !isequal(dupes[end], x[i])))
        if isequal(by(x[i]), by(x[i-1]))
            push!(dupes, x[i-1])
            push!(dupes, x[i])
        end
    end
    return dupes
end

export flat, flatmap, mapflatmap
flat(x...) = Iterators.flatten(x)
flatmap(f, coll) = Iterators.flatten(Iterators.map(f, coll))
mapflatmap(fout, fin, coll) = Iterators.map(fout, Iterators.flatten(Iterators.map(fin, coll)))

gteev(v, x) = v[gtee(v, x)]
function gtee(v, x)::Int
    i = searchsortedfirst(v, x)
    return min(i, lastindex(v))
end

lteev(v, x) = v[ltee(v, x)]
function ltee(v, x)::Int
    i = searchsortedlast(v, x)
    return max(i, firstindex(v))
end

function sublist(v, from, to)
    left = searchsortedfirst(v, from)
    right = searchsortedlast(v, to)
    return v[left:right]
end

#=
This should do exactly what accumulate! does, but in some testing, this might be a lot faster
f1(a, x) = (a[2], a[2]+x);
v1 = rand(100);
buf1 = Vector{NTuple{2,Float64}}(undef, length(v1));
init = (0.0, 0.0);
r1 = @btime CollUtil.maps!($f1, $buf1, $v1, $init);
r2 = @btime accumulate!($f1, $buf1, $v1; init=$init);
@assert r1 == r2
=#
# function maps!(f, buf, v, init)
#     s = init
#     i = 0
#     for x in v
#         i += 1
#         s = f(s, x)
#         buf[i] = s
#     end
#     return buf
# end

struct _DefinitelyNothingThisTime end
function accum!(op, B, A; dims::Union{Integer, Nothing} = nothing, init = _DefinitelyNothingThisTime)
    Base._accumulate!(op, B, A, dims, init === _DefinitelyNothingThisTime ? nothing : Some(init))
end

# function accumul!(op, B, A; dims::Union{Integer, Nothing} = nothing, kw...)
#     if isnothing(init)
#         Base._accumulate!(op, B, A, dims, nothing)
#     else
#         Base._accumulate!(op, B, A, dims, Some(init))
#     end
# end

# # function accum!(op, B, A; dims::Union{Integer, Nothing} = nothing)
# #     Base._accumulate!(op, B, A, dims, nothing)
# # end

# function accumul!(op, B, A; dims::Union{Integer, Nothing} = nothing, kw...)
#     if isempty(kw)
#         Base._accumulate!(op, B, A, dims, nothing)
#     else
#         ks = keys(kw)
#         check = length(ks) === 1 && first(ks) === :init
#         if check
#             x = first(values(kw))
#             x2 = Some(x)
#             Base._accumulate!(op, B, A, dims, x2)
#         end
#         # @time ks = keys(kw)
#         # @time check = length(ks) === 1 && first(ks) === :init
#         # if check
#         #     @time x = first(values(kw))
#         #     @time x2 = Some(x)
#         #     @time Base._accumulate!(op, B, A, dims, x2)
#         # else
#         #     throw(ArgumentError("acccumulate! does not support the keyword arguments $(setdiff(keys(kw), (:init,)))"))
#         # end
#     end
# end

end