module CollUtil
using BaseTypes

export cu
const cu = @__MODULE__

export concat, find, findFrom, ensureVector, sortExp!, del!, prinsert!, flatvec
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

function find2(f, pred, itr)
    for x in itr
        y = f(x)
        pred(y) && return (x, y)
    end
    return nothing
end

# function find3(f, pred, itr)
#     i = 0
#     for x in itr
#         i += 1
#         y = f(x)
#         pred(y) && return (i, x, y)
#     end
#     return nothing
# end

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

function findMaxDom(f, itr)
    mx = -Inf
    retx = nothing
    for x in itr
        y = f(x)
        if !isnothing(y) && y > mx
            mx = y
            retx = x
        end
    end
    return retx
end

ensureVector(o) = isnothing(o) ? [] : (isa(o, Array) ? o : [o])

# sortExp(f, v; kws...) = v[sortperm(f.(v))]
# sortExp!(f, v; kws...) = (sort!(StructArray((f.(v), v)); kws..., by=first); return)
sortExp!(f, v; kws...) = Base.permute!!(v, sortperm(f.(v); kws...))

# https://github.com/JeffreySarnoff/SortingNetworks.jl/blob/master/src/swapsort.jl
using SH

f1(x1, x2) = getStrike(x1[1]) < getStrike(x2[1])


function sortupleperm2(x1, x2)
    return getStrike(x1[1]) > 1 ? (1,2,3,4) : (2,3,4,5)
end

function sortupleperm3(lt, x1, x2)
    return getStrike(x1[1]) > 1 ? (1,2,3,4) : (2,3,4,5)
end


(sortupleperm8(lt, t::NTuple{4,T}, t2::NTuple{4,T2})::NTuple{4,Int}) where {T,T2} = sortupleperm9(lt, t..., t2...)
function sortupleperm9(lt, a1, b1, c1, d1, a2, b2, c2, d2)::NTuple{4,Int}
    # return getStrike(a1) > 1 ? (1,2,3,4) : (2,3,4,5) # lt((a1, a2), (b1, b2))
    a, b, ia, ib = mmlt(lt, (a1, a2), (b1, b2), 1, 2)
    # return ia, ib, ia, ib
    c, d, ic, id = mmlt(lt, (c1, c2), (d1, d2), 3, 4)
    a, c, ia, ic = mmlt(lt, a, c, ia, ic)
    b, d, ib, id = mmlt(lt, b, d, ib, id)
    b, c, ib, ic = mmlt(lt, b, c, ib, ic)
    return ia, ib, ic, id
end

(sortupleperm(lt, t::NTuple{4,T})::NTuple{4,Int}) where T = sortupleperm(lt, t...)
function sortupleperm(lt, a, b, c, d)::NTuple{4,Int}
    a, b, ia, ib = mmlt(lt, a, b, 1, 2)
    c, d, ic, id = mmlt(lt, c, d, 3, 4)
    a, c, ia, ic = mmlt(lt, a, c, ia, ic)
    b, d, ib, id = mmlt(lt, b, d, ib, id)
    b, c, ib, ic = mmlt(lt, b, c, ib, ic)
    return ia, ib, ic, id
end
mmlt(lt, a, b, ia, ib) = lt(a, b) ? (a, b, ia, ib) : (b, a, ib, ia)
(tupleperm(tup::NTuple{4,T}, inds::NTuple{4,Integer})::NTuple{4,T}) where T = (tup[inds[1]], tup[inds[2]], tup[inds[3]], tup[inds[4]])

(sortuple(t::NTuple{4,T}, by)::NTuple{4,T}) where T = sortuple(t..., by)
(sortuple(by::Function, t::NTuple{4,T})::NTuple{4,T}) where T = sortuple(t..., by)
(sortuple(by::Function, x1::T, x2::T, x3::T, x4::T)::NTuple{4,T}) where T = sortuple(x1, x2, x3, x4, by)
function sortuple(x1::T, x2::T, x3::T, x4::T, by)::NTuple{4,T} where T
    a, b, c, d = decorate(by, x1, x2, x3, x4)
    a, b = minmax2(a, b)
    c, d = minmax2(c, d)
    a, c = minmax2(a, c)
    b, d = minmax2(b, d)
    b, c = minmax2(b, c)
    return a[2], b[2], c[2], d[2]
end

(sortuple(t::NTuple{3,T}, by)::NTuple{3,T}) where T = sortuple(t..., by)
(sortuple(by::Function, t::NTuple{3,T})::NTuple{3,T}) where T = sortuple(t..., by)
(sortuple(by::Function, x1::T, x2::T, x3::T)::NTuple{3,T}) where T = sortuple(x1, x2, x3, by)
function sortuple(x1::T, x2::T, x3::T, by)::NTuple{3,T} where T
    a, b, c = decorate(by, x1, x2, x3)
    a, b = minmax2(a, b)
    a, c = minmax2(a, c)
    b, c = minmax2(b, c)
    return a[2], b[2], c[2]
end

# sortargs(args...) = sortuple(identity, args)

function sortuple(by::Function, x1::T, x2::T)::NTuple{2,T} where T
    a, b = decorate(by, x1, x2)
    a, b = minmax2(a, b)
    return a[2], b[2]
end

function decorate(by, x1, x2, x3, x4)
    return ((by(x1), x1), (by(x2), x2), (by(x3), x3), (by(x4), x4))
end
function decorate(by, x1, x2, x3)
    return ((by(x1), x1), (by(x2), x2), (by(x3), x3))
end
function decorate(by, x1, x2)
    return ((by(x1), x1), (by(x2), x2))
end
minmax2(x1, x2) = x1[1] < x2[1] ? (x1, x2) : (x2, x1)

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
flatvec(v) = collect(Iterators.flatten(v))
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

function between(v, from, to)::Tuple{Int,Int}
    left = searchsortedfirst(v, from)
    right = searchsortedlast(v, to)
    return (max(left, firstindex(v)), min(right, lastindex(v)))
end


function sublist(v, from, to)
    left = searchsortedfirst(v, from)
    right = searchsortedlast(v, to)
    return @view v[left:right]
end
function countSublist(v, from, to)
    left = searchsortedfirst(v, from)
    right = searchsortedlast(v, to)
    return right - left + 1
end

#=
This should do exactly what accumulate! does, but in some testing, this might be a lot faster
f1(a, x) = (a[2], a[2]+x);
v1 = rand(100);
buf1 = Vector{NTuple{2,Float64}}(undef, length(v1));
init = (0.0, 0.0);
r1 = @benchmark CollUtil.maps!($f1, $buf1, $v1, $init);
r2 = @benchmark accumulate!($f1, $buf1, $v1; init=$init);
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

function vtToTv(vt)
    vs = map(x -> Vector{typeof(x)}(), first(vt))
    jinds = eachindex(first(vt))
    for i in eachindex(vt)
        tup = vt[i]
        for j in jinds
            push!(vs[j], tup[j])
        end
    end
    return vs
end

function findextrema(v, from=firstindex(v), to=lastindex(v))
    mni = 1
    mxi = 1
    mn = Inf
    mx = -Inf
    for i in from:to
        x = v[i]
        if x < mn
            mn = x
            mni = i
        end
        if x > mx
            mx = x
            mxi = i
        end
    end
    return ((mni, mn), (mxi, mx))
end

function minkey(d)
    (minkey, minvalue), rest = Iterators.peel(d)
    for (key, value) in rest
        if value < minvalue
            minkey = key
            minvalue = value
        end
    end
    minkey
end

function maxkey(d)
    (minkey, minvalue), rest = Iterators.peel(d)
    for (key, value) in rest
        if value > minvalue
            minkey = key
            minvalue = value
        end
    end
    minkey
end

# # TODO: could write optimized loop for sorted one
# uniqueidx(v) = unique(i -> v[i], eachindex(v))

# function pushsortedunique!(into::AbstractVector{T}, vals::AbstractVector{T}, into2::AbstractVector{B}, vals2::AbstractVector{B}) where {T,B}

function pushsortedunique!(into, vals)
    last = first(vals)
    push!(into, last)
    for i in eachindex(vals)
        x = vals[i]
        if x !== last
            push!(into, x)
            last = x
        end
    end
    return nothing
end

function pushsortedunique!(into, vals, into2, vals2)
    last = first(vals)
    push!(into, last)
    push!(into2, first(vals2))
    for i in eachindex(vals)
        x = vals[i]
        if x !== last
            push!(into, x)
            push!(into2, vals2[i])
            last = x
        end
    end
    return nothing
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

# # Test if it's faster to for loop vs. broadcast across full array when there's extra
# using BenchmarkTools
# using LoopVectorization
# function testPerfExtra(full=500, len=200)
#     v = Vector{Float64}(undef, full)
#     v[1] = 10000.0
#     v[2] = .5
#     v[3] = 3466453456.234234234
#     println(v[1])
#     k = 1.01
#     display(@benchmark forloop($k, $v, $len))
#     display(@benchmark broad($k, $v))
#     println(v[1])
# end

# @inline function forloop(k, v, len)
#     @turbo for i in 1:len
#         v[i] /= k
#     end
# end

# @inline function broad(k, v)
#     @turbo v ./= k
# end

import StructArrays
function maparray(f, v)
    sa = StructArrays.collect_structarray(f(x) for x in v)
    return StructArrays.components(sa)
end

export are_all_finite
are_all_finite(x::CollT) = isnothing(findfirst(!isfinite, x))

end