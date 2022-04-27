init1(N) = fill(Inf, N)
function min_values1(N, collection, v)
    @inbounds for x in collection
        if x < last(v)
            v[N] = x
            sort!(v)
        end
    end
    v
end
vr = min_values1(3, [4], [1,3,5])
# @info "check" vr
@assert vr == [1,3,4]
vr = min_values1(3, [6], [1,3,5])
# @info "check2" vr
@assert vr == [1,3,5]
vr = min_values1(3, [1], [1,3,5])
# @info "check3" vr
@assert vr == [1,1,3]
vr = min_values1(3, [0], [1,3,5])
# @info "check4" vr
@assert vr == [0,1,3]
vr = min_values1(3, [2], [1,3,5])
# @info "check5" vr
@assert vr == [1,2,3]

init2(N) = fill(Inf, 2N)
function min_values2(N, collection, v)
    i = 1
    @inbounds for x in collection
        v[i] = x

        i += 1
        if i > lastindex(v)
            partialsort!(v, 1:N)
            i = N+1
        end
    end
    partialsort!(v, 1:N)
end

using DataStructures

struct BoundedBinaryHeap{T, O <: Base.Ordering} <: DataStructures.AbstractHeap{T}
    ordering::O
    valtree::Vector{T}
    n::Int # maximum length

    function BoundedBinaryHeap{T}(n::Integer, ordering::Base.Ordering) where T
        n ≥ 1 || throw(ArgumentError("max heap size $n must be ≥ 1"))
        new{T, typeof(ordering)}(ordering, sizehint!(Vector{T}(), n), n)
    end

    function BoundedBinaryHeap{T}(n::Integer, ordering::Base.Ordering, xs::AbstractVector) where T
        n ≥ length(xs) || throw(ArgumentError("initial array is larger than max heap size $n"))
        valtree = sizehint!(DataStructures.heapify(xs, ordering), n)
        new{T, typeof(ordering)}(ordering, valtree, n)
    end
end

BoundedBinaryHeap(n::Integer, ordering::Base.Ordering, xs::AbstractVector{T}) where T = BoundedBinaryHeap{T}(n, ordering, xs)

BoundedBinaryHeap{T, O}(n::Integer) where {T, O<:Base.Ordering} = BoundedBinaryHeap{T}(n, O())
BoundedBinaryHeap{T, O}(n::Integer, xs::AbstractVector) where {T, O<:Base.Ordering} = BoundedBinaryHeap{T}(n, O(), xs)

Base.length(h::BoundedBinaryHeap) = length(h.valtree)
Base.isempty(h::BoundedBinaryHeap) = isempty(h.valtree)
@inline Base.first(h::BoundedBinaryHeap) = h.valtree[1]

@inline Base.pop!(h::BoundedBinaryHeap) = DataStructures.heappop!(h.valtree, h.ordering)

function Base.push!(h::BoundedBinaryHeap, v)
    if length(h) < h.n
        DataStructures.heappush!(h.valtree, v, h.ordering)
    elseif Base.Order.lt(h.ordering, @inbounds(h.valtree[1]), v)
        DataStructures.percolate_down!(h.valtree, 1, v, h.ordering)
    end
    return h
end

init3(N) = BoundedBinaryHeap{Float64}(N, Base.Order.Reverse)
function min_values3(collection, h)
    @inbounds for x in collection
        push!(h, x)
    end
    h
end

init4(N) = fill(Inf, N)
function min_values4(N, collection, v)
    @inbounds for x in collection
        x < v[N] || continue
        v[N] = x
        for i in N-1:-1:1
            x < v[i] || break
            (v[i], v[i+1]) = (x, v[i])
        end
    end
    v
end
vr = min_values4(3, [4], [1,3,5])
# @info "check" vr
@assert vr == [1,3,4]
vr = min_values4(3, [6], [1,3,5])
# @info "check2" vr
@assert vr == [1,3,5]
vr = min_values4(3, [1], [1,3,5])
# @info "check3" vr
@assert vr == [1,1,3]
vr = min_values4(3, [0], [1,3,5])
# @info "check4" vr
@assert vr == [0,1,3]
vr = min_values4(3, [2], [1,3,5])
# @info "check5" vr
@assert vr == [1,2,3]

# SortedSet can't have duplicates, so can't fill with Inf
init5(N) = SortedSet{Float64}(Base.Order.ReverseOrdering(), 1000000:(1000000+N-1))
function min_values5(collection, v)
    @inbounds for x in collection
        if x < first(v)
            delete!(v.bt, DataStructures.beginloc(v.bt)) # pop!(v)
            DataStructures.insert!(v.bt, convert(DataStructures.keytype(v),x), nothing, false) # push!(v, x)
        end
    end
    v
end

init6(N) = fill(Inf, N)
function min_values6(N, collection, v)
    # ptr = pointer(v)
    @inbounds for x in collection
        x < v[N] || continue
        # i = N-1
        # while (i >= 1 && x < v[i])
        #     i -= 1
        # end
        # unsafe_copyto!(pointer(v, i+2), pointer(v, i+1), N-i)
        # v[i+1] = x

        i = searchsortedfirst(v, x)
        if i < N
            # unsafe_copyto!(ptr + (i << 3), ptr + ((i-1) << 3), N-i)
            unsafe_copyto!(pointer(v, i+1), pointer(v, i), N-i)
        end
        v[i] = x
    end
    v
end
vr = min_values6(3, [4], [1,3,5])
# @info "check" vr
@assert vr == [1,3,4]
vr = min_values6(3, [6], [1,3,5])
# @info "check2" vr
@assert vr == [1,3,5]
vr = min_values6(3, [1], [1,3,5])
# @info "check3" vr
@assert vr == [1,1,3]
vr = min_values6(3, [0], [1,3,5])
# @info "check4" vr
@assert vr == [0,1,3]
vr = min_values6(3, [2], [1,3,5])
# @info "check5" vr
@assert vr == [1,2,3]

using BenchmarkTools
x = randn(1_000_000)
N = 1000

v1 = init1(N)
v1 = min_values1(N, x, v1);
v11 = init1(N)
@btime min_values1($N, $x, $v11);

v2 = init2(N)
v2 = min_values2(N, x, v2);
@assert last(v1) == last(v2)
v22 = init2(N)
@btime min_values2($N, $x, $v22);

v3 = init3(N)
v3 = min_values3(x, v3);
@assert last(v1) == pop!(v3)
v33 = init3(N)
@btime min_values3($x, $v33);

v4 = init4(N)
v4 = min_values4(N, x, v4);
@assert last(v1) == last(v4)
v44 = init4(N)
@btime min_values4($N, $x, $v44);

v5 = init5(N);
v5 = min_values5(x, v5);
# @info "check" first(v1) last(v1) first(v5) last(v5) length(v1) length(v5)
@assert last(v1) == first(v5);
v55 = init5(N);
@btime min_values5($x, $v55);

v6 = init6(N);
v6 = min_values6(N, x, v6);
# @info "check" first(v1) last(v1) first(v6) last(v6) length(v1) length(v6)
@assert last(v1) == last(v6);
v66 = init6(N);
@btime min_values6($N, $x, $v66);
