module Keepers
using DataStructures

struct Keeper{T}
    store::CircularBuffer{T}
    function Keeper{T}(size) where T
        return new(CircularBuffer{T}(size))
    end
end

function Base.push!(keeper::Keeper{T}, x::T) where T
    if isempty(keeper.store) || length(keeper.store) < capacity(keeper.store) || x > keeper.store[end]
        pushfirst!(keeper.store, x)
        sort!(keeper.store; rev=true)
    end
end

(Base.first(keeper::Keeper{T})::T) where T = first(keeper.store)
Base.iterate(keeper::Keeper{T}) where T = iterate(keeper.store)
Base.iterate(keeper::Keeper{T}, s) where T = iterate(keeper.store, s)

function Base.merge(keepers::Vector{Keeper{T}}, size)::Keeper{T} where T
    keeper = Keeper{T}(size)
    for x in Iterators.flatten(keepers)
        push!(keeper, x)
    end
    return keeper
end

# struct KeepN{T}
#     heap::BinaryMinMaxHeap{T}
#     n::Int
#     function KeepN{T}(size) where T
#         heap = BinaryMinMaxHeap{T}()
#         sizehint!(heap, size)
#         return new(heap, size)
#     end
# end

# function Base.push!(keepn::KeepN{T}, x::T) where T
#     if isempty(keepn.heap) || x > minimum(keepn.heap)
#         if length(keepn.heap) == keepn.n
#             popmin!(keepn.heap)
#         end
#         push!(keepn.heap, x)
#     end
# end

# (Base.first(keepn::KeepN{T})::T) where T = first(keepn.heap)

# using BenchmarkTools
# function perfKeep(n=100)
#     global k1 = KeepN{Int}(n)
#     global k3 = Keeper{Int}(n)
#     for i in 1:(2*n)
#         push!(k1, i)
#         push!(k3, i)
#     end
#     println(k1)
#     println(k3)
#     x = 100*n
#     println("Keep1:")
#     display(@benchmark push!($k1, x) setup=(x = inc()))
#     display(@benchmark first($k1))
#     println("Keep3:")
#     display(@benchmark push!($k3, x) setup=(x = inc()))
#     display(@benchmark first($k3))
#     println(k1)
#     println(k3)
# end

# const incref2 = Ref(0)

# inc() = incref2[] += 1

end