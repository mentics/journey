using CUDA
import SliceMap, Zygote, Flux
CUDA.allowscalar(false)
function func(m, k, data)
    y = SliceMap.slicemap(m; dims=1) do col
        col .* k
    end
    return sum(y)
end
function test_gpu()
    m = fill(1.0, 3, 10) |> Flux.gpu
    data = fill(2.0, 3, 10) |> Flux.gpu
    k = [0.7, 0.5, 0.5] |> Flux.gpu
    Zygote.gradient((m, k) -> func(m, k, data), m, k)
end

function func(m)
    prod(m)
end
function test_prod()
    m = zeros(3) |> Flux.gpu
    Zygote.gradient(func, m)
end


import SliceMap, Zygote
function op(dx, k)
    return dx ./ k
end
# function func(m, k, data)
#     @assert size(m) == (3, 10)
#     @assert size(k) == (3,)
#     @assert size(data) == (3, 10)
#     y = SliceMap.mapcols(m) do col
#         diff = data .- col
#         @assert size(diff) == (3, 10)
#         s1 = SliceMap.mapcols(diff) do diff_col
#             op(diff_col, k)
#         end
#         @assert size(s1) == (3, 10)
#         return sum(s1)
#     end
#     @assert size(y) == (1, 10)
#     return sum(y)
# end

function test()
    m = fill(1.0, 3, 10)
    data = fill(2.0, 3, 10)
    k = [0.7, 0.5, 0.5]
    grad = Zygote.gradient((m, k) -> func(m, k, data), m, k)
    val1 = func(m, k, data)
    k2 = k .+ 1.0
    val2 = func(m, k2, data)
    # @assert val1 != val2
    return grad
end

function test2()
    m = fill(1.0, 3, 10)
    k = [0.7, 0.5, 0.5]
    Zygote.gradient((m, k) -> sum(k .* m), m, k)
end
