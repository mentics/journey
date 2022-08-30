module MLUtil

unflatten(dims::NTuple{N,Int}) where N = x -> reshape(x, dims)

end