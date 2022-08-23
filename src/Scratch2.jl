using Statistics, Plots
function rollQuantile(data::Vector{Float64}, width::Int, step::Int)
    quants = collect(0.0:0.1:1.0)
    stop = length(data) - winWidth
    count = div(stop, step)
    mat = Array{Float64, 2}(undef, count, length(quants))
    start = 1
    for i in 1:count
        dv = @view data[i:i+winWidth]
        q = quantile(dv, quants)
        mat[i,:] = q'
        start += width
    end
    return mat
end
res = rollQuantile(rand(len), 100, 10)




using BenchmarkTools
a = (1, 2, 3)
a_large = ([rand(1:5) for i in 1:50]...,)
f_splat(a)   = [a...];
f_collect(a) = collect(a);
f_comp(a)    = [i for i in a];
@btime f_splat($a);
@btime f_collect($a);
@btime f_comp($a);
@btime f_splat($a_large);
@btime f_collect($a_large);
@btime f_comp($a_large);


b = (1, 2, 3)
b_large = ([rand(1:5) for i in 1:50]...,)
c = [1,2,3]
c_large = rand(1:5, 50)
d = [4,5,6]
(concatV(a::AVec{T}, b::AVec{T})::Vector{T}) where T = vcat(a, b)
(concat1(a::Coll{T}, b::Coll{T})::Vector{T}) where T = collect(Iterators.flatten((a, b)))
(concat2(a::Coll{T}, b::Coll{T})::Vector{T}) where T = vcat(collect(a), collect(b))
(concat3(a::Coll{T}, b::Coll{T})::Vector{T}) where T = vcat([x for x in a], [x for x in b])
(concat4(a::Coll{T}, b::Coll{T})::Vector{T}) where T = [x for x in Iterators.flatten((a, b))]
@btime concatV($c, $d);
@btime concat1($a, $b);
@btime concat2($a, $b);
@btime concat3($a, $b);
@btime concat4($a, $b);
