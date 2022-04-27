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
