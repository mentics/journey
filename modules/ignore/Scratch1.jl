module Scratch1

function model1(xs, p)
    # @info "model1" xs p
    return @. p[1] / (p[2] * (xs - p[3]))
end

function model2(xs, p)
    return @. p[1] / (p[2] * (xs - p[3])) + p[4] / (p[5] * (xs - p[6])) + p[7] / (p[8] * (xs - p[9])^2)
end

fit1(xys) = p -> sum((model2(xys[:,1], p) .- xys[:,2]).^2)

function test()
    data = [-100000. .0; -300. 1.; -100. 3.; -50 8.; .0 20.]
    # println("test1: ", model1(data, [1., 2., 3.]))
    # println("test2: ", fit1(data)([1., 2., 3.]))
    SearchRange = fill((-100., 100.), 9)
    res = bboptimize(fit1(data); SearchRange, MaxTime=2)
    bc = best_candidate(res)
    println("result: ", model1(data, bc))
    return (bc, res)
end

end