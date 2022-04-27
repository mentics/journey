function test1()
    @btime reduce(+, Iterators.filter(g2, Iterators.map(add2, Iterators.flatten((a, b)))))
    return nothing
end

function test2()
    @btime reduce(+, filter(g2, map(add2, vcat(a, b))))
    return nothing
end


a = 1:10000
b = 2:20000

function test3()
    @btime Iterators.filter(g2, Iterators.map(add2, Iterators.flatten((a, b))))
    return nothing
end

function test4()
    @btime filter(g2, map(add2, vcat(a, b)))
    return nothing
end

test3()

test4()
