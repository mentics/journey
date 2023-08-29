module PerfTesting

module Perf1
    function test1(x, extra=nothing)
        a = 2*x
        b = if isnothing(extra)
            a
        else
            a + extra
        end
        c = sqrt(b)
        return c
    end

    function test2(x; extra=nothing)
        a = 2*x
        b = if isnothing(extra)
            a
        else
            a + extra
        end
        c = sqrt(b)
        return c
    end
end

module Perf2
    div2(x) = x / 2
    times2(x) = 2*x
    times3(x) = 3*x

    function test1(v)
        return map(div2, v), map(times2, v), map(times3, v)
    end

    function test2(v)
        return div2.(v), times2.(v), times3.(v)
    end

    function test3(v)
        len = length(v)
        v1 = Vector{Float64}(undef, len)
        v2 = Vector{Int}(undef, len)
        v3 = Vector{Int}(undef, len)
        @inbounds for i in eachindex(v)
            x = v[i]
            v1[i] = div2(x)
            v2[i] = times2(x)
            v3[i] = times3(x)
        end
        return v1, v2, v3
    end

    using StructArrays
    function test4(v)
        sa = StructArrays.collect_structarray((div2(x), times2(x), times3(x)) for x in v)
        return sa
    end

    function test5(v)
        sa = StructArrays.collect_structarray((div2(x), times2(x), times3(x)) for x in v)
        return sa.:1, sa.:2, sa.:3
    end

    function maparray(f, v)
        sa = StructArrays.collect_structarray(f(x) for x in v)
        return StructArrays.components(sa)
    end

    function test6(v)
        return maparray(v) do x
            (div2(x), times2(x), times3(x))
        end
    end

    using BenchmarkTools
    function time3(n=100000)
        v = fill(17, n)
        @btime test1($v)
        @btime test2($v)
        @btime test3($v)
        @btime test4($v)
        @btime test5($v)
        @btime test6($v)
        return
    end
end

end