module CollUtilTest
using Test, TestUtil
using CollUtil

function runTests()
    @testset "prinsert" begin
        n = 100
        start = 11.1
        v = fill(start, n)
        prinsert!(v, 12.4)
        @test v[end] == 12.4
        for _ in 1:100
            prinsert!(v, rand())
        end
        @test v[end] == 12.4
        @test v[1] == start
    end
end

end