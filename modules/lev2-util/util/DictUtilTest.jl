module DictUtilTest
using Test
using DictUtil

function runTests()
    @testset "test" begin
        local d = Dict(:a => Dict(:b => Dict(:c => Dict(:d => "before"))))
        @test d[:a][:b][:c][:d] == "before"
        local f(d, k) = k == :d ? (d[k] = "foundit" ; return false) : return true
        walkKeys(f, d)
        @test d[:a][:b][:c][:d] == "foundit"
    end
end

end