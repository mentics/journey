module PayloadUtilTest
using Test, Dates
using SH, PayloadUtil
using Shorthand

function runTests()
    @testset "mergeLegs" begin
        legs = shLegs("s447c1@1 / s447c1@1", [today()+Day(3)])
        @test length(legs) == 2
        @test getQuantity(legs[1]) == 1
        @test getQuantity(legs[2]) == 1
        mergeLegs!(legs)
        @test length(legs) == 1
        @test getQuantity(legs[1]) == 2
    end
    return
end

end