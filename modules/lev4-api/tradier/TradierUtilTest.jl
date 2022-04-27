module TradierUtilTest
using Test
using TradierUtil

function runTests()
    @testset "tier.parseToMs" begin
        @test tier.parseToMs("2022-03-24T14:44:10.086Z") === 1648133050086
    end
end

end