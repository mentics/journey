module TradierUtilTest
using Test
using TradierUtil

function runTests()
    @testset "tier.parseTs" begin
        @test Int(datetime2unix(tier.parseTs("2022-03-24T14:44:10.086Z")) * 1000) === 1648133050086
    end
end

end