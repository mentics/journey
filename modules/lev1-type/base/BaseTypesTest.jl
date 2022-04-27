module BaseTypesTest
using Test
using BaseTypes

function runTests()
    @testset "BaseTypes" begin
        @test string(Currency) === "Currency"
        @test string(C(123)) === "123.000"
        @test string(C(123.234123)) === "123.234"
        @test string(C(123.234523)) === "123.235"
        # @test randomC() isa Currency && -10.0 < randomC() < 10.0
    end
end

end