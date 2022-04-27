module QuoteTypesTest
using Test
using BaseTypes, SH, SmallTypes, QuoteTypes

function runTests()
    @testset "QuoteTypes" begin
        @test string(Quote(Action.open, C(2.9314))) === "Qo(2.931)"
        @test string(Quote(Action.close, C(-3))) === "Qc(-3.000)"
        @test getBid(Quote(C(2), C(4.2))) == 2.0 && getAsk(Quote(C(2), C(4.2))) == 4.2
    end
end

end