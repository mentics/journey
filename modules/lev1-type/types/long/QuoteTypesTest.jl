module QuoteTypesTest
using Test
using BaseTypes, SH, SmallTypes, QuoteTypes

function runTests()
    @testset "QuoteTypes" begin
        @test string(Quote(Action.open, C(2.9314))) === "Qo(2.931)"
        @test string(Quote(Action.close, C(-3))) === "Qc(-3.000)"
        @test getBid(Quote(C(2), C(4.2))) == 2.0 && getAsk(Quote(C(2), C(4.2))) == 4.2
    end

    @testset "improve" begin
        @test improve(Quote(C(2.0), C(3.0)), .2) === C(2.2)
        @test improve(Quote(C(2.0), C(3.0)), 1.0) === C(3.0)
        @test improve(Quote(C(2.0), C(4.0)), .2) === C(2.4)
        @test improve(Quote(C(2.0), C(5.0)), .2) === C(2.4)
        @test improve(Quote(C(3.0), C(55.0)), .3) === C(3.9)
        @test improve(Quote(C(3.0), C(55.0)), 1.0) === C(6.0)
        @test improve(Quote(C(-3.0), C(-2.2)), .3) === C(-2.76)
        @test improve(Quote(C(-6.0), C(-3.0)), .3) === C(-5.1)
        @test improve(Quote(C(-40.0), C(-20.0)), .5) === C(-30.0)
        @test improve(Quote(C(-40.0), C(-3.0)), .5) === C(-30.0)
        @test improve(Quote(C(-5.0), C(55.0)), .2) === C(-4.5)
        @test improve(Quote(C(-5.0), C(55.0)), 1.0) === C(-2.5)
        @test improve(Quote(C(-5.0), C(0.0)), 0.5) === C(-3.75)
    end
end

end