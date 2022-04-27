module CalcUtilTest
using Test, TestUtil
using CalcUtil

function runTests()
    @testset "calcEvPnl" begin
        m = calcMetrics([.0, .1, .5, .2, .0], [-.1, -.1, .3, -.2, .4])
        @test m.profit ≅ .1425
        @test m.loss ≅ -.01 + -.04
    end

    @testset "normalize" begin
        v1 = [1., 2., 3., 4.]
        normalize!(v1)
        @test v1 ≅ [.1, .2, .3, .4]
    end

    @testset "smooth!" begin
        v = randn(100)
        normalize!(v)
        mid1 = CalcUtil.findMidBin(v)
        @test sum(v) ≅ 1.0
        smooth!(v)
        @test sum(v) ≅ 1.0
        mid2 = CalcUtil.findMidBin(v)
        @test mid1 === mid2
    end
end

end