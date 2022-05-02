module BinsTest
using Test, TestUtil
using Bins

function runTests()
    @testset "BinsBasic" begin
        xs = Bins.XS
        @test Bins.XLEFT == xs[1]
        @test Bins.MFIRST == xs[2]
        @test Bins.MLAST == xs[end-1]
        @test Bins.XRIGHT == xs[end]
        @test (Bins.MLAST - Bins.MFIRST) ≈ Bins.SPAN
        @test (Bins.XRIGHT - Bins.XLEFT) / Bins.WIDTH ≈ Bins.NUM
        @test Bins.WIDTH ≈ Bins.SPAN / (Bins.NUM-1)
    end

    @testset "BinsLeftRightOf" begin
        @test Bins.leftOf(Bins.XLEFT - 0.000001) === 1
        @test Bins.leftOf(Bins.XLEFT) === 1
        @test Bins.leftOf(Bins.XLEFT + 0.000001) === 1
        @test Bins.leftOf(Bins.MFIRST - 0.000001) === 1
        @test Bins.leftOf(Bins.MFIRST) === 2
        for (i, x) in Bins.midsi()[1:end-1]
            @test Bins.leftOf(x + rand() * Bins.WIDTH) === i
        end
        @test Bins.leftOf(Bins.MLAST - 0.000001) === Bins.VNUM - 2
        @test Bins.leftOf(Bins.MLAST) === Bins.VNUM - 1
        @test Bins.leftOf(Bins.MLAST + 0.000001) === Bins.VNUM - 1
        # @test Bins.leftOf(Bins.XRIGHT - 0.000001) === Bins.VNUM - 1
        # @test Bins.leftOf(Bins.XRIGHT) === Bins.VNUM - 1
        # @test Bins.leftOf(Bins.XRIGHT + 0.000001) === Bins.VNUM - 1
        @test Bins.leftOf(Bins.XRIGHT - 0.000001) === Bins.VNUM - 1
        @test Bins.leftOf(Bins.XRIGHT) === Bins.VNUM
        @test Bins.leftOf(Bins.XRIGHT + 0.000001) === Bins.VNUM

        # x = Bins.XLEFT - 0.000001
        # @info "check" (Bins.VNUM - floor(Int, (Bins.XRIGHT - x) / binWidth())) (floor(Int, (Bins.XRIGHT - x) / binWidth())) ((Bins.XRIGHT - x) / binWidth())
        @test Bins.rightOf(Bins.XLEFT - 0.000001) === 1
        @test Bins.rightOf(Bins.XLEFT) === 1
        @test Bins.rightOf(Bins.XLEFT + 0.000001) === 2
        @test Bins.rightOf(Bins.MFIRST - 0.000001) === 2
        @test Bins.rightOf(Bins.MFIRST) === 2
        for (i, x) in Bins.midsi()
            @test Bins.rightOf(x + rand() * Bins.WIDTH) === i + 1
        end
        @test Bins.rightOf(Bins.MLAST - 0.000001) === Bins.VNUM - 1
        @test Bins.rightOf(Bins.MLAST) === Bins.VNUM - 1
        @test Bins.rightOf(Bins.MLAST + 0.000001) === Bins.VNUM
        @test Bins.rightOf(Bins.XRIGHT - 0.000001) === Bins.VNUM
        @test Bins.rightOf(Bins.XRIGHT) === Bins.VNUM
        @test Bins.rightOf(Bins.XRIGHT + 0.000001) === Bins.VNUM
        @test Bins.rightOf(Bins.XRIGHT - 0.000001) === Bins.VNUM
        @test Bins.rightOf(Bins.XRIGHT) === Bins.VNUM
        @test Bins.rightOf(Bins.XRIGHT + 0.000001) === Bins.VNUM
    end
end

end