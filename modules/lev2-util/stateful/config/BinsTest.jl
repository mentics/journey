module BinsTest
using Test, TestUtil
using Bins

function runTests()
    @testset "other" begin
        @test (binX(numVals()) - binX(1)) / binWidth() ≅ 602
    end

    @testset "BinsLeftRightOf" begin
        @test binLeftOf(binX(1) - 0.000001) === 1
        @test binLeftOf(binX(1)) === 1
        @test binLeftOf(binX(1) + 0.000001) === 1
        @test binLeftOf(binMin() - 0.000001) === 1
        @test binLeftOf(binMin()) === 2
        for i in numBins()
            @test binLeftOf(binX(i) + rand() * binWidth()) === i
        end
        @test binLeftOf(binMax() - 0.000001) === numVals() - 2
        @test binLeftOf(binMax()) === numVals() - 1
        @test binLeftOf(binMax() + 0.000001) === numVals() - 1
        @test binLeftOf(binsRight() - 0.000001) === numVals() - 1
        @test binLeftOf(binsRight()) === numVals() - 1
        @test binLeftOf(binsRight() + 0.000001) === numVals() - 1
        @test binLeftOf(binX(numVals()) - 0.000001) === numVals() - 1
        @test binLeftOf(binX(numVals())) === numVals()
        @test binLeftOf(binX(numVals()) + 0.000001) === numVals()

        # x = binX(1) - 0.000001
        # @info "check" (numVals() - floor(Int, (binX(numVals()) - x) / binWidth())) (floor(Int, (binX(numVals()) - x) / binWidth())) ((binX(numVals()) - x) / binWidth())
        @test binRightOf(binX(1) - 0.000001) === 1
        @test binRightOf(binX(1)) === 1
        @test binRightOf(binX(1) + 0.000001) === 2
        @test binRightOf(binMin() - 0.000001) === 2
        @test binRightOf(binMin()) === 2
        for i in numBins()
            @test binRightOf(binX(i) + rand() * binWidth()) === i + 1
        end
        @test binRightOf(binMax() - 0.000001) === numVals() - 1
        @test binRightOf(binMax()) === numVals() - 1
        @test binRightOf(binMax() + 0.000001) === numVals()
        @test binRightOf(binsRight() - 0.000001) === numVals()
        @test binRightOf(binsRight()) === numVals()
        @test binRightOf(binsRight() + 0.000001) === numVals()
        @test binRightOf(binX(numVals()) - 0.000001) === numVals()
        @test binRightOf(binX(numVals())) === numVals()
        @test binRightOf(binX(numVals()) + 0.000001) === numVals()
    end
end

end