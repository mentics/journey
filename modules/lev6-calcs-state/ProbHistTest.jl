module ProbHistTest
using Test, TestUtil
using ProbHist

function runTests()
    @testset "probhist" begin
        testStart() ; try

        p1 = probHist(100.0, 1)
        @test p1.center === 100.0
        # TODO: more stuff

        finally testStop() end
    end
end

end