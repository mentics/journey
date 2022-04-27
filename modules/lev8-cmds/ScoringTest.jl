module ScoringTest
using Test, TestUtil
using Scoring

function runTests()
    @testset "Scoring" begin
        ideal = [0.0, 1.0, 3.0, 1.0, 0.0]
        pv = [0.1, 0.2, 0.3, 0.2, 0.1]
        @test compareVals(pv, ideal, ideal) ≅ 0.0
        @test compareVals(pv, ideal, [-1.0, 1.0, 0.1, -0.5, -1.0]) ≅ 3.173
    end
end

end