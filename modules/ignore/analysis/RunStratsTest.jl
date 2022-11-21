module RunStratsTest
using Test
using SH, StratTypes
using Strats, RunStrats, Snapshots, Chains, Positions

t = RunStrats

function runTests()
    # Covered in TestProcess for now
    # @testset "RunStratsSnapped" begin
        # isnothing(snap()) && snap(1)
        # chs = chains()
        # exps = sort!(collect(keys(chs)))[3:4]
        # sp = cp = getStrike(chs[exps[1]].chain[div(end, 2)])

        # allSpreads2 = allSpreads(chs, isConflict, (sp, cp), exps)
        # t.runStrats(allSpreads2::Spreads2, t.calcScore1, nothing, 1000, 12000)

        # TODO: run through all Strat results (hook in there to get every single one processed) and make sure they are all valid
    # end
end

end