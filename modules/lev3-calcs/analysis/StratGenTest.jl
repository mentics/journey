module StratGenTest
using Test, TestUtil, Dates
using BaseTypes, Globals, SH, SmallTypes, ChainTypes, StratTypes
using StratGen, Snapshots, Positions, Chains
using Shorthand
const t = StratGen

function runTests()
    @testset "doLegsConflict" begin
        legs = shLegs("s447c1@1 / l447c1@1", [today()+Day(3)])
        @test t.doLegsConflict(legs[1], legs[2])
    end

    @testset "makeLeg" begin
        lr = t.makeLegRet(OptionQuote(), Side.long, today()+Day(1), C(450.0), .85)
        @test lr isa t.LegRet
    end

    @testset "allLegs" begin
        testStart(); try

        chs = chains()
        exps = sort!(collect(keys(chs)))[3:4]
        sp = cp = getStrike(chs[exps[1]].chain[div(end, 2)])
        vtyRatio = 0.85

        opts1 = t.forDates(chs, exps)
        @test collect(opts1) isa Vector{OptionQuote}

        opts2 = t.validOptions(cp, chs, exps)
        @test collect(opts2) isa Vector{OptionQuote}

        tups1 = t.itrOptSide(exps[1], opts2)
        @test collect(Tuple{OptionQuote,Side.T}, tups1) isa Vector{Tuple{OptionQuote,Side.T}}

        @test map(t.optSideToLeg(exps[1], sp, vtyRatio), tups1) isa Vector{t.LegRet}

        @test collect(t.itrLegRets(x->true, sp, exps[1], opts2)) isa Vector{t.LegRet}

        allSpreads2 = allSpreads(chs, isConflict, (sp, cp), exps)
        @test length(allSpreads2[1]) > 3200 / 2 # (20 below cp, 20 above cp)^2 * length(exps) - filtered (lots because no short cals)
        @test length(allSpreads2[2]) > 3200 / 2
        # @test issorted(allSpreads2[1]; lt=ltSpread)
        # @test issorted(allSpreads2[2]; lt=ltSpread)

        legs = collect(Iterators.flatten(Iterators.flatten(allSpreads2)))
        @test sort!(unique!([getExpiration(leg[1]) for leg in legs])) == exps

        @test unique(allSpreads2[1]) == allSpreads2[1]
        @test unique(allSpreads2[2]) == allSpreads2[2]

        alls = vcat(allSpreads2[1], allSpreads2[2])
        sc = 0 ; lc = 0 ; v = 0 ; ss = 0  ; ds = 0
        for spr in alls
            isVert(spr) && (v += 1)
            isCalLong(spr) && (lc += 1)
            isCalShort(spr) && (sc += 1)
            isSideSame(spr) && (ss += 1)
            isStyleDiff(spr) && (ds += 1)
            @test getStrike(spr[1]) < getStrike(spr[2]) || getExpiration(spr[1]) < getExpiration(spr[2])
        end
        @test v > 1000
        @test lc > 1000
        @test sc === 0
        @test ss === 0
        @test ds === 0

        # shortCal = findfirst(alls) do spr
        #     for leg in spr
        #         getExpiration(leg) > exps[1] && getSide(leg) == Side.short && return true
        #     end
        #     return false
        # end
        # @test isnothing(shortCal)

        # println(typeof(alls))
        # vert1 = findfirst((l1, l2) -> getExpiration(l1) == getExpiration(l2), alls)
        # @test !isnothing(vert1)

        # display([getStrike(getLeg(lr[1])) for lr in legRetTypes[4]])
        # snop()

        finally testStop() end
    end
end

# function ltSpread(spr1, spr2)
#     if getStrike(spr1[1]) == getStrike(spr2[1])
#         getExpiration(spr1[1]) == getExpiration(spr2[1]) && ((@error "Duplication in spreads" spr1[1][1] spr1[2][1] spr2[1][1] spr2[2][1]) ; error("stop"))
#         return getExpiration(spr1[1]) < getExpiration(spr2[1])
#     end
#     return getStrike(spr1[1]) < getStrike(spr2[1])
# end

end