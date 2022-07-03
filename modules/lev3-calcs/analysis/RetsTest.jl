module RetsTest
using Test, Dates
using SH, BaseTypes, SmallTypes, Bins, LegTypes, OptionTypes, OptionMetaTypes, RetTypes
using TestUtil, Rets
const t = Rets

function runTests()
    @testset "RetAtExp" begin
        # Long Call
        testAt(lineRet(Style.call, Side.long, NETO_NEG), [NETO_NEG, NETO_NEG, NETO_NEG, NETO_NEG + SPLIT*10*Bins.WIDTH, 40.152499999999975])
        # Long Put
        testAt(lineRet(Style.put, Side.long, NETO_NEG), reverse([NETO_NEG, NETO_NEG, NETO_NEG, NETO_NEG + SPLIT*10*Bins.WIDTH, 40.152499999999975]))
        # Short Call
        testAt(lineRet(Style.call, Side.short, NETO_POS), [NETO_POS, NETO_POS, NETO_POS, NETO_POS - SPLIT*10*Bins.WIDTH, -41.912499999999966])
        # Short Put
        testAt(lineRet(Style.put, Side.short, NETO_POS), reverse([NETO_POS, NETO_POS, NETO_POS, NETO_POS - SPLIT*10*Bins.WIDTH, -41.912499999999966]))
    end

    @testset "RetAfterExp" begin
        # TODO: verify the following
        # Exp2 Long Call
        testAt(curveRet(Style.call, Side.long, NETO_NEG), [NETO_NEG, -4.031640904959338, -3.1169047522659445, -1.7734559965167795, 40.1525])
            # [NETO_NEG, NETO_NEG, NETO_NEG, NETO_NEG + SPLIT*10*Bins.WIDTH, 28.9025])
        # Exp2 Long Put
        testAt(curveRet(Style.put, Side.long, NETO_NEG), [40.1525, -1.781640904959338, -3.1169047522659445, -4.023455996516666, NETO_NEG])
            # reverse([NETO_NEG, NETO_NEG, NETO_NEG, NETO_NEG + SPLIT*10*Bins.WIDTH, 28.9025]))
        # Exp2 Short Call
        testAt(curveRet(Style.call, Side.short, NETO_POS), [NETO_POS, 2.2716409049593382, 1.3569047522659448, 0.013455996516779756, -41.9125])
            # [NETO_POS, NETO_POS, NETO_POS, NETO_POS - SPLIT*10*Bins.WIDTH, -30.6625])
        # Exp2 Short Put
        testAt(curveRet(Style.put, Side.short, NETO_POS), [-41.9125, 0.021640904959338236, 1.3569047522659448, 2.263455996516666, NETO_POS])
            # reverse([NETO_POS, NETO_POS, NETO_POS, NETO_POS - SPLIT*10*Bins.WIDTH, -30.6625]))
    end

    @testset "combine!" begin
        buf = Vector{Float64}(undef, 10)
        rets = map(v -> Ret(collect(v), 1.0, 4), (10.0:19.0, 20.0:29.0, 30.0:39.0, 40.0:49.0))
        extra = combineRets(map(v -> Ret(collect(v), 1.0, 4), [100.0:109.0, 200.0:209.0]))
        expected = reduce((v1, v2) -> v1 + v2, map(x->x.vals, vcat(collect(rets), extra)))
        combineRetVals!(buf, rets, extra.vals)
        @test buf == expected
    end

    @testset "combine" begin
        rets = map(v -> Ret(collect(v), 1.0, 4), (10.0:19.0, 20.0:29.0, 30.0:39.0, 40.0:49.0))
        expected = reduce((v1, v2) -> v1 + v2, map(x->x.vals, vcat(collect(rets))))
        res = combineRetVals(rets)
        @test res == expected
    end

    @testset "combineRetsC" begin
        ret1 = Ret(Bins.with(1.0), 99.0, 4)
        spike = Bins.with(0.0)
        spike[Bins.center()] = 2.0
        ret2 = Ret(spike, 102.1, 2)
        ret3 = Ret(Bins.with(x -> 10*(x - 1.0)), 105.8, 4)
        res = combineRetsC((ret1, ret2, ret3))
        Main.save[:rets] = [ret1, ret2, ret3, res]
        @test res.numLegs == 10
        @test res.center == 102.3
        @test res.vals[Bins.center()] ≅ 1.0 + 0.0 - 0.3308128
        # @test res == Ret([], , 10)
    end
end

const MET = OptionMeta(0.17336784644757075)
const NETO_POS = 3.2
const NETO_NEG = -4.96
const EXP1 = today() + Day(1)
const EXP2 = today() + Day(3)
const SPLIT = 450.0

const MID = div(Bins.VNUM, 2)+1
const TEST_XS = [1, MID-10, MID, MID+10, Bins.VNUM]

lineRet(style::Style.T, side::Side.T, neto) =
    makeRet(Leg(;option=Option(style, EXP1, SPLIT), side), MET, C(neto), EXP1, C(SPLIT), .8)

curveRet(style::Style.T, side::Side.T, neto) =
    makeRet(Leg(;option=Option(style, EXP2, SPLIT), side), MET, C(neto), EXP1, C(SPLIT), .8)

at(r::Ret, x::Int)::Float64 = getVals(r)[x]
testAt(r::Ret, expected) = @test at.(Ref(r), TEST_XS) ≅ expected

end