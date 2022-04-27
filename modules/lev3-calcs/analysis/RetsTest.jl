module RetsTest
using Test, Dates
using SH, BaseTypes, SmallTypes, Bins, LegTypes, OptionTypes, OptionMetaTypes, RetTypes
using TestUtil, Rets
const t = Rets

function runTests()
    @testset "RetAtExp" begin
        # NOTE: Extreme vals are not just in a line because Bins.leftVal/rightVal which averages further out
        # Long Call
        testAt(lineRet(Style.call, Side.long, NETO_NEG), [NETO_NEG, NETO_NEG, NETO_NEG, NETO_NEG + SPLIT*10*binWidth(), 28.9025])
        # Long Put
        testAt(lineRet(Style.put, Side.long, NETO_NEG), reverse([NETO_NEG, NETO_NEG, NETO_NEG, NETO_NEG + SPLIT*10*binWidth(), 28.9025]))
        # Short Call
        testAt(lineRet(Style.call, Side.short, NETO_POS), [NETO_POS, NETO_POS, NETO_POS, NETO_POS - SPLIT*10*binWidth(), -30.6625])
        # Short Put
        testAt(lineRet(Style.put, Side.short, NETO_POS), reverse([NETO_POS, NETO_POS, NETO_POS, NETO_POS - SPLIT*10*binWidth(), -30.6625]))
    end

    @testset "RetAfterExp" begin
        # TODO: verify the following
        # Exp2 Long Call
        testAt(curveRet(Style.call, Side.long, NETO_NEG), [-4.959999999999993, -3.627265976507423, -3.1169047522659445, -2.4977928426642597, 28.902500000000522])
            # [NETO_NEG, NETO_NEG, NETO_NEG, NETO_NEG + SPLIT*10*binWidth(), 28.9025])
        # Exp2 Long Put
        testAt(curveRet(Style.put, Side.long, NETO_NEG), [28.902499999999954, -2.5022659765073945, -3.1169047522659445, -3.622792842664203, -4.959999999999483])
            # reverse([NETO_NEG, NETO_NEG, NETO_NEG, NETO_NEG + SPLIT*10*binWidth(), 28.9025]))
        # Exp2 Short Call
        testAt(curveRet(Style.call, Side.short, NETO_POS), [3.199999999999993, 1.867265976507423, 1.3569047522659448, 0.7377928426642599, -30.662500000000524])
            # [NETO_POS, NETO_POS, NETO_POS, NETO_POS - SPLIT*10*binWidth(), -30.6625])
        # Exp2 Short Put
        testAt(curveRet(Style.put, Side.short, NETO_POS), [-30.662499999999955, 0.7422659765073947, 1.3569047522659448, 1.862792842664203, 3.199999999999483])
            # reverse([NETO_POS, NETO_POS, NETO_POS, NETO_POS - SPLIT*10*binWidth(), -30.6625]))
    end

    @testset "combine!" begin
        buf = Vector{Float64}(undef, 10)
        rets = map(v -> Ret(1.0, collect(v)), (10.0:19.0, 20.0:29.0, 30.0:39.0, 40.0:49.0))
        extra = combineRets(map(v -> Ret(1.0, collect(v)), [100.0:109.0, 200.0:209.0]))
        expected = reduce((v1, v2) -> v1 + v2, map(x->x.vals, vcat(collect(rets), extra)))
        combineRetVals!(buf, rets, extra.vals)
        @test buf == expected
    end

    @testset "combine" begin
        rets = map(v -> Ret(1.0, collect(v)), (10.0:19.0, 20.0:29.0, 30.0:39.0, 40.0:49.0))
        expected = reduce((v1, v2) -> v1 + v2, map(x->x.vals, vcat(collect(rets))))
        res = combineRetVals(rets)
        @test res == expected
    end
end

const MET = OptionMeta(0.17336784644757075)
const NETO_POS = 3.2
const NETO_NEG = -4.96
const EXP1 = today() + Day(1)
const EXP2 = today() + Day(3)
const SPLIT = 450.0

const MID = div(numVals(), 2)+1
const TEST_XS = [1, MID-10, MID, MID+10, numVals()]

lineRet(style::Style.T, side::Side.T, neto) =
    makeRet(Leg(;option=Option(style, EXP1, SPLIT), side), MET, C(neto), EXP1, C(SPLIT), .8)

curveRet(style::Style.T, side::Side.T, neto) =
    makeRet(Leg(;option=Option(style, EXP2, SPLIT), side), MET, C(neto), EXP1, C(SPLIT), .8)

at(r::Ret, x::Int)::Float64 = getVals(r)[x]
testAt(r::Ret, expected) = @test at.(Ref(r), TEST_XS) ≅ expected

end