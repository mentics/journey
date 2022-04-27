module TradierOrderConvertTest
using Test, TestUtil, TradierOrderConvert, TradierTestData
using SH, BaseTypes, SmallTypes, LegTypes, LegMetaTypes, OrderTypes

function runTests()
    @testset "TradierOrderConvert" begin
        testStart() ; try

        tord = mockOrder(1, Action.open, 17, 100009999, "filled", [LegMeta(;leg=Leg(;side=Side.short))], 1.1)["order"]
        ord = to(Order, tord)
        @test getId(ord) === 17
        @test getPrimitDir(ord) == C(1.1)

        tord = mockOrder(1, Action.open, 19, 100009999, "filled", [LegMeta()], -2.2)["order"]
        ord = to(Order, tord)
        @test getId(ord) === 19
        @test getPrimitDir(ord) == C(-2.2)

        finally testStop() end
    end
end

end