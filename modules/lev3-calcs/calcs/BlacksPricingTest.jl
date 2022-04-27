module BlacksPricingTest
using Test, TestUtil, Dates
using BlacksPricing
using DateUtil, SmallTypes, OptionTypes

function runTests()
    @testset "test time to expire" begin
        expTo = today() + Day(3)
        expFrom = today() + Day(1)
        t1 = timeToExpir(expFrom, expTo)
        t2 = timeToExpir(marketClose(expFrom), expTo)
        t3 = timeToExpir(toms(marketClose(expFrom)), expTo)

        @test t1 == t2 == t3 == (2*24*60*60*1000) / (365*24*60*60*1000)
    end

    @testset "test pricing" begin
        timeToExpY = timeToExpir(today() + Day(1), today() + Day(3))

        p1 = priceOption(Style.call, 450.0, timeToExpY, .2, 450.0)
        @test p1 ≅ 2.65776865
        @test priceOption(Style.call, 450.0, timeToExpY, .3, 450.0) > p1
    end
end

end