module DateUtilTest
using Test, Dates, TimeZones
using DateUtil

function runTests()
    @testset "new date tests" begin
        # @test toDateLocal(Int(datetime2unix(now(UTC))*1000)) == today()
        @test toDateLocal(now(UTC)) == today()
    end

    @testset "nextMarketPeriod" begin
        t1 = round(now(UTC), Hour(1))
        @test DateUtil.nextMarketPeriod(t1, true, t1, Minute(4), Second(0), Second(5)) == t1 + Second(5)
        @test DateUtil.nextMarketPeriod(t1, true, t1 + Second(2), Minute(4), Second(0), Second(5)) == t1 + Second(7)
        @test DateUtil.nextMarketPeriod(t1, true, t1 + Minute(4), Minute(4), Second(0), Second(5)) == t1 + Minute(4) + Second(5)
        @test DateUtil.nextMarketPeriod(t1-Second(2), true, t1, Hour(1), Second(73), Second(5)) == t1 + Second(5)
        @test DateUtil.nextMarketPeriod(t1-Second(2), true, t1, Minute(4), Second(0), Second(3)) == t1 + Second(3)
    end

    # @testset "OldTestDates" begin
    #     futureDate = today() + Day(2)
    #     # @info "test" futureDate Int(datetime2unix(DateTime(futureDate)))
    #     # println((msMarketClose(futureDate) - Int(TimeZones.zdt2unix(marketClose(futureDate))*1000))/1000/60/60)
    #     @test msMarketClose(futureDate) == Int(TimeZones.zdt2unix(marketClose(futureDate))*1000)
    #     @test tims(msMarketClose(today())) == marketClose(today())
    #     @test msMarketClose(today()) === toms(marketClose(today()))
    # end
end

end