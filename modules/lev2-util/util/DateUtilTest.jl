module DateUtilTest
using Test, Dates, TimeZones
using DateUtil

function runTests()
    @testset "new date tests" begin
        @test toDateLocal(Int(datetime2unix(now(UTC))*1000)) == today()
    end

    @testset "OldTestDates" begin
        futureDate = today() + Day(2)
        # @info "test" futureDate Int(datetime2unix(DateTime(futureDate)))
        # println((msMarketClose(futureDate) - Int(TimeZones.zdt2unix(marketClose(futureDate))*1000))/1000/60/60)
        @test msMarketClose(futureDate) == Int(TimeZones.zdt2unix(marketClose(futureDate))*1000)
        @test tims(msMarketClose(today())) == marketClose(today())
        @test msMarketClose(today()) === toms(marketClose(today()))
    end
end

end