module CalendarsTest
using Test, Dates
using Calendars

function runTests()
    @testset "calcDurToExpr" begin
        dateExpr = Date("2022-06-01")
        time1 = Time(02,00)
        time2 = Time(09,28)
        time3 = Time(12,32)
        time4 = Time(12,00)
        time5 = Time(23,50)
        closed::Second
        pre::Second
        open::Second
        post::Second

        @test calcDurToExpr(DateTime(dateExpr, time1), dateExpr) == MarketDur(Hour(16), Hour(4), Hour(6), Hour(4))
        println()
    end
end

end