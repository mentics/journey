module CalendarsTest
using Test, Dates
using Calendars

function runTests()
    @testset "getDursToExpr" begin
        dateExpr = Date("2022-06-01")
        time1 = Time(02,00)
        time2 = Time(09,28)
        time3 = Time(12,32)
        time4 = Time(12,00)
        time5 = Time(23,50)
        println(getDursToExpr(DateTime(dateExpr, time1), dateExpr))
    end
end

end