module CalendarsTest
using Test, Dates
using MarketDurTypes
using DateUtil, MarketDurUtil
using Calendars, Expirations

const cal = Calendars

function runTests()
    @testset "calcDurToExpr" begin
        dateTo = findWeekOpen(expirs())
        markTime = cal.marketTime(dateTo)

        dur1 = Hour(2)
        time1 = toTimeMarket(getMarketClose(dateTo)) - dur1
        ts1 = fromMarketTZ(dateTo, time1)
        ts0 = fromMarketTZ(dateTo, ZERO_TIME)

        expDurPart = MarketDur(; open=dur1)
        expDurForDay = calcDurForDay(time1, markTime)
        expDayToClose = calcDurToClose(ZERO_TIME, markTime)
        expDayFull = cal.marketDur(dateTo)
        expWeekend = DUR_CLOSED

        @test cal.marketDur(dateTo) == cal.marketDur(dateTo - Day(1))

        @test cal.calcDurToExpr(ts0, dateTo) == expDayToClose

        @test cal.calcDurToExpr(ts1, dateTo) == calcDurToClose(time1, markTime)
        @test cal.calcDurToExpr(ts1, dateTo) == expDurPart

        @test cal.calcDurToExpr(ts1 - Day(3), dateTo) == expDurForDay + expDayToClose + multDur(expDayFull, 2)
        @test cal.calcDurToExpr(ts1 - Day(8), dateTo) == expDurForDay + expDayToClose + multDur(expDayFull, 5) + multDur(expWeekend, 2)
    end
end

multDur(dur::MarketDur, k::Real)::MarketDur = MarketDur(k * dur.closed, k * dur.pre, k * dur.open, k * dur.post)

function findWeekOpen(exps::Vector{Date})::Date
    for exp in exps
        isopen = isnothing(findfirst(1:4) do i
            !marketTime(exp - Day(i)).isOpen
        end)
        isopen && return exp
    end
end

end