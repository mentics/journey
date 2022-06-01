module MarketDurUtilTest
using Test, Dates, Intervals
using MarketDurTypes, MarketDurUtil, DateUtil

function runTests()
    markTime = MarketTime(Time(7)..Time(9, 24), Time(9, 30)..Time(16), Time(16)..Time(19, 55))
    markDur = MarketDur(markTime)
    durInClose1 = Second(9432)
    timeInClose1 = Time(Nanosecond(durInClose1))
    durInPre = Minute(61)
    timeInPre = first(markTime.pres) + durInPre
    durInOpen = Second(607)
    timeInOpen = first(markTime.opens) + durInOpen
    durInPost = Millisecond(57*60*1000 + 23)
    timeInPost = first(markTime.posts) + durInPost
    durInClose3 = Hour(1) + Minute(3) + Nanosecond(3223)
    timeInClose3 = last(markTime.posts) + durInClose3

    durClosed1 = Time(7) - TIME_ZERO
    durClosed2 = first(markTime.opens) - last(markTime.pres)
    durClosed12 = durClosed1 + durClosed2
    durClosed3 = Day(1) - (last(markTime.posts) - TIME_ZERO)

    @testset "Weekend Holiday" begin
        wend = today() - Day(dayofweek(today()))
        dataWend = Dict{String,Any}("date" => string(wend), "status" => "closed")
        dataHoliday = Dict{String,Any}("date" => string(wend + Day(2)), "status" => "closed")
        mtWend = MarketTime(dataWend)
        @test mtWend.status == MarketDurTypes.MTStatus.weekend
        mtHoliday = MarketTime(dataHoliday)
        @test mtHoliday.status == MarketDurTypes.MTStatus.holiday
        @test MarketDur(mtWend) == DUR_WEND
        @test MarketDur(mtHoliday) == DUR_HOLIDAY
    end

    @testset "MarketDur" begin
        @test markDur.pre == Time(9,24) - Time(7)
        @test markDur.open == Time(16) - Time(9,30)
        @test markDur.post == Time(19,55) - Time(16)
        @test markDur.closed == Day(1) - (markDur.pre + markDur.open + markDur.post)
    end

    @testset "calcDurForDay" begin
        @test calcDurForDay(TIME_ZERO, markTime) == markDur
        @test calcDurForDay(timeInClose1, markTime) == MarketDur(markDur; closed = markDur.closed - durInClose1)
        @test calcDurForDay(timeInPre, markTime) == MarketDur(markDur; closed = markDur.closed - durClosed1, pre = markDur.pre - durInPre)
        @test calcDurForDay(timeInOpen, markTime) == MarketDur(markDur; closed = markDur.closed - durClosed12, pre = SECOND_ZERO, open = markDur.open - durInOpen)
        @test calcDurForDay(timeInPost, markTime) == MarketDur(durClosed3, SECOND_ZERO, SECOND_ZERO, markDur.post - MarketDurUtil.roundur(durInPost))
        @test calcDurForDay(timeInClose3, markTime) == MarketDur(MarketDurUtil.roundur(durClosed3 - durInClose3), SECOND_ZERO, SECOND_ZERO, SECOND_ZERO)
    end

    @testset "calcDurToClose" begin
        @test calcDurToClose(TIME_ZERO, markTime) == MarketDur(durClosed12, markDur.pre, markDur.open, SECOND_ZERO)
        @test calcDurToClose(timeInClose1, markTime) == MarketDur(durClosed12 - durInClose1, markDur.pre, markDur.open, SECOND_ZERO)
        @test calcDurToClose(timeInPre, markTime) == MarketDur(durClosed2, markDur.pre - durInPre, markDur.open, SECOND_ZERO)
        @test calcDurToClose(timeInOpen, markTime) == MarketDur(SECOND_ZERO, SECOND_ZERO, markDur.open - durInOpen, SECOND_ZERO)
        @test calcDurToClose(timeInPost, markTime) == DUR_ZERO
        @test calcDurToClose(timeInClose3, markTime) == DUR_ZERO
    end
end

end