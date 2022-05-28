module MarketDurUtilTest
using Test, Dates
using MarketDurUtil, DateUtil

function runTests()
    markTime = MarketTime(true, Time(7) => Time(9, 24), Time(9, 30) => Time(16), Time(16) => Time(19, 55))
    markDur = MarketDur(markTime)
    durInClose1 = Second(9432)
    timeInClose1 = Time(Nanosecond(durInClose1))
    durInPre = Minute(61)
    timeInPre = ttFrom(markTime.pres) + durInPre
    durInOpen = Second(607)
    timeInOpen = ttFrom(markTime.opens) + durInOpen
    durInPost = Millisecond(57*60*1000 + 23)
    timeInPost = ttFrom(markTime.posts) + durInPost
    durInClose3 = Hour(1) + Minute(3) + Nanosecond(3223)
    timeInClose3 = ttTo(markTime.posts) + durInClose3

    durClosed1 = Time(7) - ZERO_TIME
    durClosed2 = ttFrom(markTime.opens) - ttTo(markTime.pres)
    durClosed12 = durClosed1 + durClosed2
    durClosed3 = Day(1) - (ttTo(markTime.posts) - ZERO_TIME)

    @testset "MarketDur" begin
        @test markDur.pre == Time(9,24) - Time(7)
        @test markDur.open == Time(16) - Time(9,30)
        @test markDur.post == Time(19,55) - Time(16)
        @test markDur.closed == Day(1) - (markDur.pre + markDur.open + markDur.post)
    end

    @testset "calcDurForDay" begin
        @test calcDurForDay(ZERO_TIME, markTime) == markDur
        @test calcDurForDay(timeInClose1, markTime) == MarketDur(markDur; closed = markDur.closed - durInClose1)
        @test calcDurForDay(timeInPre, markTime) == MarketDur(markDur; closed = markDur.closed - durClosed1, pre = markDur.pre - durInPre)
        @test calcDurForDay(timeInOpen, markTime) == MarketDur(markDur; closed = markDur.closed - durClosed12, pre = ZERO_SECOND, open = markDur.open - durInOpen)
        @test calcDurForDay(timeInPost, markTime) == MarketDur(durClosed3, ZERO_SECOND, ZERO_SECOND, markDur.post - MarketDurUtil.roundur(durInPost))
        @test calcDurForDay(timeInClose3, markTime) == MarketDur(MarketDurUtil.roundur(durClosed3 - durInClose3), ZERO_SECOND, ZERO_SECOND, ZERO_SECOND)
    end

    @testset "calcDurToClose" begin
        @test calcDurToClose(ZERO_TIME, markTime) == MarketDur(durClosed12, markDur.pre, markDur.open, ZERO_SECOND)
        @test calcDurToClose(timeInClose1, markTime) == MarketDur(durClosed12 - durInClose1, markDur.pre, markDur.open, ZERO_SECOND)
        @test calcDurToClose(timeInPre, markTime) == MarketDur(durClosed2, markDur.pre - durInPre, markDur.open, ZERO_SECOND)
        @test calcDurToClose(timeInOpen, markTime) == MarketDur(ZERO_SECOND, ZERO_SECOND, markDur.open - durInOpen, ZERO_SECOND)
        @test calcDurToClose(timeInPost, markTime) == ZERO_DUR
        @test calcDurToClose(timeInClose3, markTime) == ZERO_DUR
    end
end

end