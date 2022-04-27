module SchedTest
using Test, Dates
using Sched

function runTests()
    @testset "SchedInitialOrder" begin
        empty!(Checks)
        Sched.clearJobs()
        Sched.stop()
        @test !Sched.ison()
        Sched.add("Test1", @__MODULE__, "callThis(:Test1)", "simpleDelay(Day(1))", true)
        Sched.add("Test2", @__MODULE__, "callThis(:Test2)", "simpleDelay(Day(1))", true)
        Sched.add("Test3", @__MODULE__, "callThis(:Test3)", "simpleDelay(Day(1))", true)
        @test map(j -> j.job.name, Sched.Jobs) == ["Test1", "Test2", "Test3"]
        Sched.start()
        sleep(0.1)
        Sched.stop()
        @test Checks[:Test1] <= Checks[:Test2] <= Checks[:Test3]
    end

    @testset "ScheduledOrder" begin
        # println("Start: ", now(UTC))
        empty!(Checks)
        Sched.clearJobs()
        Sched.stop()
        @test !Sched.ison()
        Sched.add("Test1", @__MODULE__, "callThis(:Test1)", "simpleDelay(Millisecond(1))", false)
        Sched.add("Test2", @__MODULE__, "callThis(:Test2)", "simpleDelay(Millisecond(11))", false)
        Sched.add("Test3", @__MODULE__, "callThis(:Test3)", "simpleDelay(Millisecond(22))", false)
        @test map(j -> j.job.name, Sched.Jobs) == ["Test1", "Test2", "Test3"]
        # foreach(println, j.timeNext for j in Sched.Jobs)
        Sched.start()
        sleep(0.1)
        Sched.stop()
        @test Checks[:Test1] <= Checks[:Test2] <= Checks[:Test3]
    end
end

const Checks = Dict{Symbol,Any}()

callThis(sym::Symbol) = () -> ( Checks[sym] = now(UTC) ; Sched.remove(string(sym)) )
simpleDelay(p::Period) = (from,_,_) -> from + p

end