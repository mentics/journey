module DataHelper
using Dates
using Globals, DateUtil
using Calendars

# TODO: where in dirs/levels?

export whenMarket, tooOld

tooOld(period::Period)::Period = isMarketOpen() ? 2*period : Hour(10) # convert(Millisecond, nextMarketChange() - now(UTC) + (period ÷ 2))

function whenMarket(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime, period::Period)
    # untilChange = nextMktChange - from
    if isMktOpen
        from + period
    else
        nextMktChange + randPeriod(period, Second(30))
    end
end

function randPeriod(period::Period, within::Period)::Period
    return Millisecond(mod(hash(period), Millisecond(within).value))
end

# schedUpdater(mod::Module, code::String, sym::Symbol, period::Period) = Sched.add("update-$(sym)", mod, code, period, true)

end