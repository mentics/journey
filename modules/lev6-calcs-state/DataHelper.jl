module DataHelper
using Dates
using Globals, DateUtil
using Calendars
# using Sched

# TODO: where in dirs/levels?

export whenMarket, tooOld

tooOld(period::Period)::Period = isMarketOpen() ? 2*period : Hour(10) # convert(Millisecond, nextMarketChange() - now(UTC) + (period ÷ 2))

function whenMarket(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime, period::Period)
    # untilChange = nextMktChange - from
    if isMktOpen
        from + period
    else
        nextMktChange + (period ÷ 2)
    end
end

# getData(::Type{T}, sym::Symbol)::T where T
# schedUpdater(mod::Module, code::String, sym::Symbol, period::Period) = Sched.add("update-$(sym)", mod, code, period, true)

# adjustPeriod(period::Int) = (intest() || dev() || !isNowWithinMarket()) ? LONG_PERIOD : period

# const LONG_PERIOD = 4*60*60*1000

end