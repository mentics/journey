module OptionPricing
using DateUtil
using Calendars

# TODO: consider moving ts to next market open time because pricing during market closed might not be meaningful
function timeToExpr(ts::DateTime, expr::Date)::Second
    dateBegin = toDateMarket(ts)
    if dateBegin == expr
        getDursToClose(ts, toTimeMarket(ts))
    else
        getDurs(ts)
        for date in (dateBegin + Day(1)):Day(1):(expr - Day(1))
            durs = getDurs(date)
        end
    end
end

end