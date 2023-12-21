module DataConst
using Dates

# Based on earliest historical options data from ThetaData
const DATE_START = Date(2012, 6, 1)

# Ignore xpirs more than this far out everywhere.
# const XPIRS_WITHIN = Day(192)

# Only analyze xpirs within XPIRS_WITHIN_CALC.
const XPIRS_WITHIN_CALC2 = Day(20)

filter_ts_calc(df; view=false) = filter([:ts,:expir] => filter_for_calc, df; view)

function filter_for_calc(ts, xpirts)
    date = Date(ts)
    return xpirts - ts < XPIRS_WITHIN_CALC2 &&
            ts != xpirts &&
            ts < cal.getMarketClose(date) &&
            ts > cal.getMarketOpen(date)
end
filter_ts_calc(ts) = xpir -> filter_for_calc(ts, xpir)

# filter_df(df, within_days; view=true) = filter([:ts,:expir] => filter_all(within_days), df; view)

# remove_at_xpirts(df) = filter([:ts,:expir] => ((ts, xpirts) -> ts != xpirts), df; view=true)
# filter_within_days(df, within_days) = filter([:ts, :expir] => is_within_days(within_days), df; view=true)
# is_within_days(within_days) = (ts, xpir) -> xpir - ts <= within_days

end