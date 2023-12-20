module DataConst
using Dates

# Based on earliest historical options data from ThetaData
const DATE_START = Date(2012, 6, 1)

# Ignore xpirs more than this far out everywhere.
const XPIRS_WITHIN = Day(192)

# Only analyze xpirs within XPIRS_WITHIN_CALC.
const XPIRS_WITHIN_CALC = Day(40)

end