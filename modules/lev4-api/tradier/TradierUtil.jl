module TradierUtil
using Dates, TimeZones
using DateUtil#, StoreUtil
using SH, BaseTypes, SmallTypes, OptionTypes

export tier

const tier = TradierUtil

# ex: "2022-03-24T14:44:10.086Z"
const TRADIER_DATETIME = dateformat"yyyy-mm-ddTHH:MM:SS.sss\Z"

const OCC_REGEX = r"(\w+)(\d{6})([PC])(\d{8})"
const OCC_DATE_FORMAT = dateformat"yymmdd"

# https://help.yahoo.com/kb/SLN13884.html
# TODO: hardcoding SPY here is ok, but...
function optToOcc(info::Option)::String
    dt = Dates.format(info.expiration, OCC_DATE_FORMAT)
    # strike = replace(Printf.@sprintf("%09.3f", info.strike), '.'=>"")
    str1 = replace(string(getStrike(info)), '.'=>"")
    strikeStr = repeat('0',8 - length(str1)) * str1
    underlying = "SPY" # TODO: we can pull default symbol from Globals
    return "$(underlying)$(dt)$(uppercase(toCode(info.style)))$(strikeStr)"
end

function occToOpt(occ::AStr)::Option
    m = match(OCC_REGEX, occ)
    expir = Date(m[2], OCC_DATE_FORMAT) + Dates.Year(2000)
    return Option(to(Style.T, m[3]), expir, parse(Currency, m[4])/1000)
end

toDateStr(ms::Int)::String = Dates.format(TimeZones.unix2zdt(ms / 1000.0), TRADIER_DATETIME)
parseTs(dt::AStr)::DateTime = DateTime(dt, TRADIER_DATETIME)

toTierAct(act::Action.T, side::Side.T) = Int(act) * Int(side) == 1 ? "buy" : "sell"
toTierSide(act::Action.T, sid::Side.T)::String = "$(toTierAct(act, sid))_to_$(act)"

orderType(val::Real) = val > zero(val) ? "credit" : (val < zero(val) ? "debit" : "even")

end