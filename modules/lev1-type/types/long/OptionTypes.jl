module OptionTypes
using Dates
using BaseTypes, SH, SmallTypes

export Option, canShort

struct Option
    style::Style.T
    expiration::Date
    strike::Currency
end
Option(row::NamedTuple) = Option(Style.T(row.style), row.expiration, C(Float64(row.strike)))
Option(;style=Style.call, expiration=today()+Day(1), strike=C(450.0)) = Option(style, expiration, strike)
SH.getOption(o::Option) = o
SH.getStyle(o::Option) = o.style
SH.getStrike(o::Option) = o.strike
SH.getExpir(o::Option) = o.expiration
# SH.random(::Type{Option}) = Option(random(Style.T), rand((today()+Day(1)):Day(1):(today()+Day(10))), randomC())
Base.show(io::IO, o::Option) = print(io, "Option($(o.style), $(o.expiration), $(o.strike))")
# Usually called with config as Globals.get(:Strats)
function canShort(config::Dict{Symbol,Any}, curp::Currency)
    cch = curp - config[:maxCallHeight]
    cph = curp + config[:maxPutHeight]
    return o ->
        (getStyle(o) == Style.call && getStrike(o) >= cch) ||
        (getStyle(o) == Style.put && getStrike(o) <= cph)
end

end