module OptionTypes
using Dates
using BaseTypes, SH, SmallTypes

export Option, isNitm

struct Option
    style::Style.T
    expiration::Date
    strike::Currency
end
Option(;style=Style.call, expiration=today()+Day(1), strike=C(450.0)) = Option(style, expiration, strike)
SH.getStyle(o::Option) = o.style
SH.getStrike(o::Option) = o.strike
SH.getExpiration(o::Option) = o.expiration
# SH.random(::Type{Option}) = Option(random(Style.T), rand((today()+Day(1)):Day(1):(today()+Day(10))), randomC())
Base.show(io::IO, o::Option) = print(io, "Option($(o.style), $(o.expiration), $(o.strike))")
# Usually called with config as Globals.get(:Strats)
isNitm(o::Option, curp::Currency, config::Dict{Symbol,Any}) =
        (o.style == Style.call && o.strike <= curp + config[:maxCallHeight]) ||
        (o.style == Style.put && o.strike >= curp - config[:maxPutHeight])

end