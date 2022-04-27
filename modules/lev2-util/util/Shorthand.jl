module Shorthand
using Dates
using SH, SmallTypes

export legsTosh, shLeg, shLegs

# l448p@1 / s452p2@2 / s456c@2 / l458c2@1
legsTosh(legs, exps) = join((legTosh(leg, exps) for leg in legs), " / ")

legTosh(leg, exps) = string(toCode(getSide(leg)), round(getStrike(leg); digits=1), toCode(getStyle(leg)), '@', searchsortedfirst(exps, getExpiration(leg)))

# strikes = join(map(l -> "$(first(string(side(l))))$(s(strike(l), 1))$(first(string(style(l))))@$(searchsortedfirst(exps, expiration(l)))", legs(ar)), " / ")


# export shLegs, shLeg, toSh

# function toSh(o)
#     # TODO: this assumes all the same expirations
#     expir = expiration(legs(o)[1])
#     return "$(expir): $(toSh.(legs(o))) : $(action(o)) $(priceLimit(o)) : $(tag(o))"
# end

# function toSh(l::AbstractLeg)
#     # return "$(first(string(side(l))))$(s(strike(l), 1))$(first(string(style(l))))"
#     return "$(first(string(side(l))))$(s(strike(l), 1))$(first(string(style(l))))"
# end

using BaseTypes, SmallTypes, OptionTypes, LegTypes
export shLeg
shLeg(sh::AbstractString, expir::Date)::Leg = shLeg(sh, [expir])
function shLeg(sh::AbstractString, expir::Vector{Date})::Leg
    m = match(r"(.)([\d\.]+)(.)(\d)?@?(\d)?", sh)
    xs = m.captures
    styl = to(Style.T, xs[3])
    sid = to(Side.T, xs[1])
    qty = !isnothing(xs[4]) ? parse(Int, xs[4]) : 1
    ex = !isnothing(xs[5]) ? parse(Int, xs[5]) : 1
    return Leg(Option(styl, expir[ex], parse(Currency, xs[2])), Float64(qty), sid)
end

# shLegs(sh::AbstractString, expir::Date)::Vector{Leg} = shLegs(sh, [expir])
function shLegs(sh::AbstractString, expir::Vector{Date})::Vector{Leg}
    ss = split(sh, r"\s*/\s*")
    return [shLeg(s, expir) for s in ss]
end
function shLegs(sh::AbstractString)::Vector{Leg}
    m = match(r"(.+?)\s*:\s*(.+)\s*", sh)
    xs = m.captures
    expirs = Date.(split(xs[1], ','))
    legs = xs[2]
    return shLegs(legs, expirs)
end

# # module Test
# # import ..shOrder
# # using StructEquality
# # @def_structequal MultiLegOrder
# # const EXAMPLE = "2021-12-22: l448p / s452p / s456c / l458c : open 2.12 : tag"
# # const EXAMPLE_RESULT = MultiLegOrder("tag", Action.open, OrderDuration.day, "SPY", OrderType.credit, Currency(2.120), Leg[Leg(Option(Style.put, Dates.Date("2021-12-22"), Currency(448.000)), Side.long, 1.0), Leg(Option(Style.put, Dates.Date("2021-12-22"), Currency(452.000)), Side.short, 1.0), Leg(Option(Style.call, Dates.Date("2021-12-22"), Currency(456.000)), Side.short, 1.0), Leg(Option(Style.call, Dates.Date("2021-12-22"), Currency(458.000)), Side.long, 1.0)])
# # if shOrder(EXAMPLE) != EXAMPLE_RESULT
# #     throw("Shorthand module error: shOrder(EXAMPLE) should be EXAMPLE_RESULT")
# # end
# # end

end