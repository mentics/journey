module Positions
using Dates
using SH, BaseTypes, SmallTypes, OptionTypes, LegTypes, PositionTypes, Caches
using TradierUtil, TradierAccount

export positions, isConflict, verifyPoss, verifyPos

positions(; age=Hour(1))::Vector{Position} = cache!(getPoss, POSITIONS_TYPE, POSITIONS, age)

isConflict(opt::Option, side::Side.T) = !isnothing(conflicter(opt, side))
conflicter(opt::Option, side::Side.T) = findfirst(pos -> isConflict(opt, side, pos), positions())
isConflict(opt::Option, side::Side.T, pos::Position) = opt == getOption(pos) && side != getSide(pos)

using CollUtil, LegTradeTypes
function verifyPoss(legs::AVec{LegTrade})::Vector{Bool}
    poss = positions(; age=Minute(10))
    return [verifyPos(legs[i], poss) for i in eachindex(legs)]
end
verifyPos(leg::LegTrade, poss::Vector{Position}=positions(;age=Minute(10)))::Bool = !isnothing(find(p -> matches(p, leg), poss))

#region Local
const POSITIONS = :positions
const POSITIONS_TYPE = Vector{Position}

getPoss() = toPosition.(tradierPositions())
function toPosition(tierPos::Dict{String,Any})
    qty = Float64(tierPos["quantity"])
    Position(
        Leg(tier.occToOpt(tierPos["symbol"]),
            abs(qty),
            signbit(qty) ? Side.short : Side.long),
        tier.parseToMs(tierPos["date_acquired"]),
        C(abs(tierPos["cost_basis"] / 100.0) / qty)
    )
end

matches(p::Position, leg::LegTrade) = getOption(p) == getOption(leg) && getSide(p) == getSide(leg) && getQuantity(p) >= getQuantity(leg)
#endregion

end