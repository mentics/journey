module Positions
using Dates
using SH, BaseTypes, SmallTypes, OptionTypes, LegTypes, PositionTypes, Caches
using TradierUtil, TradierAccount

export positions, verifyPoss, verifyPos

positions(; age=Hour(1))::Vector{Position} = cache!(getPoss, POSITIONS_TYPE, POSITIONS, age)

using CollUtil, LegTradeTypes
function verifyPoss(legs::AVec{LegTrade})::Vector{Bool}
    poss = positions(; age=Minute(10))
    return [verifyPos(legs[i], poss) for i in eachindex(legs)]
end
verifyPos(leg::LegTrade, poss::Vector{Position}=positions(;age=Minute(10)))::Bool = !isnothing(find(p -> matches(p, leg), poss))

#region Local
const POSITIONS = :positions
const POSITIONS_TYPE = Vector{Position}

getPoss() = filter(!isnothing, toPosition.(tradierPositions()))
function toPosition(tierPos::Dict{String,Any})
    length(tierPos["symbol"]) == 18 || (println("WARN: Found unexpected position (may be assignment): ", tierPos) ; return )
    qty = Float64(tierPos["quantity"])
    Position(toLeg(tierPos, qty), toBasis(tierPos, qty), tier.parseTs(tierPos["date_acquired"]))
end

toLeg(tierPos::Dict{String,Any}, qty::Float64)::Leg = Leg(tier.occToOpt(tierPos["symbol"]), abs(qty), signbit(qty) ? Side.short : Side.long)
toBasis(tierPos::Dict{String,Any}, qty::Float64) = C(abs(tierPos["cost_basis"] / 100.0) / qty)

matches(p::Position, leg::LegTrade) = getOption(p) == getOption(leg) && getSide(p) == getSide(leg) && getQuantity(p) >= getQuantity(leg)
#endregion

end