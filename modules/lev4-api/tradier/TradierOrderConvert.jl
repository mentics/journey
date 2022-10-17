module TradierOrderConvert
using SH, BaseTypes, SmallTypes, OrderTypes, StatusTypes, LegTypes
using BaseUtil, TradierUtil

# primitDir = ordType != "market" ? tord["price"] * tierTypeSign(tord["type"]) : 0.0

function SH.to(::Type{Order}, tord::Dict{String,Any})::Order
    legs = haskey(tord, "leg") ? tord["leg"] : [tord]
    class = toEnum(OrderClass, tord["class"])
    typ = toEnum(OrderType, tord["type"])

    primitDir = haskey(tord, "price") ? abs(tord["price"]) : nothing
    prillDir = haskey(tord, "avg_fill_price") ? abs(tord["avg_fill_price"]) : nothing
    # @info "" class typ tord["side"] (tord["side"] in ("buy_to_open", "buy_to_close"))
    if (class == OrderClass.multileg && typ == OrderType.debit) ||
            (class != OrderClass.multileg && tord["side"] in ("buy_to_open", "buy_to_close"))
        !isnothing(primitDir) && (primitDir *= -1)
        !isnothing(prillDir) && (prillDir *= -1)
    end

    return Order{toStatus(tord["status"])}(tord["id"], tord["symbol"], class, typ, primitDir, prillDir, tos(LegOrder, legs), tier.parseTs(tord["create_date"]), tier.parseTs(tord["transaction_date"]))
end

function SH.to(::Type{LegOrder}, tleg::Dict{String,Any})::LegOrder
    action = Action.T(tierActionLeg(tleg)) # tleg["side"] in ("buy_to_open", "sell_to_open") ? Action.open : Action.close
    side = Side.T(tierSideLeg(tleg)) # tleg["side"] in ("buy_to_open", "buy_to_close") ? Side.long : Side.short
    prillDir = tierLegDir(tleg) * abs(tleg["avg_fill_price"])
    # TODO: commented this assert out because started using this for conflict legs, but is that ok?
    # @assert action == Action.close || !iszero(prillDir) "zero prillDir: $(action) $(prillDir) $(tleg["status"])"
    @assert checkDirOrder(side, prillDir) "checkDirOrder($(side), $(prillDir))"
    return LegOrder(tleg["id"], toStatus(tleg["status"]), action, C(prillDir),
                    Leg(tier.occToOpt(tleg["option_symbol"]), abs(tleg["quantity"]), side),
                    tier.parseTs(tleg["create_date"]), tier.parseTs(tleg["transaction_date"]))
end

function fromAssigned(tord::Dict{String,Any}, legTrade1, legTrade2)::Order
    typ = toEnum(OrderType, tord["type"])
    tlegs = tord["leg"]
    @assert tlegs[1]["class"] == "equity"
    @assert tlegs[2]["class"] == "option"
    prillDir1 = tlegs[1]["avg_fill_price"] - getStrike(legTrade1) # bought it at strike, selling it at fill
    prillDir2 = tlegs[2]["avg_fill_price"]
    prillDir = prillDir1 + prillDir2
    @assert tlegs[1]["quantity"] == 100 * tlegs[2]["quantity"]
    primitDir = getStrike(legTrade2) - getStrike(legTrade1)

    leg1 = LegOrder(tlegs[1]["id"], Filled, Action.close, prillDir1, switchSide(getLeg(legTrade1)),
            tier.parseTs(tlegs[1]["create_date"]), tier.parseTs(tlegs[1]["transaction_date"]))
    leg2 = LegOrder(tlegs[2]["id"], Filled, Action.close, prillDir2, switchSide(getLeg(legTrade2)),
            tier.parseTs(tlegs[2]["create_date"]), tier.parseTs(tlegs[2]["transaction_date"]))
    return Order{Filled}(tord["id"], tord["symbol"], OrderClass.combo, typ, primitDir, prillDir, [leg1, leg2], tier.parseTs(tord["create_date"]), tier.parseTs(tord["transaction_date"]))
end

tierActionLeg(tierSide) = occursin("open", tierSide) ? 1 : -1
tierActionLeg(lg::Dict{String,Any}) = tierActionLeg(lg["side"])
tierSideLeg(tierSide) = occursin("buy", tierSide) ? 1 : -1
tierSideLeg(lg::Dict{String,Any}) = tierSideLeg(lg["side"])
# tierLegDir(legOrd) = -Int(tierActionLeg(legOrd)) * Int(tierSideLeg(legOrd))
tierLegDir(legOrd) = -tierSideLeg(legOrd)

tierActionOrder(tord::Dict{String,Any}) = haskey(tord, "leg") ? tierActionLeg(tord["leg"][1]) : tierActionLeg(tord) # TODO: handle no leg case?
tierNumLegs(tord::Dict{String,Any}) = haskey(tord, "leg") ? length(tord["leg"]) : 1

end