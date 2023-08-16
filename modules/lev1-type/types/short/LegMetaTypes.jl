module LegMetaTypes
using SH, BaseTypes, SmallTypes, QuoteTypes, LegTypes, OptionMetaTypes, ChainTypes

export LegMeta, LegMetaOpen, LegMetaClose

abstract type Open end
abstract type Close end

struct LegMeta{S} <: LegLike
    leg::Leg
    quot::Quote
    # NOTE: Greeks are always stored here as if the associated position was open, unlike Quote
    meta::OptionMeta
    # function LegMeta(leg::Leg, quot::Quote, meta::OptionMeta)
    #     side = getSide(leg)
    #     newQuote = Quote(quot, side)
    #     if quot != newQuote
    #         @error "LegMeta() wrong quote received" quot newQuote leg meta
    #         # @warn "LegMeta() wrong quote received" quot newQuote stacktrace()
    #     end
    #     if side == Side.short && getBid(newQuote) == 0.0
    #         @error "LegMeta() received 0.0 bid which may cause problems" newQuote leg meta
    #         # error("LegMeta() received 0.0 bid which may cause problems")
    #     end
    #     return new(leg, newQuote, meta)
    # end
end
const LegMetaOpen = LegMeta{Open}
const LegMetaClose = LegMeta{Close}
function LegMeta{Open}(oq::OptionQuote, side::Side.T, qty::Float64=1.0; adjustprices::Currency=CZ)
    dir = DirSQ(side, qty)
    return LegMetaOpen(Leg(getOption(oq), dir), newQuote(getQuote(oq), DirSQA(dir, Action.open); adjustprices), newOptionMeta(getOptionMeta(oq), dir))
end
function LegMeta{Close}(oq::OptionQuote, side::Side.T, qty::Float64)
    dir = DirSQA(side, qty, Action.close)
    return LegMetaClose(Leg(getOption(oq), dir), newQuote(getQuote(oq), DirSQA(dir, Action.open)), newOptionMeta(getOptionMeta(oq), dir))
end

LegMeta{Open}(leg::Leg, oq::Nothing) = nothing
function LegMeta{Open}(leg::Leg, oq::OptionQuote)
    dir = DirSQ(getSide(leg), getQuantity(leg))
    return LegMetaOpen(leg, newQuote(getQuote(oq), DirSQA(dir, Action.open)), newOptionMeta(getOptionMeta(oq), dir))
end

LegMeta{Close}(leg::Leg, oq::Nothing) = nothing
function LegMeta{Close}(leg::Leg, oq::OptionQuote)
    dir = DirSQ(getSide(leg), getQuantity(leg))
    return LegMetaClose(leg, newQuote(getQuote(oq), DirSQA(dir, Action.close)), newOptionMeta(getOptionMeta(oq), dir))
end

# LegMeta(;leg=Leg(), quot=Quote(), meta=OptionMeta()) = LegMeta(leg, quot, meta)
SH.getLeg(lm::LegMeta) = lm.leg
SH.getQuote(lm::LegMeta) = lm.quot
SH.getOptionMeta(lm::LegMeta) = lm.meta

SH.getOption(lm::LegMeta) = getOption(lm.leg)
SH.getExpir(lm::LegMeta) = getExpir(lm.leg)
SH.getStyle(lm::LegMeta) = getStyle(lm.leg)
SH.getStrike(lm::LegMeta) = getStrike(lm.leg)
SH.getSide(lm::LegMeta) = getSide(lm.leg)
SH.getQuantity(lm::LegMeta) = getQuantity(lm.leg)

SH.getBid(lm::LegMeta) = getBid(lm.quot)
SH.getAsk(lm::LegMeta) = getAsk(lm.quot)
SH.getIv(lm::LegMeta) = getIv(lm.meta)
SH.getMeta(lm::LegMeta) = lm.meta

# function SH.withQuantity(lm::LegMeta{T}, qty::Float64) where T
#     rat = qty / getQuantity(lm)
#     LegMeta{T}(Leg(lm.leg; quantity=qty), rat * lm.quot, rat * lm.meta)
# end

SH.getGreeks(lm::LegMeta)::Greeks = lm.meta.greeks
function metaToFlat(lm::LegMeta)
    meta = lm.meta
    greeks = meta.greeks
    return (
        greeks.delta,
        greeks.theta,
        # greeks.phi, # always seems to be NaN
        greeks.vega,
        greeks.rho,
        greeks.gamma,
        meta.mid_iv
    )
end

end