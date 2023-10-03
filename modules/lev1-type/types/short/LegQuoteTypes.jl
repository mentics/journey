module LegQuoteTypes
using SH, BaseTypes, SmallTypes, QuoteTypes, LegTypes, OptionMetaTypes, ChainTypes

export LegQuote, LegQuoteOpen, LegQuoteClose

# TODO:
# abstract type Dir end
# abstract type Open <: Dir end
# abstract type Close <: Dir end

abstract type Open end
abstract type Close end

struct LegQuote{S} <: LegLike
    leg::Leg
    quot::Quote
    # NOTE: Greeks are always stored here as if the associated position was open, unlike Quote
    # meta::OptionMeta
    # function LegQuote(leg::Leg, quot::Quote, meta::OptionMeta)
    #     side = getSide(leg)
    #     newQuote = Quote(quot, side)
    #     if quot != newQuote
    #         @error "LegQuote() wrong quote received" quot newQuote leg meta
    #         # @warn "LegQuote() wrong quote received" quot newQuote stacktrace()
    #     end
    #     if side == Side.short && getBid(newQuote) == 0.0
    #         @error "LegQuote() received 0.0 bid which may cause problems" newQuote leg meta
    #         # error("LegQuote() received 0.0 bid which may cause problems")
    #     end
    #     return new(leg, newQuote, meta)
    # end
end
const LegQuoteOpen = LegQuote{Open}
const LegQuoteClose = LegQuote{Close}
function LegQuote{Open}(oq::OptionQuote, side::Side.T, qty::Float64=1.0; adjustprices::Currency=CZ)
    dir = DirSQ(side, qty)
    return LegQuoteOpen(Leg(getOption(oq), dir), newQuote(getQuote(oq), DirSQA(dir, Action.open); adjustprices))
end
function LegQuote{Close}(oq::OptionQuote, side::Side.T, qty::Float64)
    dir = DirSQA(side, qty, Action.close)
    return LegQuoteClose(Leg(getOption(oq), dir), newQuote(getQuote(oq), DirSQA(dir, Action.open)))
end

LegQuote{Open}(leg::Leg, oq::Nothing) = nothing
function LegQuote{Open}(leg::Leg, oq::OptionQuote)
    dir = DirSQ(getSide(leg), getQuantity(leg))
    return LegQuoteOpen(leg, newQuote(getQuote(oq), DirSQA(dir, Action.open)))
end

LegQuote{Close}(leg::Leg, oq::Nothing) = nothing
function LegQuote{Close}(leg::Leg, oq::OptionQuote)
    dir = DirSQ(getSide(leg), getQuantity(leg))
    return LegQuoteClose(leg, newQuote(getQuote(oq), DirSQA(dir, Action.close)))
end

SH.random(::Type{LegQuote{Open}}, style, side) = LegQuote{Open}(random(Leg, style, side), random(Quote, side))

# LegQuote(;leg=Leg(), quot=Quote(), meta=OptionMeta()) = LegQuote(leg, quot, meta)
SH.getLeg(lm::LegQuote) = lm.leg
SH.getQuote(lm::LegQuote) = lm.quot

SH.getOption(lm::LegQuote) = getOption(lm.leg)
SH.getExpir(lm::LegQuote) = getExpir(lm.leg)
SH.getStyle(lm::LegQuote) = getStyle(lm.leg)
SH.getStrike(lm::LegQuote) = getStrike(lm.leg)
SH.getSide(lm::LegQuote) = getSide(lm.leg)
SH.getQuantity(lm::LegQuote) = getQuantity(lm.leg)

SH.getBid(lm::LegQuote) = getBid(lm.quot)
SH.getAsk(lm::LegQuote) = getAsk(lm.quot)

# function SH.withQuantity(lm::LegQuote{T}, qty::Float64) where T
#     rat = qty / getQuantity(lm)
#     LegQuote{T}(Leg(lm.leg; quantity=qty), rat * lm.quot, rat * lm.meta)
# end

# function metaToFlat(lm::LegQuote)
#     meta = lm.meta
#     greeks = meta.greeks
#     return (
#         greeks.delta,
#         greeks.theta,
#         # greeks.phi, # always seems to be NaN
#         greeks.vega,
#         greeks.rho,
#         greeks.gamma,
#         meta.mid_iv
#     )
# end

getOrigQuote(lq::LegQuoteOpen) = Side.short == getSide(lq) ? getQuote(lq) : QuoteTypes.invert(getQuote(lq))
getOrigQuote(lq::LegQuoteClose) = Side.long == getSide(lq) ? getQuote(lq) : QuoteTypes.invert(getQuote(lq))

end