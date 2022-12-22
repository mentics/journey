module LegMetaTypes
using SH, BaseTypes, SmallTypes, QuoteTypes, LegTypes, OptionMetaTypes, ChainTypes

export LegMeta, LegMetaOpen, LegMetaClose

abstract type Open end
abstract type Close end

struct LegMeta{S}
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
function LegMeta{Open}(oq::OptionQuote, side::Side.T, qty::Float64)
    dir = DirSQ(side, qty)
    return LegMetaOpen(Leg(getOption(oq), dir), newQuote(getQuote(oq), DirSQA(dir, Action.open)), newOptionMeta(getOptionMeta(oq), dir))
end
function LegMeta{Close}(oq::OptionQuote, side::Side.T, qty::Float64)
    dir = DirSQA(side, qty, Action.close)
    return LegMetaClose(Leg(getOption(oq), dir), newQuote(getQuote(oq), DirSQA(dir, Action.open)), newOptionMeta(getOptionMeta(oq), dir))
end

function LegMeta{Close}(leg::Leg, oq::OptionQuote)
    dir = DirSQ(getSide(leg), getQuantity(leg))
    return LegMetaClose(leg, newQuote(getQuote(oq), DirSQA(dir, Action.close)), newOptionMeta(getOptionMeta(oq), dir))
end

# LegMeta(;leg=Leg(), quot=Quote(), meta=OptionMeta()) = LegMeta(leg, quot, meta)
SH.getLeg(lm::LegMeta) = lm.leg
SH.getQuote(lm::LegMeta) = lm.quot
SH.getOptionMeta(lm::LegMeta) = lm.meta

SH.getOption(lm::LegMeta) = getOption(lm.leg)
SH.getExpiration(lm::LegMeta) = getExpiration(lm.leg)
SH.getStyle(lm::LegMeta) = getStyle(lm.leg)
SH.getStrike(lm::LegMeta) = getStrike(lm.leg)
SH.getSide(lm::LegMeta) = getSide(lm.leg)
SH.getQuantity(lm::LegMeta) = getQuantity(lm.leg)

SH.getBid(lm::LegMeta) = getBid(lm.quot)
SH.getAsk(lm::LegMeta) = getAsk(lm.quot)
SH.getIv(lm::LegMeta) = getIv(lm.meta)

# SH.getNetOpen(lm::LegMeta) = getQuantity(lm.leg) * bap(lm) # getBid(lm.quot)

# SH.addQuantity(lm::LegMeta, addend::Real) =
#     LegMeta(Leg(lm.leg; quantity=getQuantity(lm.leg) + addend), lm.quot, lm.meta)
function SH.withQuantity(lm::LegMeta{T}, qty::Float64) where T
    rat = qty / getQuantity(lm)
    LegMeta{T}(Leg(lm.leg; quantity=qty), rat * lm.quot, rat * lm.meta)
end

# TODO: still not sure if abs is right here
# SH.to(OptionQuote, lm::LegMeta) = OptionQuote(getOption(lm), abs(getQuote(lm)), getMeta(lm), nothing)

# SH.getGreeks(lm::LegMeta)::Greeks = multGreeks(getQuantityDir(getLeg(lm)), getGreeks(getOptionMeta(lm)))
SH.getGreeks(lm::LegMeta)::Greeks = lm.meta.greeks #multGreeks(getQuantityDir(getLeg(lm)), getGreeks(getOptionMeta(lm)))

end