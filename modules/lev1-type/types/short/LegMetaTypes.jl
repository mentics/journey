module LegMetaTypes
using SH, BaseTypes, SmallTypes, QuoteTypes, LegTypes, OptionMetaTypes, ChainTypes

export LegMeta

struct LegMeta
    leg::Leg
    quot::Quote
    meta::OptionMeta
    function LegMeta(leg::Leg, quot::Quote, meta::OptionMeta)
        side = getSide(leg)
        newQuote = Quote(quot, side)
        if quot != newQuote
            @error "LegMeta() wrong quote received" quot newQuote leg meta
            # @warn "LegMeta() wrong quote received" quot newQuote stacktrace()
        end
        if side == Side.short && getBid(newQuote) == 0.0
            @error "LegMeta() received 0.0 bid which may cause problems" newQuote leg meta
            # error("LegMeta() received 0.0 bid which may cause problems")
        end
        new(leg, newQuote, meta)
    end
end
LegMeta(oq::OptionQuote, qty::Float64, side::Side.T) = LegMeta(Leg(getOption(oq), qty, side), getQuote(oq, side), getMeta(oq))
LegMeta(;leg=Leg(), quot=Quote(), meta=OptionMeta()) = LegMeta(leg, quot, meta)
SH.getLeg(lm::LegMeta) = lm.leg
SH.getQuote(lm::LegMeta) = lm.quot
SH.getMeta(lm::LegMeta) = lm.meta

SH.getOption(lm::LegMeta) = getOption(lm.leg)
SH.getExpiration(lm::LegMeta) = getExpiration(lm.leg)
SH.getStyle(lm::LegMeta) = getStyle(lm.leg)
SH.getStrike(lm::LegMeta) = getStrike(lm.leg)
SH.getSide(lm::LegMeta) = getSide(lm.leg)
SH.getQuantity(lm::LegMeta) = getQuantity(lm.leg)

SH.getBid(lm::LegMeta) = getBid(lm.quot)
SH.getAsk(lm::LegMeta) = getAsk(lm.quot)
SH.getIv(lm::LegMeta) = getIv(lm.meta)

SH.getNetOpen(lm::LegMeta) = getQuantity(lm.leg) * bap(lm) # getBid(lm.quot)

SH.addQuantity(lm::LegMeta, addend::Real) =
    LegMeta(Leg(lm.leg; quantity=getQuantity(lm.leg) + addend), lm.quot, lm.meta)
SH.withQuantity(lm::LegMeta, qty::Real) =
    LegMeta(Leg(lm.leg; quantity=qty), lm.quot, lm.meta)

# TODO: still not sure if abs is right here
SH.to(OptionQuote, lm::LegMeta) = OptionQuote(getOption(lm), abs(getQuote(lm)), getMeta(lm), nothing)

SH.getTheta(lm::LegMeta) = lm.meta.theta * getQuantityDir(getLeg(lm))
# SH.getTheta(lms::Coll{LegMeta}) = sum(getTheta, lms)
# getTheta(oq::OptionQuote) = oq.meta.theta * getQuantityDir(getLeg(lm))
# getTheta(oqs::Coll{OptionQuote}) = sum(getThetaDir, oqs)
SH.getDelta(lm::LegMeta) = lm.meta.delta * getQuantityDir(getLeg(lm))
# SH.getDelta(lms::Coll{LegMeta}) = sum(getDelta, lms)
SH.getGamma(lm::LegMeta) = lm.meta.gamma * getQuantityDir(getLeg(lm))
# SH.getGamma(lms::Coll{LegMeta}) = sum(getGamma, lms)

end