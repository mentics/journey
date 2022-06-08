module ConstructUtil
using SH, BaseTypes, SmallTypes, LegTypes, LegMetaTypes, ChainTypes

export makeCondor, makeButterfly

function makeCondor(oqs::Coll{OptionQuote,4}, side::Side.T=Side.long)::NTuple{4,LegMeta}
    otherSide = toOther(side)
    lms = (
        LegMeta(Leg(getOption(oqs[1]), 1.0, side), getQuote(oqs[1], side), getMeta(oqs[1])),
        LegMeta(Leg(getOption(oqs[2]), 1.0, otherSide), getQuote(oqs[2], otherSide), getMeta(oqs[2])),
        LegMeta(Leg(getOption(oqs[3]), 1.0, otherSide), getQuote(oqs[3], otherSide), getMeta(oqs[3])),
        LegMeta(Leg(getOption(oqs[4]), 1.0, side), getQuote(oqs[4], side), getMeta(oqs[4]))
    )
    return lms
end

# TODO: deprecate?
function makeButterfly(oqLeft::OptionQuote, oqMid::OptionQuote, oqRight::OptionQuote, side::Side.T=Side.short)::NTuple{4,LegMeta}
    otherSide = toOther(side)
    # TODO: check if exactly the same if use qty 2 for mid
    lms = (
        LegMeta(Leg(getOption(oqLeft), 1.0, side), getQuote(oqLeft, side), getMeta(oqLeft)),
        LegMeta(Leg(getOption(oqMid), 1.0, otherSide), getQuote(oqMid, otherSide), getMeta(oqMid)),
        LegMeta(Leg(getOption(oqMid), 1.0, otherSide), getQuote(oqMid, otherSide), getMeta(oqMid)),
        LegMeta(Leg(getOption(oqRight), 1.0, side), getQuote(oqRight, side), getMeta(oqRight))
    )
    return lms
end

SH.to(::Type{LegMeta}, oq, side) = LegMeta(Leg(getOption(oq), 1.0, side), getQuote(oq, side), getMeta(oq))

end