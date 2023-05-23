module Scoring
using BaseTypes
import Kelly, Pricing
import DateUtil:calcRate
import LinesLeg as LL

export ScoreData

struct ScoreData
    profit::Currency
    risk::Currency
    rate::Float64
    kel::Float64
end
# const ZERO_SCORES = Scores(CZ, CZ, 0.0, 0.0)
# @inline zero(::Scores) = ZERO_SCORES

function score_condor_long(prob, curp, tmult, lms; params=(;MinProfit=0.1, MinRate=0.4, MinKel=0.1, PriceAdjust=-0.01))
    # TODO: clean up without exception?
    segs = nothing
    try
        segs = LL.toSegments(lms)
    catch e
        # ignore for now, but we can't use this lms
        return nothing
    end
    neto = Pricing.price(lms, false) + params.PriceAdjust
    profit = neto
    profit > params.MinProfit || return nothing

    margin = Pricing.calcMarg(curp, segs)
    @assert margin.long >= 0.0
    @assert margin.short >= 0.0
    risk = max(margin)
    if risk < 0.009
        return nothing
    end

    rate = calcRate(tmult, profit, risk)
    rate >= params.MinRate || return nothing

    segsWithZeros = LL.toSegmentsWithZeros(segs)
    kel = Kelly.calcKel(prob, risk, segsWithZeros)
    kel >= params.MinKel || return nothing

    return (;score=kel, neto, margin, data=ScoreData(profit, risk, rate, kel))
end

end