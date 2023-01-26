module TestStrat
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
import LogUtil
import DateUtil:timult,calcRate
import ChainUtil as ch
using Pricing

#region Types
struct Scoring
    ret::Currency
    risk::Currency
    rate::Float64
end

struct Cand
    lms::NTuple{3,LegMetaOpen}
    score::Float64
    scoring::Scoring
end

struct Params
    balInit::Currency
end

struct Context
    bufCand::Vector{Cand}
end

struct TStrat <: Strat
    params::Params
    ctx::Context
end

makeCtx() = Context(Vector{Cand}())

makeStrat() = TStrat(
    Params(C(1000)),
    makeCtx(),
)
#endregion

#region InterfaceImpl
function (s::TStrat)(ops, tim, chain)
    curp = ch.getCurp(chain)
    log("Strat running for ts $(tim.ts) curp:$(curp)")

    keep = s.ctx.bufCand
    empty!(keep)

    for xpir in ch.getXpirs(chain)[8:20]
        tmult = timult(tim.date, xpir)
        search = ch.toSearch(curp, chain.xsoqs[xpir].put)
        findEntry(keep, tim, search, tmult)
        # if !isempty(keep)
        #     println("Found: ", keep[1])
        # end
    end
    if !isempty(keep)
        ops.openTrade(keep[1].lms, C(round(keep[1].scoring.ret, RoundDown; digits=2)), 1, "scored", keep[1].scoring)
    end
end

BackTypes.pricingOpen(::TStrat, lmso::NTuple{3,LegMetaOpen}) = calcPrice(lmso)
BackTypes.pricingClose(::TStrat, lmsc::NTuple{3,LegMetaClose}) = calcPrice(lmsc)
function BackTypes.checkExit(strat::TStrat, tradeOpen::TradeBTOpen{3}, tim, lmsc)::Union{Nothing,String}
    curVal = tradeOpen.neto + calcPrice(lmsc)
    rate = calcRate(tim.date, getExpir(tradeOpen), curVal, tradeOpen.extra.risk)
    rateOrig = tradeOpen.extra.rate
    # log("checkExit: $(tradeOpen.id) $(round(rate;digits=5)) $(round(rateOrig;digits=5))")
    if rate >= 1.5 * rateOrig
        return "rate $(rate) >= 1.5 * $(rateOrig)"
    end
end
#endregion

#region Find
function findEntry(keep, tim, search, tmult)
    # global keepSearch = search
    oqs = ch.oqsLteCurp(search, .9)
    itr1 = Iterators.reverse(eachindex(oqs)[3:end])
    for i1 in itr1
        itr2 = i1-1:-1:2
        for i2 in itr2
            itr3 = i2-1:-1:1
            for i3 in itr3
                # @show i1 i2 i3
                oq1 = oqs[i1]
                oq2 = oqs[i2]
                oq3 = oqs[i3]
                lms = (LegMetaOpen(oq3, Side.long), LegMetaOpen(oq2, Side.long), LegMetaOpen(oq1, Side.short))
                r = score(tim, lms, tmult)
                if !isnothing(r) && (isempty(keep) || r[1] > keep[end].score)
                    # TODO: not optimized
                    push!(keep, Cand(lms, r[1], r[2]))
                    sort!(keep; rev=true, by=x->x.score)
                    length(keep) <= 10 || pop!(keep)
                end
            end
        end
    end
end

function score(tim, lms, tmult)
    # global keepLms = lms
    # log("Scoring")
    neto = Pricing.bapFast(lms, .2)
    neto > 0.02 || return nothing
    risk = Pricing.calcMargin(lms)
    @assert risk > CZ "risk was <= 0"
    # println(tim.date, ' ',getExpir(lms))
    # TODO: the following assumes we want it to expire worthless
    # rate = calcRate(tim.date, getExpir(lms), neto, risk)
    rate = calcRate(tmult, neto, risk)
    # log(lms)
    # error("stop")
    return (rate, Scoring(neto, risk, rate))
end
#endregion

#region Util
calcPrice(lms) = bap(lms, .2)
log(args...) = LogUtil.logit(:backtest, args...)
#endregion

end
