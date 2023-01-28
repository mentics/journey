module TestStrat
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
import LogUtil
import DateUtil:timult,calcRate
import ChainUtil as ch
using Pricing
import ProbKde, ProbUtil, Kelly, HistData

#region Types
struct Scoring
    ret::Currency
    risk::Currency
    rate::Float64
    kel::Float64
end

struct Cand2
    lms::NTuple{3,LegMetaOpen}
    score::Float64
    neto::Currency
    margin::Sides{}
    scoring::Scoring
end

struct Params
    balInit::PT
    maxMarginPerTrade::PT
end

struct Context
    bufCand::Vector{Cand2}
end

struct TStrat <: Strat
    acctTypes::Tuple{Int,DataType}
    params::Params
    ctx::Context
end

makeCtx() = Context(Vector{Cand2}())

makeStrat() = TStrat(
    (3,Scoring),
    Params(C(1000), C(52)),
    makeCtx(),
)
#endregion

#region InterfaceImpl
function (s::TStrat)(ops, tim, chain)::Nothing
    curp = ch.getCurp(chain)
    log("Strat running for ts $(tim.ts) curp:$(curp)")

    min(ops.marginAvail()) > 0 || return

    keep = s.ctx.bufCand
    empty!(keep)

    for xpir in ch.getXpirs(chain)[8:20]
        fromPrice = curp # TODO: could use recent extrema?
        tmult = timult(tim.date, xpir)
        search = ch.toSearch(curp, chain.xsoqs[xpir].put)
        prob = ProbKde.probToClose(F(fromPrice), F(HistData.vixOpen(tim.date)), tim.ts, xpir)
        findEntry!(keep, search, tmult, prob)
    end
    if !isempty(keep)
        x = keep[1]
        multiple = qtyForMargin(s.params.maxMarginPerTrade, ops.marginAvail(), x.margin, x.scoring.kel)
        if multiple > 0
            ops.openTrade(tim.ts, x.lms, round(PT, x.neto, RoundDown), round(PT, x.margin, RoundUp), multiple, "best score", keep[1].scoring)
        else
            println("0 multiple found, odd.")
        end
    end
    return
end

BackTypes.pricingOpen(::TStrat, lmso::NTuple{3,LegMetaOpen}) = calcPrice(lmso)
BackTypes.pricingClose(::TStrat, lmsc::NTuple{3,LegMetaClose}) = calcPrice(lmsc)
function BackTypes.checkExit(strat::TStrat, tradeOpen::TradeBTOpen{3}, tim, lmsc)::Union{Nothing,String}
    curVal = tradeOpen.neto + calcPrice(lmsc)
    rate = calcRate(tim.date, getExpir(tradeOpen), curVal, tradeOpen.extra.risk)
    rateOrig = tradeOpen.extra.rate
    # log("checkExit: $(tradeOpen.id) $(round(rate;digits=5)) $(round(rateOrig;digits=5))")
    # if rate >= 1.5 * rateOrig
    if rate >= 0.4 || rate >= 1.5 * rateOrig
        return "rate $(rate) >= 1.5 * $(rateOrig)"
    end
end
#endregion

#region Find
function findEntry!(keep, search, args...)
    oqs = ch.oqsLteCurp(search, .9)
    itr1 = Iterators.reverse(eachindex(oqs)[3:end])
    for i1 in itr1
        itr2 = i1-1:-1:2
        for i2 in itr2
            itr3 = i2-1:-1:1
            for i3 in itr3
                oq1 = oqs[i1]
                oq2 = oqs[i2]
                oq3 = oqs[i3]
                lms = (LegMetaOpen(oq3, Side.long), LegMetaOpen(oq2, Side.long), LegMetaOpen(oq1, Side.short))
                r = score(lms, args...)
                if !isnothing(r) && (isempty(keep) || r[1].score > keep[end].score)
                    # TODO: not optimized
                    info, about = r
                    push!(keep, Cand2(lms, info..., about))
                    sort!(keep; rev=true, by=x->x.score)
                    length(keep) <= 10 || pop!(keep)
                end
            end
        end
    end
end

function score(lms, tmult, prob)
    neto = Pricing.bapFast(lms, .2)
    neto > 0.02 || return nothing
    margin = Pricing.calcMargin(lms)
    risk = max(margin)
    @assert risk > CZ "risk was <= 0"
    rate = calcRate(tmult, neto, risk)
    kel = calcKel(tmult, neto, risk, prob, getStrike(lms[3])) # TODO: lms[3]
    return ((;score=rate, neto, margin), Scoring(neto, risk, rate, kel))
end
#endregion

#region Util
calcPrice(lms) = bap(lms, .2)
function qtyForMargin(maxMarginPerTrade, avail, marginTrade, kel)::Int
    # @show maxMarginPerTrade avail marginTrade kel
    long = marginTrade.long > 0 ? qtyForAvail(avail.long, marginTrade.long, kel) : typemax(Int)
    short = marginTrade.short > 0 ? qtyForAvail(avail.short, marginTrade.short, kel) : typemax(Int)
    margin = max(marginTrade)
    @assert margin > 0
    qty = min(long, short, round(Int, F(maxMarginPerTrade / margin), RoundDown))
    return qty
end
qtyForAvail(avail, risk, kel)::Int = round(Int, kel * avail / risk, RoundDown)

function calcKel(tmult, neto, risk, prob, atStrike)
    MoneyValueAPR = 0.05
    ExpDurRatioAvg = 0.5 # 0.5
    ret = round(neto - .01 - risk * (MoneyValueAPR * ExpDurRatioAvg / tmult), RoundDown; digits=2)
    rateAdj = calcRate(tmult, ret, risk)
    # TODO
    side = Side.long
    winRate = side == Side.long ? ProbUtil.cdfFromRight(prob, F(atStrike)) : ProbUtil.cdfFromLeft(prob, F(atStrike))
    # theta = getGreeks(lo).theta - getGreeks(sho).theta
    kel = Kelly.simple(winRate, F(ret), F(risk))
end

log(args...) = LogUtil.logit(:backtest, args...)
#endregion

end
