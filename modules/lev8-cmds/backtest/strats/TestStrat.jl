module TestStrat
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
using LogUtil, OutputUtil, BacktestUtil, CollUtil, DateUtil
import DateUtil:timult,calcRate
import ChainUtil as ch
using Pricing
import ProbKde, ProbUtil, Kelly, HistData
import LinesLeg as LL

#region Types
struct Scoring
    ret::Currency
    risk::Currency
    rate::Float64
    kel::Float64
end

struct Cand
    lms::NTuple{3,LegMetaOpen}
    score::Float64
    neto::Float64
    margin::Sides{Float64}
    scoring::Scoring
end

struct Params
    balInit::PT
    maxMarginPerTrade::PT
end

struct Context
    bufCand::Vector{Cand}
end

struct TStrat <: Strat
    acctTypes::Tuple{Int,DataType}
    params::Params
    ctx::Context
end

makeCtx() = Context(Vector{Cand}())

makeStrat() = TStrat(
    (3,Scoring),
    Params(C(1000), C(52)),
    makeCtx(),
)
#endregion

#region InterfaceImpl
function (s::TStrat)(ops, tim, chain)::Nothing
    curp = ch.getCurp(chain)

    min(ops.marginAvail()) > 0 || return

    keep = s.ctx.bufCand
    empty!(keep)

    xpirs = ch.getXpirs(chain)
    starti = CollUtil.gtee(xpirs, bdaysAfter(tim.date,24))
    endi = CollUtil.gtee(xpirs, bdaysAfter(tim.date, 84))
    for xpir in xpirs[starti:endi]
        fromPrice = curp # TODO: could use recent extrema?
        tmult = timult(tim.date, xpir)
        search = ch.toSearch(curp, chain.xsoqs[xpir].put)
        prob = ProbKde.probToClose(F(fromPrice), F(HistData.vixOpen(tim.date)), tim.ts, xpir)
        findEntry!(keep, search, tmult, prob)
    end
    if !isempty(keep)
        x = keep[1]
        multiple = qtyForMargin(s.params.maxMarginPerTrade, ops.marginAvail(), x.margin, x.scoring.kel)
        # println("Found best $(multiple): $(x)")
        if multiple > 0
            ops.openTrade(tim.ts, x.lms, toPT(x.neto, RoundDown), toPT(x.margin), multiple, "best score", keep[1].scoring)
        else
            println("0 multiple found, odd.")
        end
    end
    return
end
BaseTypes.toPT(sides::Sides{Float64})::Sides{PT} = Sides(toPT(sides.long, RoundDown), round(toPT(sides.short, RoundDown)))

BackTypes.pricingOpen(::TStrat, lmso::NTuple{3,LegMetaOpen}) = calcPrice(lmso)
BackTypes.pricingClose(::TStrat, lmsc::NTuple{3,LegMetaClose}) = calcPrice(lmsc)
function BackTypes.checkExit(strat::TStrat, tradeOpen::TradeBTOpen{3,Scoring}, tim, lmsc, curp)::Union{Nothing,String}
    Date(tradeOpen.ts) < tim.date || return # No closing on the same day
    curVal = tradeOpen.neto + calcPrice(lmsc)
    rate = calcRate(Date(tradeOpen.ts), tim.date, curVal, tradeOpen.extra.risk)
    # rateOrig = tradeOpen.extra.rate
    # blog("checkExit: $(tradeOpen.id) $(round(rate;digits=5)) $(round(rateOrig;digits=5))")
    # if rate >= 1.5 * rateOrig
    # if rate >= 0.4 || rate >= 1.5 * rateOrig
    #     return "rate $(rate) >= 0.4 or 1.5 * $(rateOrig)"
    # end
    ratio = trad.targetRatio(tradeOpen, Date(tim.date))
    if rate >= 0.4 * ratio
        return "rate $(rd5(rate)) >= 0.4 * $(ratio) ($(.4*ratio))"
    end
    theta = getGreeks(lmsc).theta
    if theta < -0.01 && curp <= getStrike(lmsc[3])
        return "theta $(rd5(theta)) <= 0.01 curp:$(curp)<$(getStrike(lmsc[3]))"
    end
end
#endregion

#region Find
function findEntry!(keep, search, args...)
    MaxWidth = 40.0
    # TODO: have dist based on percent chance of move size like was done in btsimple
    oqs = ch.oqsLteCurp(search, .96)
    itr1 = Iterators.reverse(eachindex(oqs)[3:end])
    for i1 in itr1
        oq1 = oqs[i1]
        itr2 = i1-1:-1:2
        for i2 in itr2
            oq2 = oqs[i2]
            # TODO: long specific
            if getStrike(oq1) - getStrike(oq2) > MaxWidth
                break
            end
            itr3 = i2-1:-1:1
            for i3 in itr3
                oq3 = oqs[i3]
                if getStrike(oq2) - getStrike(oq3) > MaxWidth
                    break
                end
                lms = (LegMetaOpen(oq3, Side.long), LegMetaOpen(oq2, Side.long), LegMetaOpen(oq1, Side.short))
                r = score(lms, args...)
                if !isnothing(r) && (isempty(keep) || r[1].score > keep[end].score)
                    # TODO: not optimized
                    info, about = r
                    push!(keep, Cand(lms, info..., about))
                    sort!(keep; rev=true, by=x->x.score)
                    length(keep) <= 10 || pop!(keep)
                end
            end
        end
    end
end

function score(lms, tmult, prob)
    neto = Pricing.bapFast(lms, .2)
    neto > 0.24 || return nothing
    margin = Pricing.calcMarginFloat(lms)
    risk = max(margin)
    @assert risk > CZ "risk was <= 0"
    rate = calcRate(tmult, neto, risk)
    rate >= .12 || return nothing
    kel = calcKel(tmult, neto, risk, prob, LL.toSections(lms))
    kel > 0 || return nothing
    # score = rate / (getStrike(lms[2]) - getStrike(lms[1]))
    score = rate * kel / risk
    return ((;score, neto, margin), Scoring(neto, risk, rate, kel))
end
#endregion

#region Util
calcPrice(lms)::PT = toPT(bap(lms, 0.0))
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

function calcKel(tmult, neto, risk, prob, sections)
    MoneyValueAPR = 0.05
    ExpDurRatioAvg = 0.5
    # ret = round(neto - .01 - risk * (MoneyValueAPR * ExpDurRatioAvg / tmult), RoundDown; digits=2)
    ret = round(neto - risk * (MoneyValueAPR * ExpDurRatioAvg / tmult), RoundDown; digits=2)
    ret > 0 || return -1.0
    # rateAdj = calcRate(tmult, ret, risk)
    winRat = calcWinRat(prob, sections)
    # side = Side.long
    # winRate = side == Side.long ? ProbUtil.cdfFromRight(prob, F(atStrike)) : ProbUtil.cdfFromLeft(prob, F(atStrike))
    # theta = getGreeks(lo).theta - getGreeks(sho).theta
    # @show ret risk winRat
    kel = Kelly.simple(winRat, F(ret), F(risk))
    return kel
end

function calcWinRat(prob, sections)
    cdfPrev = 0.0
    return reduce(sections; init=0.0) do a::Float64, s
        cdf = ProbUtil.cdfFromLeft(prob, s.x2)
        r = s.y > 0.0 ? a + (cdf - cdfPrev) : a
        # @show a, r, s.x2, s.y, cdfPrev, cdf
        cdfPrev = cdf
        return r
    end
end

blog(args...) = LogUtil.logit(:backtest, args...)
#endregion

#region Testing
import SimpleStore as SS
probFor(ts::DateTime, xpir::Date) = ProbKde.probToClose(F(SS.curpFor(ts)), F(HistData.vixOpen(Date(ts))), ts, xpir)
function testCalcWinRate(tradeOpen)
    lms = tradeOpen.lms
    ts = tradeOpen.ts
    prob = probFor(ts, getExpir(lms))
    sitr = LL.toSections(lms)
    winRat = calcWinRat(prob, sitr)
    return winRat
end

function testScore(tradeOpen)
    lms = tradeOpen.lms
    ts = tradeOpen.ts
    tmult = timult(Date(ts), getExpir(lms))
    prob = probFor(ts, getExpir(lms))
    score(lms, tmult, prob)
end
#endregion

end
