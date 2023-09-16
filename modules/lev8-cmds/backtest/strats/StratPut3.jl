module StratPut3
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegQuoteTypes
using LogUtil, OutputUtil, BacktestUtil, CollUtil, DateUtil, ThreadUtil
import DateUtil:timult,calcRate
import ChainUtil as ch
using Pricing
import ProbKde, ProbUtil, Kelly, HistData
import LinesLeg as LL

# using ThreadPools, Combinatorics, OptionQuoteTypes

macro deb(exs...)
    return
    # prblk = Expr(:call, (esc(LogUtil.logit)))
    # LogUtil.inner((:backtest, exs...), prblk)
    # return Expr(:block, prblk)
end

#region Types
struct Scoring
    ret::Currency
    risk::Currency
    rate::Float64
    kel::Float64
end

struct Cand{N}
    lms::NTuple{N,LegQuoteOpen}
    score::Float64
    neto::Float64
    margin::Sides{Float64}
    scoring::Scoring
end

struct Params
    balInit::PT
    maxMarginPerTradeRat::Float64
    InitTakeRate::Float64
    MaxWidthRat::Float64
    ProbMin::Float64
    MinNeto::PT
    MinRate::Float64
    MoneyValueAPR::Float64
    ExpDurRatioAvg::Float64
    MinXpirBdays::Int
    MaxXpirBdays::Int
end

const NUM_LEGS = 3

makeStrat() = TStrat(
    (NUM_LEGS, Scoring),
    Params(C(1000), 0.04, 0.2, 0.02, 0.68, 0.07, 0.2, 0.05, 0.5, 1, 12),
    makeCtx(),
)

struct Context{N}
    bufCandLong::Vector{Cand{N}}
    bufCandShort::Vector{Cand{N}}
end

struct TStrat <: Strat
    acctTypes::Tuple{Int,DataType}
    params::Params
    ctx::Context
end

BackTypes.hasMultExpirs(::TStrat) = false

makeCtx() = Context(Vector{Cand{NUM_LEGS}}(), Vector{Cand{NUM_LEGS}}())

const CurpPrev = Ref(CZ)
#endregion

#region InterfaceImpl
function BackTypes.resetStrat(s::TStrat)
    CurpPrev[] = CZ
    # println("RESETTING $(CurpPrev)")
end

function (s::TStrat)(ops, tim, chain, otoq)::Nothing
    params = s.params
    curp = ch.getCurp(chain)
    # if abs(curp - CurpPrev[]) < 1.0
    # if curp - CurpPrev[] < 0.0025 * curp
    # TODO: goes back up % of recent down?
    if curp - CurpPrev[] < 0.005 * curp
        if curp < CurpPrev[]
            CurpPrev[] = curp
        end
        return
    else
        CurpPrev[] = CZ
    end
    fromPrice = curp

    # min(ops.marginAvail()) > 0 || return

    keepLong = s.ctx.bufCandLong
    keepShort = s.ctx.bufCandShort
    empty!(keepLong)
    empty!(keepShort)

    xpirs = ch.getXpirs(chain)
    starti = CollUtil.gtee(xpirs, bdaysAfter(tim.date, params.MinXpirBdays))
    endi = CollUtil.gtee(xpirs, bdaysAfter(tim.date, params.MaxXpirBdays))
    for i in starti:endi
        xpir = xpirs[i]
        tmult = timult(tim.date, xpir)
        searchLong = ch.toSearch(curp, chain.xsoqs[xpir].put)
        prob = ProbKde.probToClose(F(fromPrice), F(HistData.vixOpen(tim.date)), tim.ts, xpir)
        findEntry!(keepLong, params, prob, searchLong, otoq, tmult)
    end

    if !isempty(keepLong)
        # TODO: identify if new opportunity is better than currently open one
        if (closeEarlyForMargin(params, tim.ts, ops, otoq))
            x = keepLong[1]
            multiple = max(1, qtyForMargin(s.params.maxMarginPerTradeRat * ops.bal(), ops.marginAvail(), x.margin, x.scoring.kel))
            if multiple > 0
                @assert getExpir(x.lms[1]) <= getExpir(x.lms[2])
                ops.openTrade(tim.ts, x.lms, toPT(x.neto, RoundDown), toPT(x.margin), multiple, "best score $(rd5(x.score))", x.scoring)
                CurPrev[] = curp
            else
                println("0 multiple found, odd.")
            end
        end
    else
        # println("No entry");
    end
    return
end
# BaseTypes.toPT(sides::Sides{Float64})::Sides{PT} = Sides(toPT(sides.long, RoundDown), round(toPT(sides.short, RoundDown)))

BackTypes.pricingOpen(::TStrat, lmso::NTuple{N,LegQuoteOpen}) where N = calcPrice(lmso)
BackTypes.pricingClose(::TStrat, lmsc::NTuple{N,LegQuoteClose}) where N = calcPrice(lmsc)
calcPrice(lms)::PT = toPT(Pricing.price(lms)) # toPT(bap(lms, 0.0)) + P(0.02)
calcPriceFast(lms)::Float64 = Pricing.price(lms) # Pricing.bapFast(lms, 0.0) + 0.02

function BackTypes.checkExit(params::Params, tradeOpen::TradeBTOpen{NUM_LEGS,Scoring}, tim, lmsc, curp)::Union{Nothing,String}
    dateOpen = Date(tradeOpen.ts)
    dateOpen < tim.date || return # No closing on the same day
    netc = calcPrice(lmsc)
    curVal = tradeOpen.neto + netc
    rate = calcRate(Date(tradeOpen.ts), tim.date, curVal, tradeOpen.extra.risk)
    # rateOrig = tradeOpen.extra.rate
    # blog("checkExit: $(tradeOpen.id) $(round(rate;digits=5)) $(round(rateOrig;digits=5))")
    # if rate >= 1.5 * rateOrig
    # if rate >= 0.4 || rate >= 1.5 * rateOrig
    #     return "rate $(rate) >= 0.4 or 1.5 * $(rateOrig)"
    # end
    ratio = trad.targetRatio(tradeOpen, Date(tim.date))
    if rate >= params.InitTakeRate * ratio
        # Avoid exiting on last day when would be better to expire
        if rate >= tradeOpen.extra.rate || !(tim.date == trad.targetDate(tradeOpen) && curp > getStrike(tradeOpen.lms[end]) + 2)
            return "rate $(rd5(rate)) >= $(params.InitTakeRate) * $(ratio) ($(.4*ratio))"
        end
    end

    # curpOpen = curpFor(dateOpen)
    # if curp < curpOpen * 0.98
    # theta = getGreeks(lmsc).theta
    # bdaysLeft = bdays(tim.date, getExpir(tradeOpen))
    # netExp = Pricing.netExpired(lmsc, curp)
    # @show tradeOpen.extra.risk netExp
    # if theta < -0.01 && curp <= getStrike(lmsc[3]) && bdaysLeft < 7 && netc > netExp
    #     return "theta $(rd5(theta)) <= -0.01 curp:$(curp)<$(getStrike(lmsc[3]))"
    # end
end

function closeEarlyForMargin(params, ts, ops, otoq)
    # TODO: handle long short separately
    avail = min(ops.marginAvail())
    if avail < 54.0
        cv = CollUtil.findMaxDom(firstNinf, Iterators.map(t -> trad.calcCloseInfo(t, ts, otoq, calcPrice), ops.tradesOpen()))
        !isnothing(cv) || ( println("Ran out of margin? Or couldn't quote a lot.") ; return false )
        cv.rate > 0.1 || return false
        blog("Closing for margin $(cv.curVal)")
        ops.closeTrade(cv.trade, ts, cv.lmsc, cv.netc, "margin $(avail)")
    end
    return true
end
firstNinf(::Nothing) = -Inf
firstNinf(x) = first(x)
#endregion

#region Find
function score(lms, params, prob, tmult)
    neto = calcPriceFast(lms)
    neto >= params.MinNeto || ( @deb "no score neto" neto params.MinNeto ; return nothing )
    margin = Pricing.calcMarginFloat(lms)
    risk = max(margin)
    @assert risk > CZ "risk was <= 0"

    rate = calcRate(tmult, neto, risk)
    rate >= params.MinRate || ( @deb "no score rate" rate params.MinRate ; return nothing )

    kel = calcKel(params, tmult, neto, risk, prob, LL.toSections(lms))
    kel > 0.08 || ( @deb "no score kel" kel rate tmult neto risk ; return nothing )

    score = rate * kel
    return ((;score, neto, margin), Scoring(neto, risk, rate, kel))
end

function findEntry!(keep, params, prob, search, otoq, args...)
    strikeExt, _ = ProbUtil.probFromRight(prob, params.ProbMin)
    oqs = ch.oqsLte(search, strikeExt)
    if length(oqs) < NUM_LEGS
        blog("WARN: oqs < $(NUM_LEGS)")
        return nothing
    end
    maxWidth = params.MaxWidthRat * prob.center

    lasti = lastindex(oqs)
    for i1 in eachindex(oqs)[1:end-2]
        oq1 = oqs[i1]
        str1 = getStrike(oq1)
        for i2 in i1+1:lasti
            oq2 = oqs[i2]
            # str2 = getStrike(oq2)
            # str2 - str1 <= maxWidth || break
            for i3 in i2+1:lasti
                oq3 = oqs[i3]
                str3 = getStrike(oq2)
                # str3 - str2 <= maxWidth || break
                str3 - str1 <= maxWidth || break

                lms = (LegQuoteOpen(oq1, Side.long), LegQuoteOpen(oq2, Side.long), LegQuoteOpen(oq3, Side.short))
                r = score(lms, params, prob, args...)
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
    return
end
#endregion

#region Util
function qtyForMargin(maxMarginPerTrade, avail, marginTrade, kel)::Int
    # @show maxMarginPerTrade avail marginTrade kel
    long = marginTrade.long > 0 ? qtyForAvail(avail.long, marginTrade.long, kel) : typemax(Int)
    short = marginTrade.short > 0 ? qtyForAvail(avail.short, marginTrade.short, kel) : typemax(Int)
    margin = max(marginTrade)
    @assert margin > 0
    qty = min(long, short, round(Int, F(maxMarginPerTrade / margin), RoundDown))
    # println("qty for margin: $((;avail, maxMarginPerTrade, margin, kel, qty, long, short, fromMax=round(Int, F(maxMarginPerTrade / margin), RoundDown)))")
    if qty > 0 && (avail.long < 0 || avail.short < 0)
        @show maxMarginPerTrade avail marginTrade kel long short margin qty
        error("qtyformargin not working")
    end
    return min(20, qty)
end
# qtyForAvail(avail, risk, kel)::Int = round(Int, kel * avail / risk, RoundDown)
qtyForAvail(avail, risk, kel)::Int = round(Int, kel * avail / (2 * risk), RoundDown)

function calcKel(params, tmult, neto, risk, prob, sections, adjust=true)
    if adjust
        # ret = round(neto - .01 - risk * (MoneyValueAPR * ExpDurRatioAvg / tmult), RoundDown; digits=2)
        ret = round(neto - risk * (params.MoneyValueAPR * params.ExpDurRatioAvg / tmult), RoundDown; digits=2)
        ret > 0 || return -Inf
    else
        ret = neto
    end
    # rateAdj = calcRate(tmult, ret, risk)
    winRat = calcWinRat(prob, sections)
    # side = Side.long
    # winRate = side == Side.long ? ProbUtil.cdfFromRight(prob, F(atStrike)) : ProbUtil.cdfFromLeft(prob, F(atStrike))
    # theta = getGreeks(lo).theta - getGreeks(sho).theta
    # @show ret risk winRat
    # winRat *= 2
    kel = Kelly.simple(winRat, F(ret), F(risk))
    # println("Kelcalc: $(kel)   $(winRat), $(ret), $(risk)")
    return kel
end

function calcWinRat(prob, sections)
    cdfPrev = 0.0
    return reduce(sections; init=0.0) do a::Float64, s
        cdf = ProbUtil.cdfFromLeft(prob, s.x2)
        r = s.y > 0.0 ? a + (cdf - cdfPrev) : a
        # @show a, r, s.x2, s.y, cdfPrev, cdf
        cdfPrev = cdf
        # println("$(s) $(r)");
        return r
    end
end

blog(args...) = LogUtil.logit(:backtest, args...)
#endregion

#region Testing
# using SmallTypes
import SimpleStore as SS
import OptionTypes:Option
import LegTypes:Leg
using OptionMetaTypes
probFor(ts::DateTime, xpir::Date) = ProbKde.probToClose(F(SS.curpFor(ts)), F(HistData.vixOpen(Date(ts))), ts, xpir)
function testLms()
    ts = DateTime("2022-01-10T16:30:00")
    o1 = Option(Style.put, Date("2022-01-19"), 400.000)
    o2 = Option(Style.put, Date("2022-01-19"), 430.000)
    o3 = Option(Style.put, Date("2022-01-19"), 434.000)
    return (
        LegQuoteOpen(SS.quoteFor(ts, o1), Side.long),
        LegQuoteOpen(SS.quoteFor(ts, o2), Side.long),
        LegQuoteOpen(SS.quoteFor(ts, o3), Side.short)
        # LegQuoteOpen(Leg(o1, 1.0, Side.long), getQuote(SS.quoteFor(ts, o1)), OptionMeta()),
        # LegQuoteOpen(Leg(o2, 1.0, Side.long), getQuote(SS.quoteFor(ts, o2)), OptionMeta()),
        # LegQuoteOpen(Leg(o3, 1.0, Side.short), getQuote(SS.quoteFor(ts, o3)), OptionMeta())
    )
end
testCalcWinRate(tradeOpen) = testCalcWinRate(tradeOpen.ts, tradeOpen.lms)
function testCalcWinRate(ts, lms)
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
