module TestStrat
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
using LogUtil, OutputUtil, BacktestUtil, CollUtil, DateUtil
import DateUtil:timult,calcRate
import ChainUtil as ch
using Pricing
import ProbKde, ProbUtil, Kelly, HistData
import LinesLeg as LL

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

struct Cand
    lms::NTuple{3,LegMetaOpen}
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

makeStrat() = TStrat(
    (3,Scoring),
    Params(C(1000), 0.084, 0.3, 0.06, 0.95, 0.02, 0.1, 0.05, 0.5, 8, 84),
    makeCtx(),
)

struct Context
    bufCandLong::Vector{Cand}
    bufCandShort::Vector{Cand}
end

struct TStrat <: Strat
    acctTypes::Tuple{Int,DataType}
    params::Params
    ctx::Context
end

BackTypes.hasMultExpirs(::TStrat) = false # true

makeCtx() = Context(Vector{Cand}(), Vector{Cand}())
#endregion

#region InterfaceImpl
function (s::TStrat)(ops, tim, chain, otoq)::Nothing
    params = s.params
    curp = ch.getCurp(chain)

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
        # extrais = max(starti-2, i * 3 ÷ 4):i
        extrais = i:i # (i-2):i
        xpirsExtra = xpirs[extrais]
        # @show starti endi extrais
        fromPrice = curp # TODO: could use recent extrema?
        tmult = timult(tim.date, xpir)
        searchLong = ch.toSearch(curp, chain.xsoqs[xpir].put)
        # searchShort = ch.toSearch(curp, chain.xsoqs[xpir].call)
        prob = ProbKde.probToClose(F(fromPrice), F(HistData.vixOpen(tim.date)), tim.ts, xpir)
        findEntry!(keepLong, params, prob, searchLong, otoq, xpirsExtra, tmult)
        # findEntryShort!(keepShort, params, prob, searchShort, tmult)
    end
    if !isempty(keepLong)
        # TODO: identify if new opportunity is better than currently open one
        if (closeEarlyForMargin(params, tim.ts, ops, otoq))
            x = keepLong[1]
            multiple = qtyForMargin(s.params.maxMarginPerTradeRat * ops.bal(), ops.marginAvail(), x.margin, x.scoring.kel)
            if multiple > 0
                @assert getExpir(x.lms[1]) <= getExpir(x.lms[2])
                ops.openTrade(tim.ts, x.lms, toPT(x.neto, RoundDown), toPT(x.margin), multiple, "best score $(rd5(x.score))", x.scoring)
            else
                println("0 multiple found, odd.")
            end
        end
    end
    if !isempty(keepShort)
        # TODO: identify if new opportunity is better than currently open one
        if (closeEarlyForMargin(params, tim.ts, ops, otoq))
            x = keepShort[1]
            multiple = qtyForMargin(s.params.maxMarginPerTradeRat * ops.bal(), ops.marginAvail(), x.margin, x.scoring.kel)
            if multiple > 0
                @assert getExpir(x.lms[3]) <= getExpir(x.lms[2])
                ops.openTrade(tim.ts, x.lms, toPT(x.neto, RoundDown), toPT(x.margin), multiple, "best score $(rd5(x.score))", x.scoring)
            else
                println("0 multiple found, odd.")
            end
        end
    end
    return
end
BaseTypes.toPT(sides::Sides{Float64})::Sides{PT} = Sides(toPT(sides.long, RoundDown), round(toPT(sides.short, RoundDown)))

BackTypes.pricingOpen(::TStrat, lmso::NTuple{3,LegMetaOpen}) = calcPrice(lmso)
BackTypes.pricingClose(::TStrat, lmsc::NTuple{3,LegMetaClose}) = calcPrice(lmsc)
function BackTypes.checkExit(params::Params, tradeOpen::TradeBTOpen{3,Scoring}, tim, lmsc, curp)::Union{Nothing,String}
    Date(tradeOpen.ts) < tim.date || return # No closing on the same day
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
        if rate >= tradeOpen.extra.rate || !(tim.date == trad.targetDate(tradeOpen) && curp > getStrike(tradeOpen.lms[3]) + 2)
            return "rate $(rd5(rate)) >= $(params.InitTakeRate) * $(ratio) ($(.4*ratio))"
        end
    end
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
function findEntry!(keep, params, prob, search, otoq, xpirsExtra, args...)
    strikeExt, _ = ProbUtil.probFromRight(prob, params.ProbMin)
    oqs = ch.oqsLteCurp(search, strikeExt)

    itr1 = Iterators.reverse(eachindex(oqs)[3:end])
    for i1 in itr1
        oq1 = oqs[i1]
        itr2 = i1-1:-1:2
        for i2 in itr2
            oq2 = oqs[i2]
            if getStrike(oq1) - getStrike(oq2) > params.MaxWidthRat * prob.center
                break
            end
            itr3 = i2-1:-1:1
            for i3 in itr3
                oq3 = oqs[i3]
                if getStrike(oq2) - getStrike(oq3) > (params.MaxWidthRat/2) * prob.center
                    break
                end
                strike = getStrike(oq3)
                for xpir in xpirsExtra
                    oq3 = ch.xssToq(otoq, xpir, Style.put, strike)
                    !isnothing(oq3) || continue
                    lms = (LegMetaOpen(oq3, Side.long), LegMetaOpen(oq2, Side.long), LegMetaOpen(oq1, Side.short))
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
    end
end

function findEntryShort!(keep, params, prob, search, args...)
    strikeExt, _ = ProbUtil.probFromLeft(prob, params.ProbMin)
    oqs = ch.oqsGteCurp(search, strikeExt)
    # @show prob.center strikeExt length(oqs)
    lasti = lastindex(oqs)
    itr1 = eachindex(oqs)[3:end]
    for i1 in itr1
        oq1 = oqs[i1]
        itr2 = i1+1:lasti-1
        for i2 in itr2
            oq2 = oqs[i2]
            if getStrike(oq2) - getStrike(oq1) > params.MaxWidthRat * prob.center
                break
            end
            # itr3 = i2:i2
            itr3 = i2+1:lasti
            for i3 in itr3
                oq3 = oqs[i3]
                if getStrike(oq3) - getStrike(oq2) > params.MaxWidthRat * prob.center
                    break
                end
                lms = (LegMetaOpen(oq1, Side.short), LegMetaOpen(oq2, Side.long), LegMetaOpen(oq3, Side.long))
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
end

function score(lms, params, prob, tmult)
    neto = calcPriceFast(lms)
    neto >= params.MinNeto || ( @deb "no score neto" neto params.MinNeto ; return nothing )
    margin = Pricing.calcMarginFloat(lms)
    risk = max(margin)
    @assert risk > CZ "risk was <= 0"
    rate = calcRate(tmult, neto, risk)
    rate >= params.MinRate || ( @deb "no score rate" rate params.MinRate ; return nothing )

    # TODO: mult xpir won't work with this kel calc
    # kel = calcKel(params, tmult, neto, risk, prob, LL.toSections(lms))
    # kel > 0 || return nothing # ( deb("kel $(kel) <= 0, $(rate), $(tmult), $(neto), $(risk)") ; return nothing )

    # score = rate * kel
    gks = getGreeks(lms)
    # TODO: could require vega > delta
    gks.theta + gks.vega > gks.delta || ( @deb "no score thetavega>delta" gks.theta gks.vega gks.delta ; return nothing )
    # score = rate * (gks.theta + gks.vega)
    # score = gks.theta + gks.vega - gks.delta
    # score = rate * (gks.theta + gks.vega - gks.delta)
    score = gks.theta + gks.vega - gks.delta
    return ((;score, neto, margin), Scoring(neto, risk, rate, 1.0))

    # Note: tested below for short entries
    # score = gks.theta + gks.delta - gks.vega
    # return ((;score, neto, margin), Scoring(neto, risk, rate, 1.0))
end
#endregion

#region Util
calcPrice(lms)::PT = toPT(bap(lms, 0.0))
calcPriceFast(lms)::Float64 = Pricing.bapFast(lms, 0.0)
function qtyForMargin(maxMarginPerTrade, avail, marginTrade, kel)::Int
    # @show maxMarginPerTrade avail marginTrade kel
    long = marginTrade.long > 0 ? qtyForAvail(avail.long, marginTrade.long, kel) : typemax(Int)
    short = marginTrade.short > 0 ? qtyForAvail(avail.short, marginTrade.short, kel) : typemax(Int)
    margin = max(marginTrade)
    @assert margin > 0
    qty = min(long, short, round(Int, F(maxMarginPerTrade / margin), RoundDown))
    if qty > 0 && (avail.long < 0 || avail.short < 0)
        @show maxMarginPerTrade avail marginTrade kel long short margin qty
        error("qtyformargin not working")
    end
    return qty
end
qtyForAvail(avail, risk, kel)::Int = round(Int, kel * avail / risk, RoundDown)

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
