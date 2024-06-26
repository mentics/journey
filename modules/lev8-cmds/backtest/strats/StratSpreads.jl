module StratSpreads
using Dates
using SH, BaseTypes, SmallTypes, BackTypes, LegQuoteTypes
using LogUtil, OutputUtil, BacktestUtil, CollUtil, DateUtil, ThreadUtil
import DateUtil:timult,calcRate
import ChainUtil as ch
using Pricing
import ProbKde, ProbUtil, Kelly, HistData
import LinesLeg as LL
using Scoring

# TODO: updating margin is probably using the close mid instead of the open mid when choosing between short and long
# TODO: try using delta vix in choosing between high/low findEntry

# using ThreadPools, Combinatorics, OptionQuoteTypes

macro deb(exs...)
    return
    # prblk = Expr(:call, (esc(LogUtil.logit)))
    # LogUtil.inner((:backtest, exs...), prblk)
    # return Expr(:block, prblk)
end

#region Types
struct Cand{N}
    lms::NTuple{N,LegQuoteOpen}
    score::Float64
    neto::Float64
    margin::Sides{Float64}
    scoring::ScoreData
end

Base.@kwdef struct Params
    balInit::PT
    MaxMarginPerTradeRat::Float64
    MaxQtyPerTrade::Float64
    InitTakeRate::Float64
    FinalTakeRate::Float64
    MaxWidthRat::Float64
    ProbMin::Float64
    MinProfit::PT
    MinRate::Float64
    MinKel::Float64
    MoneyValueAPR::Float64
    ExpDurRatioAvg::Float64
    MinXpirBdays::Int
    MaxXpirBdays::Int
    MinMoveRat::Float64
    MaxQtyPerMove::Float64
    PriceAdjust::Float64
    VixThreshold::Float64
end

const NUM_LEGS4 = 4

#     Params(
#         balInit = C(1000),
#         MaxMarginPerTradeRat = 0.08,
#         MaxQtyPerTrade = 100, # 21 losses at 100; 21 losses at 1; 21 losses at 10
#         InitTakeRate = 0.6, # 0.8
#         FinalTakeRate = -0.4,
#         MaxWidthRat = 0.032,
#         ProbMin = 0.976, # 0.87,
#         MinProfit = 0.07,
#         MinRate = 0.4,
#         MoneyValueAPR = 0.05,
#         ExpDurRatioAvg = 0.5,
#         MinXpirBdays = 1,
#         MaxXpirBdays = 48),
# Summary 2018-01-02 - 2022-09-30 (ran 1195 bdays): (wins = 1014, losses = 77)
#   bal = 3759.21, balReal = 3759.21, rpnl = 2759.21
#   overall realized rate: 0.32219
#   trade rate mean: 3.76522
#   trade rate median: 0.96675


# Params(
#     balInit = C(1000),
#     MaxMarginPerTradeRat = 0.08,
#     MaxQtyPerTrade = 100,
#     InitTakeRate = 0.6, # 0.8
#     FinalTakeRate = -0.4,
#     MaxWidthRat = 0.02,
#     ProbMin = 0.99, # 0.87,
#     MinProfit = 0.07,
#     MinRate = 0.4,
#     MoneyValueAPR = 0.05,
#     ExpDurRatioAvg = 0.5,
#     MinXpirBdays = 1,
#     MaxXpirBdays = 48,
#     MinMoveRat = 0.0025,
#     MaxQtyPerMove = 100,
# ),
# 1165.542947 seconds (14.83 G allocations: 2.135 TiB, 36.36% gc time)
# Summary 2018-01-02 - 2022-09-30 (ran 1195 bdays): (wins = 1745, losses = 113)
#   bal = 5787.82, balReal = 5877.82, rpnl = 4877.82
#   overall realized rate: 0.45293
#   trade rate mean: 3.8906
#   trade rate median: 1.04132

function makeStrat()
    s = TStrat2(
        (NUM_LEGS4, ScoreData),
        Params(
            balInit = C(1000),
            MaxMarginPerTradeRat = 0.02,
            MaxQtyPerTrade = 100,
            InitTakeRate = 0.4, # 0.8
            FinalTakeRate = -0.2,
            MaxWidthRat = 0.04,
            ProbMin = 0.95, # 0.87,
            MinProfit = 0.1,
            MinRate = 0.4,
            MinKel = 0.2,
            MoneyValueAPR = 0.05,
            ExpDurRatioAvg = 0.5,
            MinXpirBdays = 1,
            MaxXpirBdays = 8,
            MinMoveRat = 0.0025, # 0.00125
            MaxQtyPerMove = 100,
            PriceAdjust = -0.01,
            VixThreshold = 30,
        ),
        makeCtx(),
    )
    reseTStrat2(s)
    return s
end

struct Context2{N}
    bufCandLong::Vector{Cand{N}}
    bufCandShort::Vector{Cand{N}}
    curpPrev::Ref{Currency}
    vixPrev::Ref{Float64}
    vixDelta::Ref{Float64}
    qtyPerMove::Ref{Float64}
end

struct TStrat2 <: Strat
    acctTypes::Tuple{Int,DataType}
    params::Params
    ctx::Context2
end

BackTypes.hasMultExpirs(::TStrat2) = false

makeCtx() = Context2(Vector{Cand{NUM_LEGS4}}(), Vector{Cand{NUM_LEGS4}}(), Ref(CZ), Ref(0.0), Ref(0.0), Ref(0.0))
#endregion

#region InterfaceImpl
function reseTStrat2(s::TStrat2)
    s.ctx.curpPrev[] = CZ
    s.ctx.qtyPerMove[] = 0.0
end

function filterXpirs(xpirs, fromDate, params)
    starti = CollUtil.gtee(xpirs, bdaysAfter(fromDate, params.MinXpirBdays))
    endi = CollUtil.gtee(xpirs, bdaysAfter(fromDate, params.MaxXpirBdays))
    return xpirs[starti:endi]
end

function (s::TStrat2)(ops, tim, chain, otoq, vix)::Nothing
    # println("Running StratButter for ", tim.ts)
    params = s.params
    curp = ch.getCurp(chain)
    curpPrev = s.ctx.curpPrev
    if abs(curp - curpPrev[]) < params.MinMoveRat * curp
        if s.ctx.qtyPerMove[] >= params.MaxQtyPerMove
            return
        end
    else
        curpPrev[] = CZ # set it far away from curp so can open trades freely
        s.ctx.qtyPerMove[] = 0.0
    end
    fromPrice = curp

    # min(ops.marginAvail()) > 0 || return

    keeps = ( put=s.ctx.bufCandLong, call=s.ctx.bufCandShort )
    empty!(keeps.put)
    empty!(keeps.call)
    if vix != s.ctx.vixPrev[]
        s.ctx.vixDelta[] = vix - s.ctx.vixPrev[]
        s.ctx.vixPrev[] = vix
    end

    xpirs = filterXpirs(ch.getXpirs(chain), tim.date, params)
    for xpir in xpirs
        tmult = timult(tim.date, xpir)
        searchLeft = ch.toSearch(curp, chain.xsoqs[xpir].put)
        searchRight = ch.toSearch(curp, chain.xsoqs[xpir].call)
        # TODO: multithreaded precalc all the probs and put in a lookup, and store in module cache
        prob = ProbKde.probToClose(F(fromPrice), vix, tim.ts, xpir)
        # TODO: try both and keep best score. tried once and didnt' look that good.
        if vix > params.VixThreshold || s.ctx.vixDelta[] > 0
            # println("Vix: $vix > thresh: $(params.VixThreshold) OR delta: $(s.ctx.vixDelta[]) > 0 ; ", (;vix, threshold=params.VixThreshold, vixPrev=x.ctx.vixPrev[], delta=s.ctx.vixDelta))
            println((;vix, threshold=params.VixThreshold, vixPrev=s.ctx.vixPrev[], delta=s.ctx.vixDelta[]))
            findEntryHigh!(keeps.put, params, prob, searchLeft, searchRight, ops.canOpenPos, curp, tmult)
        else
            # findEntryLow!(keeps, params, prob, searchLeft, searchRight, ops.canOpenPos, curp, tmult)
        end
    end

    handleKeep!(s, params, ops, tim, otoq, curp, curpPrev, keeps.put)
    handleKeep!(s, params, ops, tim, otoq, curp, curpPrev, keeps.call)
    # if !isempty(keep)
    #     # println("Found $(length(keep)) candidates for expirs: ", join(map(x->getExpir(x.lms), keep), ','))
    #     # TODO: identify if new opportunity is better than currently open one
    #     if (closeEarlyForMargin(params, tim.ts, ops, otoq))
    #         x = keep[1]
    #         multiple = max(1, qtyForMargin(params, ops.bal(), ops.marginAvail(), x.margin, x.scoring.kel))
    #         if multiple > 0
    #             @assert getExpir(x.lms[1]) <= getExpir(x.lms[2])
    #             ops.openTrade(tim.ts, x.lms, toPT(x.neto, RoundDown), toPT(x.margin), multiple, "best score $(rd5(x.score))", x.scoring)
    #             curpPrev[] = curp
    #             s.ctx.qtyPerMove[] += multiple
    #         else
    #             println("0 multiple found, odd.")
    #         end
    #     end
    # else
    #     # println("No entry");
    # end
    # # global kkeeps = keeps
    return
end

function handleKeep!(s, params, ops, tim, otoq, curp, curpPrev, keep)
    if !isempty(keep)
        # println("Found $(length(keep)) candidates for expirs: ", join(map(x->getExpir(x.lms), keep), ','))
        # TODO: identify if new opportunity is better than currently open one
        if (closeEarlyForMargin(params, tim.ts, ops, otoq))
            x = keep[1]
            multiple = max(1, qtyForMargin(params, ops.bal(), ops.marginAvail(), x.margin, x.scoring.kel))
            if multiple > 0
                @assert getExpir(x.lms[1]) <= getExpir(x.lms[2])
                ops.openTrade(tim.ts, x.lms, toPT(x.neto, RoundDown), toPT(x.margin), multiple, "best score $(rd5(x.score))", x.scoring)
                curpPrev[] = curp
                s.ctx.qtyPerMove[] += multiple
            else
                println("0 multiple found, odd.")
            end
        end
    else
        # println("No entry");
    end
end
# BaseTypes.toPT(sides::Sides{Float64})::Sides{PT} = Sides(toPT(sides.long, RoundDown), round(toPT(sides.short, RoundDown)))

BackTypes.pricingOpen(::TStrat2, lmso::NTuple{N,LegQuoteOpen}) where N = toPT(price_open(lmso))
BackTypes.pricingClose(::TStrat2, lmsc::NTuple{N,LegQuoteClose}) where N = toPT(price_close(lmsc))
price_open(lms) = Pricing.price(lms, false)
price_close(lms) = Pricing.price(lms, true)
# calcPrice(lms)::PT = toPT(Pricing.price(lms)) # toPT(bap(lms, 0.0)) + P(0.02)
# calcPriceFast(lms)::Float64 = Pricing.price(lms) # Pricing.bapFast(lms, 0.0) + 0.02

function BackTypes.checkExit(params::Params, tradeOpen, tim, lmsc, curp)::Union{Nothing,String}
    dateOpen = getDateOpen(tradeOpen)
    dateOpen < tim.date || return # No closing on the same day
    netc = price_close(lmsc)
    curVal = getNetOpen(tradeOpen) + netc
    rate = calcRate(dateOpen, tim.date, curVal, getRisk(tradeOpen))
    # println("checkexit rate: ", rate)
    # @show dateOpen, tim.date, curVal, getRisk(tradeOpen)

    # rateOrig = tradeOpen.extra.rate
    # blog("checkExit: $(tradeOpen.id) $(round(rate;digits=5)) $(round(rateOrig;digits=5))")
    # if rate >= 1.5 * rateOrig
    # if rate >= 0.4 || rate >= 1.5 * rateOrig
    #     return "rate $(rate) >= 0.4 or 1.5 * $(rateOrig)"
    # end
    ratio = trad.targetRatio(tradeOpen, tim.date)
    if rate >= params.FinalTakeRate + ratio * (params.InitTakeRate - params.FinalTakeRate)
        # Avoid exiting on last day when would be better to expire
        if rate >= trad.calcRateOrig(tradeOpen) || !(tim.date == trad.getTargetDate(tradeOpen) && curp > getStrike(tradeOpen.lms[end]) + 2)
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
        cv = CollUtil.findMaxDom(firstNinf, Iterators.map(t -> trad.calcCloseInfo(t, ts, otoq, toPT ∘ price_close), ops.tradesOpen()))
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
function scoreHigh(lms, params, prob, curp, tmult)
    return Scoring.score_condor_short(prob, curp, tmult, lms; params)
end

function scoreLow(lms, params, prob, curp, tmult)
    return Scoring.score_condor_long(prob, curp, tmult, lms; params)
end

function findEntryHigh!(keep, params, prob, searchLeft, searchRight, canOpenPos, args...)
    # TODO: test which mid works better
    mid = prob.center
    # mid = probFromLeft(prob, .5)

    # strikeMin = CZ # , _ = ProbUtil.probFromLeft(prob, params.ProbMin/2)
    # strikeMax = C(10000.0) # , _ = ProbUtil.probFromRight(prob, params.ProbMin/2)
    # # oqs = ch.oqsBetween(search, strikeMin, strikeMax)
    # # @show mid (strikeMin, 1.01*mid) (0.99 * mid, strikeMax)
    # # oqsLeft = ch.oqsBetween(searchLeft, strikeMin, 1.01 * mid)
    # # oqsRight = ch.oqsBetween(searchRight, 0.99 * mid, strikeMax)
    strikeMin = .98 * mid
    strikeMax = 1.02 * mid
    oqsLeft = ch.oqsBetween(searchLeft, strikeMin, strikeMax)
    oqsRight = ch.oqsBetween(searchRight, strikeMin, strikeMax)
    global keepProb = prob
    if length(oqsLeft) < 2 || length(oqsRight) < 2
        blog("WARN: High oqs < $(NUM_LEGS4)")
        return nothing
    end
    maxWidth = params.MaxWidthRat * prob.center

    count = 0
    lastiLeft = lastindex(oqsLeft)
    lastiRight = lastindex(oqsRight)
    # for i1 in eachindex(oqs)[1:end-2]
    for i1 in 1:(lastiLeft-1)
        oq1 = oqsLeft[i1]
        # TODO: optimize this check: can maintain a lookup or all that would conflict and check with hashmap
        canOpenPos(getOption(oq1), Side.short) || continue
        str1 = getStrike(oq1)
        # for i2 in i1+1:lasti
        for i2 in (i1+1):lastiLeft
            oq2 = oqsLeft[i2]
            str2 = getStrike(oq2)
            str2 - str1 <= maxWidth - 2 || break
            canOpenPos(getOption(oq2), Side.long) || continue
            # for i3 in i2+1:lasti
            for i3 in 1:(lastiRight-1)
                oq3 = oqsRight[i3]
                str3 = getStrike(oq3)
                str3 >= str2 || continue
                str3 - str1 <= maxWidth - 1 || break
                canOpenPos(getOption(oq3), Side.long) || continue
                for i4 in (i3+1):lastiRight
                    oq4 = oqsRight[i4]
                    canOpenPos(getOption(oq4), Side.short) || continue
                    str4 = getStrike(oq4)
                    str4 - str1 <= maxWidth || break
                    # @show (oq1, oq2, oq3, oq4)

                    lms = CollUtil.sortuple(x -> getStrike(x) + (isCall(x) ? eps(Currency) : 0.0), LegQuoteOpen(oq1, Side.short), LegQuoteOpen(oq2, Side.long), LegQuoteOpen(oq3, Side.long), LegQuoteOpen(oq4, Side.short))
                    if mid - str1 <= 4 && str4 - mid <= 4
                        # println("found new")
                        global checkLms = lms
                    end
                    r = scoreHigh(lms, params, prob, args...)
                    count += 1
                    if !isnothing(r) && (isempty(keep) || r.score > keep[end].score)
                        # TODO: not optimized
                        score, neto, margin, data = r
                        push!(keep, Cand(lms, score, neto, margin, data))
                        sort!(keep; rev=true, by=x->x.score)
                        length(keep) <= 10 || pop!(keep)
                    end
                end
            end
        end
    end
    @blog "High scored $count candidates"
    return
end

function findEntryLow!(keeps, params, prob, searchLeft, searchRight, canOpenPos, args...)
    findSpreadPut!(keeps.put, params, prob, searchLeft, canOpenPos, args...)
    findSpreadCall!(keeps.call, params, prob, searchRight, canOpenPos, args...)
    return
end

function findSpreadPut!(keep, params, prob, searchLeft, canOpenPos, args...)
    mid = prob.center
    strikeLow, _ = ProbUtil.probFromRight(prob, params.ProbMin)
    oqsLeft = ch.oqsBetween(searchLeft, 0, strikeLow)
    if length(oqsLeft) < 2
        blog("WARN: findSpreadPut! insufficient oqs < $(NUM_LEGS4): $mid: $strikeLow")
        return nothing
    end
    maxWidth = params.MaxWidthRat * prob.center / 2

    count = 0
    lastiLeft = lastindex(oqsLeft)
    for i1 in 1:(lastiLeft-1)
        oq1 = oqsLeft[i1]
        # TODO: optimize this check: can maintain a lookup or all that would conflict and check with hashmap
        canOpenPos(getOption(oq1), Side.long) || continue
        str1 = getStrike(oq1)
        for i2 in (i1+1):lastiLeft
            oq2 = oqsLeft[i2]
            str2 = getStrike(oq2)
            str2 - str1 <= maxWidth || break
            canOpenPos(getOption(oq2), Side.short) || continue

            lms = CollUtil.sortuple(x -> getStrike(x) + (isCall(x) ? eps(Currency) : 0.0), LegQuoteOpen(oq1, Side.long), LegQuoteOpen(oq2, Side.short))
            r = scoreLow(lms, params, prob, args...)
            count += 1
            if !isnothing(r) && (isempty(keep) || r.score > keep[end].score)
                # TODO: not optimized
                score, neto, margin, data = r
                push!(keep, Cand(lms, score, neto, margin, data))
                sort!(keep; rev=true, by=x->x.score)
                length(keep) <= 10 || pop!(keep)
            end
        end
    end
    # @blog "findSpreadPut! scored $count candidates"
    return
end

function findSpreadCall!(keep, params, prob, searchRight, canOpenPos, args...)
    mid = prob.center
    strikeHigh, _ = ProbUtil.probFromLeft(prob, params.ProbMin)
    oqsRight = ch.oqsBetween(searchRight, strikeHigh, 1e9)
    if length(oqsRight) < 2
        blog("WARN: findSpreadCall! insufficient oqs < $(NUM_LEGS4): $mid: $strikeHigh")
        return nothing
    end
    maxWidth = params.MaxWidthRat * prob.center / 2

    count = 0
    lastiRight = lastindex(oqsRight)
    for i3 in 1:(lastiRight-1)
        oq3 = oqsRight[i3]
        str3 = getStrike(oq3)
        canOpenPos(getOption(oq3), Side.short) || continue
        for i4 in (i3+1):lastiRight
            oq4 = oqsRight[i4]
            str4 = getStrike(oq4)
            str4 - str3 <= maxWidth || break
            canOpenPos(getOption(oq4), Side.long) || continue

            lms = CollUtil.sortuple(x -> getStrike(x) + (isCall(x) ? eps(Currency) : 0.0), LegQuoteOpen(oq3, Side.short), LegQuoteOpen(oq4, Side.long))
            r = scoreLow(lms, params, prob, args...)
            count += 1
            if !isnothing(r) && (isempty(keep) || r.score > keep[end].score)
                # TODO: not optimized
                score, neto, margin, data = r
                push!(keep, Cand(lms, score, neto, margin, data))
                sort!(keep; rev=true, by=x->x.score)
                length(keep) <= 10 || pop!(keep)
            end
        end
    end
    # @blog "findSpreadCall! scored $count candidates"
    return
end
#endregion

#region Util
function qtyForMargin(params, bal, avail, marginTrade, kel_raw)::Int
    kel = min(1.0, kel_raw)
    maxMargin = params.MaxMarginPerTradeRat * bal
    # @show maxMarginPerTrade avail marginTrade kel
    long = marginTrade.long > 0 ? qtyForAvail(avail.long, marginTrade.long, kel) : params.MaxQtyPerTrade
    short = marginTrade.short > 0 ? qtyForAvail(avail.short, marginTrade.short, kel) : params.MaxQtyPerTrade
    margin = max(marginTrade)
    @assert margin > 0
    qty = min(long, short, floor(Int, F(maxMargin / margin)))
    # println("qty for margin: $((;avail, maxMarginPerTrade, margin, kel, qty, long, short, fromMax=round(Int, F(maxMarginPerTrade / margin), RoundDown)))")
    if qty > 0 && (avail.long < 0 || avail.short < 0)
        @show maxMargin avail marginTrade kel long short margin qty
        error("qtyformargin not working")
    end
    return floor(Int, min(params.MaxQtyPerTrade, qty))
end
# qtyForAvail(avail, risk, kel)::Int = round(Int, kel * avail / risk, RoundDown)
function qtyForAvail(avail, risk, kel)::Int
    if kel > 100 || avail > 100000 || risk < 0.001
        s = @str "ERROR: qtyForAvail args" avail risk kel
        println(s)
        @blog(s)
    end
    # TODO: was .5 kel
    res = floor(Int, 0.25 * kel * avail / risk)
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
