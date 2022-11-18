module Backtests
using Dates
using Globals, BaseTypes, SmallTypes, OptionTypes, LegMetaTypes
using DateUtil, CollUtil, DictUtil
using Expirations, Markets, Snapshots, Chains
import SH
import LegTypes
import SnapUtil
import OptionUtil

export bt
const bt = @__MODULE__

# TODO: use historical data for VIX to fill in VIX when not in Markets

function run(f)
    global acct = Dict(
        :bal => C(0.0),
        :real => C(0.0),
        :trades => Vector{Dict{Symbol,Any}}(),
        :poss => Vector{Dict{Symbol,Any}}(),
        :todayOpens => Vector{Dict{Symbol,Any}}(),
        :todayCloses => Vector{Dict{Symbol,Any}}()
    )
    num = SnapUtil.countSnaps()
    datePrev = Date(0)
    # max = 100
    # for i in num:-1:(num-max)
    # for i in min(num, max):-1:1
    for i in num:-1:1
        snap(i)
        mkt = market()
        ts = mkt.tsMarket
        date = toDateMarket(ts)
        if date != datePrev
            for trade in acct[:poss]
                xpir = getExpir(trade)
                if xpir < date
                    lmsc = requote(optQuoter, trade[:lms], Action.close)
                    closeTrade(acct, trade, lmsc, netClose(trade), "expired")
                end
            end
            empty!(acct[:todayOpens])
            empty!(acct[:todayCloses])
            datePrev = date
        end

        println("Start $(date) SPY: $(mkt.curp), VIX: $(mkt.vix)")
        acct[:ts] = ts
        acct[:date] = date
        acct[:xpirs] = SnapUtil.snapExpirs(snap())
        acct[:delta] = isempty(acct[:poss]) ? 0.0 : calcDelta(acct[:poss])
        acct[:deltas] = isempty(acct[:poss]) ? Dict{Date,Float64}() : calcDeltas(acct[:poss])
        acct[:gamma] = isempty(acct[:poss]) ? 0.0 : calcGamma(acct[:poss])
        acct[:gammas] = isempty(acct[:poss]) ? Dict{Date,Float64}() : calcGammas(acct[:poss])
        f(acct)
        println("End $(date): $(acct[:bal]) real:$(acct[:real]) delta:$(rond(acct[:delta]))")
    end
end

calcDelta(trade::Dict) = getDelta(requote(optQuoter, trade[:lms], Action.close))
calcDelta(trades::Coll) = sum(calcDelta, trades)
calcGamma(trade::Dict) = getGamma(requote(optQuoter, trade[:lms], Action.close))
calcGamma(trades::Coll) = sum(calcGamma, trades)

function calcDeltas(trades::Coll)
    d = Dict{Date,Float64}()
    for trade in trades
        addToKey(d, trade[:targetDate], calcDelta(trade))
    end
    return d
end

function calcGammas(trades::Coll)
    d = Dict{Date,Float64}()
    for trade in trades
        addToKey(d, trade[:targetDate], calcGamma(trade))
    end
    return d
end

import Joe
import Quoting:requote
function strat1(acct)
    # ts = acct[:ts]
    RateMin = 0.5
    MaxDelta = 0.3
    date = acct[:date]
    for trade in acct[:poss]
        dateOpen = toDateMarket(trade[:tsOpen])
        dateOpen < date || continue         # don't close today's entries
        xpir = getExpir(trade)
        daysLeft = bdays(date, xpir)
        daysTotal = bdays(dateOpen, xpir)
        xpirRatio = (1 + daysLeft) / daysTotal
        neto = trade[:neto]
        lmsc = requote(optQuoter, trade[:lms], Action.close)
        # TODO: maybe use lmsc instead of quoting again?
        qt = quoter(trade[:lms], Action.close)
        netc = usePrice(qt)
        curVal = neto + netc
        theta = getTheta(lmsc)
        # if curVal > 0.0 || daysLeft <= 2 || theta < -0.005
            timt = DateUtil.timult(date, dateOpen)
            mn = min(OptionUtil.legsExtrema(trade[:lms]...)...)
            rate = timt * curVal / (-mn)
            label = nothing
            if rate >= (xpirRatio * RateMin)
                label = "rate: $(rond(rate))"
            # elseif daysLeft <= 2
            #     label = "daysLeft"
            # elseif theta < -0.015
            #     label = "theta: $(rond(theta))"
            # elseif curVal <= -trade[:met].mx
            #     label = "loss < -max profit: $(curVal) <= $(-trade[:met].mx)"
            end
            if !isnothing(label)
                closeTrade(acct, trade, lmsc, netc, label)
            end
        # end
    end

    isLegAllowed = entryFilterOption(acct)
    for xpir in first(acct[:xpirs], 21)
        xpir > bdaysAfter(date, 3) || continue
        posLms = collect(Iterators.filter(x -> SH.getExpiration(x) == xpir, cu.flatmap(x -> x[:lms], acct[:poss])))
        res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms, all=true)
        # res, ctx = Joe.runJorn(xpir, isLegAllowed; nopos=true, posLms)
        !isempty(res) || continue
        deltaAcct = acct[:delta]
        deltaXpir = get(acct[:deltas], xpir, 0.0)
        filter!(res) do r
            deltaAdd = getDelta(r.lms)
            return r.roiEv >= 0.1 &&
                getTheta(r.lms) >= 0.0 &&
                (
                    abs(deltaAcct + deltaAdd) < MaxDelta ||
                    abs(deltaAcct + deltaAdd) < abs(deltaAcct)
                ) &&
                (
                    abs(deltaXpir + deltaAdd) < MaxDelta ||
                    abs(deltaXpir + deltaAdd) < abs(deltaXpir)
                )
        end
        !isempty(res) || continue
        # sort!(res; rev=true, by=r -> r.roiEv)
        sort!(res; rev=true, by=r -> r.met.prob)
        r = res[1]
        # if r.roiEv > 0.2
            trade = openTrade(acct, r.lms, netOpen(r.lms), "joe roiEv=$(rond(r.roiEv))")
            trade[:met] = r.met
        # end
    end
end

#region Actions
function openTrade(acct, lms, neto, label)
    trade = Dict(
        :id => nextTradeId(acct),
        :lms => lms,
        :neto => neto,
        :tsOpen => acct[:ts],
        :labelClose => label,
        :targetDate => minimum(SH.getExpiration, lms)
    )
    push!(acct[:trades], trade)
    push!(acct[:poss], trade)
    push!(acct[:todayOpens], trade)
    acct[:bal] += neto
    delta = getDelta(lms)
    acct[:delta] += delta
    deltaNew = addToKey(acct[:deltas], trade[:targetDate], delta)
    println("Open: '$(label)' $(trade[:id]) for $(neto) target=$(getExpir(trade)) deltaX: $(rond(delta)) -> $(rond(deltaNew)) deltaAcct: $(rond(acct[:delta]))")
    return trade
end
function closeTrade(acct, trade, lmsc, netc, label)
    # NOTE: snap might have already moved when called here, so don't use anything except acct
    trade[:lmsc] = lmsc
    trade[:netc] = netc
    trade[:tsClosed] = acct[:ts]
    trade[:labelClose] = label
    push!(acct[:todayCloses], trade)
    cu.del!(x -> x[:id] == trade[:id], acct[:poss])
    acct[:bal] += netc
    acct[:real] += trade[:neto] + netc
    trade[:labelClose] = label
    println("Close: '$(label)' $(trade[:id]) for $(netc) pnl=$(trade[:neto] + netc) expir=$(getExpir(trade)) open for $(openDur(trade)) with days left $(bdaysLeft(acct[:date], trade))")
    return
end
nextTradeId(acct) = length(acct[:trades]) + 1
#endregion

#region Utils
rond(x::Real) = round(x; digits=4)
usePrice(q) = SH.bap(q, .2) # SH.getBid(q)
netOpen(lms) = sum(usePrice, lms)
# priceOpen(trade) = usePrice(quoter(trade[:lms], Action.open))
netClose(trade) = usePrice(quoter(trade[:lms], Action.close))
getExpir(trade) = trade[:targetDate] # minimum(SH.getExpiration, lms)
openDur(trade) = bdays(toDateMarket(trade[:tsOpen]), toDateMarket(trade[:tsClosed])) + 1
bdaysLeft(from, trade) = bdays(from, getExpir(trade))

function entryFilterOption(acct)
    d = entryFilterLookup(acct)
    return function(opt, side)
        status = get(d, SH.getOption(opt), 0)
        return !(status != 0 && (status == 2 || status != Int(side)))
    end
end

function entryFilterLookup(acct)
    # can't go opposite current poss and today's opens and closes
    global legsPos = cu.mapflatmap(SH.getLeg, x -> x[:lms], acct[:poss])
    global legsOpens = cu.mapflatmap(SH.getLeg, x -> x[:lms], acct[:todayOpens])
    global legsCloses = cu.mapflatmap(x -> LegTypes.switchSide(SH.getLeg(x)), x -> x[:lms], acct[:todayCloses])
    global d = Dict{Option,Int}()
    for leg in flat(legsPos, legsOpens, legsCloses)
        opt = SH.getOption(leg)
        side = Int(SH.getSide(leg))
        status = get(d, opt, 0)
        status2 = status == 0 ? side : (status == side ? status : 2)
        # if status2 == 2
        #     error("status == 2 ", leg, ' ', status, ' ', d[opt])
        #     # @show "How did we get status 2?" leg opt d[opt]
        # end
        d[opt] = status2
    end
    return d
end
#endregion

end