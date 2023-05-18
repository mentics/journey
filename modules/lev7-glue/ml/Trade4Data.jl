module Trade4Data
using Dates
using SH, BaseTypes, SmallTypes, ChainTypes, LegMetaTypes
using BaseUtil, DateUtil, ChainUtil, CollUtil
import Pricing, Calendars
import SimpleStore as SS
import MarketHist

#=
Train two models: entry policy and exit policy.
We track: what is the best result (max profit or min loss) that could be accomplished for a candidate trade?
The loss for the entry policy is if it entered a trade that had < threshold max profit
The loss for the exit policy could be its distance below the best result which would work on both winning and losing trades. Then we can train it with losing trades, also.
We should probably try training exit policy both ways: only on potential winners, and all.

Challange for temporal things is because market days aren't continuous, we need to somehow
interpolate the underlying price for any point in time, so we can have a regularized set of returns
over the past for each observation.

    data: ts / val
0  2  5  9
5  2  9  7

desired ts / val
1   3   5   7
3.5 2  9


dur between two points:
0
0
10
5
0
curp for those two points
400.1
401.2


=#

const SCALAR = Float32

function make_data()
    # res = Array{SCALAR,2}(undef, 2, 0)
    global cands_all = Vector{Vector{SCALAR}}()

    i = 0
    SS.run(Date(2016,1,4), Date(2016,1,5); maxSeconds=1) do tim, chain
        # println("processing: ", tim.ts)
        ts = tim.ts
        i += 1
        curp = ChainUtil.getCurp(chain)
        curp_hist = map(2.0 .^ (3:10)) do tex_ago
            log(curp / MarketHist.curp_for_tex(tim.ts, tex_ago))
        end
        vix_hist =  map(2.0 .^ (3:10)) do tex_ago
            # log(MarketHist.vix_for_tex(ts, tex_ago))
            MarketHist.vix_for_tex(ts, tex_ago)
        end
        atm = ChainUtil.calc_atm(chain)
        temporal = makeTemporal(tim)
        cands = proc_ts(ts, chain)

        parts = (length(cands[1]), length(curp_hist), length(vix_hist), length(atm), length(temporal))
        ind = 1
        global inds = []
        for part in parts
            push!(inds, ind:(ind + part - 1))
            ind += part
        end
        for cand in cands
            push!(cands_all, SCALAR[
                cand...
                curp_hist...
                vix_hist...
                atm...
                temporal...
            ])
        end
        println("Spec:\n", "cand, curp_hist, vix_hist, atm, temporal\n", join(length.((first(cands), curp_hist, vix_hist, atm, temporal)), ", "))
        println("Added $(length(cands)) cands for ts $ts")
    end
 # TODO: change to matrix
    return cands_all
end

function makeTemporal(tim)
    return (
        SCALAR(dayofweek(tim.date) / 7),
        SCALAR(dayofmonth(tim.date) / daysinmonth(tim.date)),
        SCALAR(dayofquarter(tim.date) / daysinquarter(tim.date)),
        SCALAR(dayofyear(tim.date) / Dates.daysinyear(tim.date)),
        SCALAR(hour(tim.ts) / 24),
    )
end

SH.getStrike(x::Real) = x # This is because searchsorted* calls by on the search object
function proc_ts(ts::DateTime, chain::ChainInfo)
    curp = ChainUtil.getCurp(chain)
    xpirs = ChainUtil.getXpirs(chain)
    xpir = CollUtil.gteev(xpirs, Date(ts + Day(1)))
    @assert xpir > ts "Invalid xpir $xpir for ts $ts"
    soqs = chain.xsoqs[xpir]
    call_left = searchsortedfirst(soqs.call, curp - 4.0; by=getStrike)
    calls = @view soqs.call[call_left:end]
    put_right = searchsortedlast(soqs.put, curp + 4.0; by=getStrike)
    puts = @view soqs.put[1:put_right]

    # call_prices, put_prices = prices(calls, puts)
    # @assert length(call_prices) == length(calls)
    # @assert length(put_prices) == length(puts)

    res = []
    proc = (lms, neto) -> push!(res, proc_lms(lms, neto, ts, curp))
    make_condors_long(proc, calls, puts) # , call_prices, put_prices)
    # proc_condors_short(proc, puts, calls, call_prices, put_prices)
    return res
end

# function prices(calls, puts)
#     call_prices = map(oq -> Sides(Pricing.price_long_flip(oq), Pricing.price_short(oq)), calls)
#     put_prices = map(oq -> Sides(Pricing.price_long_flip(oq), Pricing.price_short(oq)), puts)
#     return call_prices, put_prices
# end

# function proc_condors_short(puts, put_prices, calls, call_prices)
# end

function proc_lms(lms, neto, ts, curp)
    texp = Calendars.calcTex(ts, Calendars.getMarketClose(getExpir(lms)))
    risk = getRisk(lms)
    return (
        make_label(ts, lms, neto, risk)...,
        texp,
        neto,
        risk,
        enc_lms(lms, curp)...,
    )
end

enc_lms(lms, curp) = Iterators.flatten(map(lm -> enc_lm(lm, curp), lms))

function enc_lm(lm, curp)
    side = Int(getSide(lm))
    q = getQuote(lm)
    spread = q.ask - q.bid
    return (
        SCALAR(Int(getStyle(lm))),
        SCALAR(log(getStrike(lm) / curp)),
        SCALAR(side),
        SCALAR(Pricing.price(lm) * side / curp), # TODO: consider dividing by curp
        SCALAR(spread / curp), # TODO: consider dividing by curp
        # SCALAR(getQuantity(lm)),
        # TODO: flatten get meta
        SCALAR.(LegMetaTypes.metaToFlat(lm))...,
    )
end

# TODO: move to where it belongs
import LinesLeg as LL
SH.getRisk(lms::Coll{<:LegMeta}) = -Pricing.calcCommit(LL.toSegments(Tuple(lms)))
# SH.getProfit(lms::Coll{<:LegMeta}) = -Pricing.calcCommit(LL.toSegments(Tuple(lms)))
function DateUtil.calcRate(from::DateTime, to::DateTime, ret, risk)::Float64
    texy = Calendars.texToYear(Calendars.calcTex(from, to))
    return ret / Float64(risk) / texy
end

function make_label(ts_start, lms::NTuple, neto, risk)
    # xpirts = Calendars.getMarketClose(getExpir(lms))
    ts_last = SS.last_ts_for(getExpir(lms))
    otoqs = SS.chains_for(SS.first_ts_for(bdaysAfter(Date(ts_start), 1)), ts_last)
    # println(length(otoqs))
    # error(typeof(otoqs))
    rate_min, rate_max = (typemax(Float64), typemin(Float64))
    price_min, price_max = (typemax(Currency), typemin(Currency))
    # TODO: store a separate vector of: ts_start, lms, neto, risk, rate that can be added to the common fields in make_data to be used to train exit
    map(otoqs) do (ts, otoq)
        lms_req = requote(otoq, lms)
        # !isnothing(lms_req) || return # sometimes it can't quote things
        try
            p = Pricing.price(lms_req, true)
            # !isnothing(p) || return
            if p < price_min
                price_min = p
                # TODO: consider tex based rate?
                r = calcRate(ts_start, ts, neto + p, risk)
                if r < rate_min
                    rate_min = r
                end
            end
            if p > price_max
                price_max = p
                r = calcRate(ts_start, ts, neto + p, risk)
                if r > rate_max
                    rate_max = r
                end
            end
        catch e
            global kts = ts
            global klms = lms
            global kotoq = otoq
            global klms_req = lms_req
            # println(e)
            if !SS.is_ts_eod(ts)
                # TODO: investigate?
                # error("expected eod ts $ts: ", (; xpir=getExpir(lms), ts_last=ts_last))
            end
            # println("WARN: skipping")
            # rethrow(e)
        end
    end
    try
        checkrates(rate_min, rate_max, price_min, price_max)
    catch e
        global kts_start = ts_start
        global klms = lms
        # global kotoq = otoq
        @show length(collect(otoqs))
        rethrow(e)
    end
    otoq = SS.chain_for(ts_last)
    price_last = find_price_last(otoq, lms, ts_last)
    rate_end = calcRate(ts_start, ts_last, neto + price_last, risk)
    return rate_min, rate_max, rate_end
end

function checkrates(rate_min, rate_max, price_min, price_max)
    if rate_min > 1000
        error("checkrates ", join((rate_min, rate_max, price_min, price_max), ", "))
    elseif rate_max < -1000
        error("checkrates ", join((rate_min, rate_max, price_min, price_max), ", "))
    elseif price_min > 100000
        error("checkrates ", join((rate_min, rate_max, price_min, price_max), ", "))
    elseif price_max <= -100000
        error("checkrates ", join((rate_min, rate_max, price_min, price_max), ", "))
    end
end

function find_price_last(otoq_last, lms, ts_last)
    price_last = reprice(otoq_last, lms)
    ts = ts_last
    otoq = otoq_last
    count = 0
    while isnothing(price_last)
        ts = SS.ts_prev(ts)
        otoq = SS.chain_for(ts)
        price_last = reprice(otoq, lms)
        count += 1
    end
    if count > 4
        global klms = lms
        global kotoq = otoq_last
        global kts = ts_last
        hours = (ts - ts_last) / Hour(1)
        println("Searched $count times, back $(hours) for last for $ts_last")
        lm = lms[which_is_missing(otoq_last, lms)]
        ms = round(Int, datetime2unix(ts_last))
        strike = getStrike(lm)
        println("$(ms).*$(ms).*$(strike)")
        # @show  round(Int, getMarketClose(xpir))
        error("stop")
    end
    return price_last
end

using Between
requote(otoq, lms::NTuple) = tosn(LegMetaClose, lms, otoq)
reprice(otoq, lms::NTuple) = ( req = requote(otoq, lms) ; isnothing(req) ? nothing : Pricing.price(req, true) )
which_is_missing(otoq, lms) = findfirst(lm -> isnothing(to(LegMetaClose, lm, otoq)), lms)

#region Util move
Pricing.price_short(x::Real) = x # to deal with searchsorted*

# make_condors_long(f, calls, puts; max_spread=4) = make_condors_long(f, calls, puts, prices(calls, puts)...; max_spread)
# function make_condors_long(f, calls, puts, call_prices, put_prices; max_spread=4)
function make_condors_long(f, calls, puts; max_spread=4)
    # global kcalls = calls
    # global kputs = puts
    # global kcall_prices = call_prices
    # global kput_prices = put_prices
    # this search assumes price and strike sort the same which isn't always the case, but is probably close enough
    put_right0 = searchsortedfirst(puts, 0.04; by=Pricing.price_short)
    put_right0 > 1 || return
    # this search assumes price and strike sort the same which isn't always the case, but is probably close enough
    call_left0 = searchsortedlast(calls, 0.04; rev=true, by=Pricing.price_short)
    call_left0 < length(calls) || return
    # global kput_right0 = put_right0
    # global kcall_left0 = call_left0
    for put_right_ind in put_right0:lastindex(puts)
        put_right_strike = getStrike(puts[put_right_ind])
        for put_left_ind in max(1, put_right_ind - max_spread):(put_right_ind - 1)
            put_left = puts[put_left_ind]
            put_right = puts[put_right_ind]
            # put_price = put_prices[put_left_ind].long + put_prices[put_right_ind].short
            put_price = Pricing.price_long_flip(put_left) + Pricing.price_short(put_right)
            if put_price < 0.03
                continue
            end
            for call_left_ind in call_left0:-1:1
                if getStrike(calls[call_left_ind]) < put_right_strike
                    break
                end
                for call_right_ind in min(lastindex(calls), call_left_ind + max_spread):-1:(call_left_ind+1)
                    call_left = calls[call_left_ind]
                    call_right = calls[call_right_ind]
                    # call_price = call_prices[call_left_ind].short + call_prices[call_right_ind].long
                    call_price = Pricing.price_short(call_left) + Pricing.price_long_flip(call_right)
                    if call_price < 0.03
                        continue
                    end
                    lms = (LegMetaOpen(put_left, Side.long), LegMetaOpen(put_right, Side.short), LegMetaOpen(call_left, Side.short), LegMetaOpen(call_right, Side.long))
                    f(lms, put_price + call_price)
                end
            end
        end
    end
end
#endregion

end