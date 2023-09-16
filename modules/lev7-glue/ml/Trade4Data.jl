module Trade4Data
using Dates
using SH, BaseTypes, SmallTypes, ChainTypes, LegQuoteTypes
using BaseUtil, DateUtil, ChainUtil, CollUtil
import Pricing, Calendars
import SimpleStore as SS
import MarketHist
import Scoring
import MLBase, ProbKde, HistData

#=
Train two models: entry policy and exit policy.
We track: what is the best result (max profit or min loss) that could be accomplished for a candidate trade?
The loss for the entry policy is if it entered a trade that had < threshold max profit
The loss for the exit policy could be its distance below the best result which would work on both winning and losing trades. Then we can train it with losing trades, also.
We should probably try training exit policy both ways: only on potential winners, and all.

Challenge for temporal things is because market days aren't continuous, we need to somehow
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

function make_data(range=MLBase.DateRange[];timeout=1)
    # res = Array{SCALAR,2}(undef, 2, 0)
    xs = Vector{Vector{SCALAR}}()
    ys = Vector{NTuple{3,SCALAR}}()
    labels = Vector{NTuple{2,SCALAR}}() # one-host yes/no
    global kxs = xs
    global kys = ys
    global klabels = labels

    i = 0
    SS.run(range; maxSeconds=timeout) do tim, chain
        println("processing: ", tim.ts)
        ts = tim.ts
        i += 1
        curp = ChainUtil.getCurp(chain)
        #
        # curp_hist = map(2.0 .^ (3:10)) do tex_ago
        curp_hist = map((24*i + 1.41^i)/24 for i in 1:20) do tex_ago
            # log(curp / MarketHist.curp_for_tex(tim.ts, tex_ago))
            curp / MarketHist.curp_for_tex(tim.ts, tex_ago)
        end
        vix_hist =  map((24*i + 1.41^i)/24 for i in 1:20) do tex_ago
            log(MarketHist.vix_for_tex(ts, tex_ago))
            # MarketHist.vix_for_tex(ts, tex_ago)
        end
        atm = ChainUtil.calc_atm(chain)
        temporal = makeTemporal(tim)
        scores, cands = proc_ts(tim, chain)

        for (cand, score) in zip(cands, scores)
            push!(xs, SCALAR[
                cand...
                curp_hist...
                vix_hist...
                atm...
                temporal...
            ])
            push!(ys, score)
            push!(labels, label(score))
        end
        if !isempty(cands)
            # println("Spec:\n", "cand, curp_hist, vix_hist, atm, temporal\n", join(length.((first(cands), curp_hist, vix_hist, atm, temporal)), ", "))
            println("Added $(length(cands)) cands for ts $ts")
        else
            println("No cands found for $ts")
        end
    end
    yesrat = count(x -> x[1] == 1, labels) / length(labels)
    println("Out of $(length(labels)): yes ratio: $(yesrat)")
 # TODO: change to matrix
    # return xs
end

# TODO: stuff right around the threshold shouldn't be penalized?
label(score) = score[2] >= 1.0 && score[3] >= -10.0 ? (1., 0.) : (0., 1.0)

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
function proc_ts(tim::SS.TimeInfo, chain::ChainInfo)
    curp = ChainUtil.getCurp(chain)
    ts = tim.ts
    date = tim.date
    xpirs = ChainUtil.getXpirs(chain)
    # TODO: do multiple expirations
    xpir = CollUtil.gteev(xpirs, bdaysAfter(Date(ts), 4))
    @assert xpir > ts "Invalid xpir $xpir for ts $ts"
    soqs = chain.xsoqs[xpir]
    call_left = searchsortedfirst(soqs.call, curp - 4.0; by=getStrike)
    calls = @view soqs.call[call_left:end]
    put_right = searchsortedlast(soqs.put, curp + 4.0; by=getStrike)
    puts = @view soqs.put[1:put_right]

    # call_prices, put_prices = prices(calls, puts)
    # @assert length(call_prices) == length(calls)
    # @assert length(put_prices) == length(puts)

    # TODO: use tex instead of dates?
    vix = F(HistData.vixOpen(tim.date))
    days, tmult = DateUtil.durtimult(date, xpir)
    prob = ProbKde.probToClose(F(curp), vix, ts, xpir)
    tex = Calendars.calcTex(ts, Calendars.getMarketClose(xpir))

    xs = []
    ys = []
    proc = (lms, score) -> begin
        y, x = proc_lms(ts, curp, lms, score)
        push!(xs, (SCALAR(tex), SCALAR(days), SCALAR(tmult), x...))
        push!(ys, y)
    end
    make_condors_long(proc, prob, curp, tmult, calls, puts) # , call_prices, put_prices)
    # proc_condors_short(proc, puts, calls, call_prices, put_prices)
    return ys, xs
end

# function prices(calls, puts)
#     call_prices = map(oq -> Sides(Pricing.price_long_flip(oq), Pricing.price_short(oq)), calls)
#     put_prices = map(oq -> Sides(Pricing.price_long_flip(oq), Pricing.price_short(oq)), puts)
#     return call_prices, put_prices
# end

# function proc_condors_short(puts, put_prices, calls, call_prices)
# end

function proc_lms(ts, curp, lms, score)
    return (make_label(ts, lms, score.neto, score.data.risk), (
        SCALAR(score.neto),
        SCALAR(score.data.profit),
        SCALAR(score.data.risk),
        SCALAR(score.data.rate),
        SCALAR(score.data.kel),
        enc_lms(lms, curp)...,
    ))
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
        SCALAR.(LegQuoteTypes.metaToFlat(lm))...,
    )
end

# TODO: move to where it belongs
# import LinesLeg as LL
# SH.getRisk(lms::Coll{<:LegQuote}) = -Pricing.calcCommit(LL.toSegments(Tuple(lms)))

function make_label(ts_start, lms::NTuple, neto, risk)
    date_start = Date(ts_start)
    # xpirts = Calendars.getMarketClose(getExpir(lms))
    ts_last = SS.last_ts_for(getExpir(lms))
    otoqs = SS.chains_for(SS.first_ts_for(bdaysAfter(date_start, 1)), ts_last)
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
                r = calcRate(date_start, Date(ts), neto + p, risk)
                if r < rate_min
                    rate_min = r
                end
            end
            if p > price_max
                price_max = p
                r = calcRate(date_start, Date(ts), neto + p, risk)
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
    # rate_end = calcRate(ts_start, ts_last, neto + price_last, risk)
    rate_end = calcRate(date_start, Date(ts_last), neto + price_last, risk)
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
requote(otoq, lms::NTuple) = tosn(LegQuoteClose, lms, otoq)
reprice(otoq, lms::NTuple) = ( req = requote(otoq, lms) ; isnothing(req) ? nothing : Pricing.price(req, true) )
which_is_missing(otoq, lms) = findfirst(lm -> isnothing(to(LegQuoteClose, lm, otoq)), lms)

#region Util move
Pricing.price_short(x::Real) = x # to deal with searchsorted*
function price_short_guard(oq)
    try
        return Pricing.price_short(oq)
    catch
        return CZ
    end
end

# make_condors_long(f, calls, puts; max_spread=4) = make_condors_long(f, calls, puts, prices(calls, puts)...; max_spread)
# function make_condors_long(f, calls, puts, call_prices, put_prices; max_spread=4)
function make_condors_long(f, prob, curp, tmult, calls, puts; max_spread=4) # TODO: bigger max spread might be fine
    # global kcalls = calls
    # global kputs = puts
    # global kcall_prices = call_prices
    # global kput_prices = put_prices
    # this search assumes price and strike sort the same which isn't always the case, but is probably close enough
    put_right0 = searchsortedfirst(puts, 0.04; by=price_short_guard)
    put_right0 > 1 || return
    # this search assumes price and strike sort the same which isn't always the case, but is probably close enough
    call_left0 = searchsortedlast(calls, 0.04; rev=true, by=price_short_guard)
    call_left0 < length(calls) || return
    # global kput_right0 = put_right0
    # global kcall_left0 = call_left0
    count = 0
    added = 0
    for put_right_ind in put_right0:lastindex(puts)
        put_right_strike = getStrike(puts[put_right_ind])
        for put_left_ind in max(1, put_right_ind - max_spread):(put_right_ind - 1)
            put_left = puts[put_left_ind]
            put_right = puts[put_right_ind]
            # put_price = put_prices[put_left_ind].long + put_prices[put_right_ind].short
            put_price = get_put_price(put_left, put_right)
            (!isnothing(put_price) && put_price >= 0.03) || continue
            for call_left_ind in call_left0:-1:1
                if getStrike(calls[call_left_ind]) < put_right_strike
                    break
                end
                for call_right_ind in min(lastindex(calls), call_left_ind + max_spread):-1:(call_left_ind+1)
                    call_left = calls[call_left_ind]
                    call_right = calls[call_right_ind]
                    # call_price = call_prices[call_left_ind].short + call_prices[call_right_ind].long
                    call_price = get_call_price(call_left, call_right)
                    (!isnothing(call_price) && call_price >= 0.03) || continue
                    lms = (LegQuoteOpen(put_left, Side.long), LegQuoteOpen(put_right, Side.short), LegQuoteOpen(call_left, Side.short), LegQuoteOpen(call_right, Side.long))
                    score = Scoring.score_condor_long(prob, curp, tmult, lms; params=ScoringParams[])
                    count += 1
                    isnothing(score) || ( added += 1 ; f(lms, score) )
                end
            end
        end
    end
    println("Scored $count condors, added $added")
end
const ScoringParams = Ref((;MinProfit=0.1, MinRate=0.4, MinKel=0.75, PriceAdjust=-0.01))
function get_put_price(put_left, put_right)
    try
        return Pricing.price_long_flip(put_left) + Pricing.price_short(put_right)
    catch e
        return nothing
    end
end
function get_call_price(call_left, call_right)
    try
        return Pricing.price_short(call_left) + Pricing.price_long_flip(call_right)
    catch e
        return nothing
    end
end
#endregion

end