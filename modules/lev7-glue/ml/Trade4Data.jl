module Trade4Data
using Dates
using SH, BaseTypes, SmallTypes, ChainTypes, LegMetaTypes
using DateUtil, ChainUtil
import Pricing, Calendars
import SimpleStore as SS
import MarketHist

#=
Train two models: entry policy and exit policy.
We track: what is the best result (max profit or min loss) that could be accomplished for a candidate trade?
The loss for the entry policy is if it entered a trade that had < threshold max profit
The loss for the exit policy would be its distance below the best result.

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
    SS.run(Date(2020,1,1), Date(2023,1,1); maxSeconds=1000) do tim, chain
        # println("processing: ", tim.ts)
        ts = tim.ts
        i += 1
        curp = ChainUtil.getCurp(chain)
        curp_hist = map(2.0 .^ (3:10)) do tex_ago
            log(curp / MarketHist.curp_for_tex(tim.ts, tex_ago))
        end
        vix_hist =  map(2.0 .^ (3:10)) do tex_ago
            log(MarketHist.vix_for_tex(ts, tex_ago))
        end
        atm = ChainUtil.calc_atm(chain)
        temporal = makeTemporal(tim)
        cands = proc_ts(ts, chain)

        for cand in cands
            push!(cands_all, SCALAR[
                cand...
                curp_hist...
                vix_hist...
                atm...
                temporal...
            ])
        end
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
    xpir = ChainUtil.getXpirs(chain)[1]
    global soqs = chain.xsoqs[xpir]
    call_left = searchsortedfirst(soqs.call, curp - 4.0; by=getStrike)
    calls = @view soqs.call[call_left:end]
    put_right = searchsortedlast(soqs.put, curp + 4.0; by=getStrike)
    puts = @view soqs.put[1:put_right]

    call_prices, put_prices = prices(calls, puts)
    @assert length(call_prices) == length(calls)
    @assert length(put_prices) == length(puts)

    res = []
    proc = (lms, neto) -> push!(res, proc_lms(lms, neto, ts, curp))
    make_condors_long(proc, calls, puts, call_prices, put_prices)
    # proc_condors_short(proc, puts, calls, call_prices, put_prices)
    return res
end

function prices(calls, puts)
    call_prices = map(oq -> Pricing.price(oq), calls)
    put_prices = map(oq -> Pricing.price(oq), puts)
    return call_prices, put_prices
end

function proc_condors_short(puts, put_prices, calls, call_prices)
end

function proc_lms(lms, neto, ts, curp)
    texp = Calendars.calcTex(ts, Calendars.getMarketClose(getExpir(lms)))
    return (
        texp,
        neto,
        enc_lms(lms, curp)...,
        make_label(ts, lms)...,
    )
end

enc_lms(lms, curp) = Iterators.flatten(map(lm -> enc_lm(lm, curp), lms))

function enc_lm(lm, curp)
    side = Int(getSide(lm))
    q = getQuote(lm)
    spread = q.ask - q.bid
    return (
        SCALAR(Int(getStyle(lm))),
        SCALAR(getStrike(lm) / curp),
        SCALAR(side),
        SCALAR(Pricing.price(lm) * side),
        SCALAR(spread),
        # SCALAR(getQuantity(lm)),
        # TODO: flatten get meta
        SCALAR.(LegMetaTypes.metaToFlat(lm))...,
    )
end

function make_label(ts, lms)
    xpirts = Calendars.getMarketClose(getExpir(lms))
    otoqs = SS.chains_for(Calendars.getMarketOpen(bdaysAfter(Date(ts), 1)), Calendars.getMarketClose(getExpir(lms)))
    rate_min, rate_max = (typemax(Float64), typemin(Float64))
    price_min, price_max = (typemax(Float64), typemin(Float64))
    map(otoqs) do ts, otoq
        p = Pricing.price(requote(otoq, lms))
        if p < price_min
            price_min = p
            r = calcRate(ts, xpirts)
            if r < rate_min
                rate_min = r
            end
        elseif p > price_max
            price_max = p
            r = calcRate(ts, xpirts)
            if r > rate_max
                rate_max = r
            end
        end
    end
    return rate_min, rate_max
end

requote(otoq, lms) = tosn(LegMetaClose, lms, otoq)

#region Util move
Pricing.price(x::Real) = x # to deal with searchsorted*
make_condors_long(f, calls, puts; max_spread=4) = make_condors_long(f, calls, puts, prices(calls, puts)...; max_spread)
function make_condors_long(f, calls, puts, call_prices, put_prices; max_spread=4)
    put_right0 = searchsortedfirst(puts, 0.04; by=Pricing.price)
    put_right0 > 1 || return
    call_left0 = searchsortedlast(calls, 0.04; by=Pricing.price)
    call_left0 < length(calls) || return
    # println("Long condors: ", (lastindex(puts) - put_right0), ' ', call_left0)
    for put_right_ind in put_right0:lastindex(puts)
        put_right_strike = getStrike(puts[put_right_ind])
        for put_left_ind in max(1, put_right_ind - max_spread):(put_right_ind - 1)
            put_price = put_prices[put_right_ind] + put_prices[put_left_ind]
            if put_price < 0.03
                continue
            end
            for call_left_ind in call_left0:-1:1
                if getStrike(calls[call_left_ind]) < put_right_strike
                    break
                end
                for call_right_ind in min(lastindex(calls), call_left_ind + max_spread):-1:(call_left_ind+1)
                    # @show call_left0 call_left_ind call_right_ind
                    call_price = call_prices[call_right_ind] + call_prices[call_left_ind]
                    if call_price < 0.03
                        continue
                    end
                    lms = (LegMetaOpen(puts[put_left_ind], Side.long), LegMetaOpen(puts[put_right_ind], Side.short), LegMetaOpen(calls[call_left_ind], Side.short), LegMetaOpen(calls[call_right_ind], Side.long))
                    f(lms, put_price + call_price)
                end
            end
        end
    end
end
#endregion

end