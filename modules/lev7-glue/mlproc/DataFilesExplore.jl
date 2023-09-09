module DataFilesExplore
using Dates, DataStructures, DataFrames
using BaseTypes, SmallTypes, ChainUtil
import SH, DateUtil, Pricing, Calendars
import DataFiles as dat
import Explore as ore
import ProbMultiKde as pmk
import DrawUtil, ThreadUtil

# TODO: check how close :under_at_xpir and :xpir_value are
# TODO: because selling at xpir is lossy, punish the outcomes harshly for that section

#region Public
function explore(;inds=nothing, yms=dat.make_yms(), skip_existing=true, use_problup=false)
    # global kdf = nothing
    # global kr1 = nothing
    tsi_index = dat.ts_indexed()
    ts_min = dat.estimate_min_ts() + Month(1)
    y_min, m_min = year(ts_min), month(ts_min)
    filter!(yms) do (y, m)
        y > y_min || (y == y_min && m >= m_min)
    end
    for (y, m) in yms
        kde = pmk.get_kde(y, m)
        oqs_df = dat.oqs_df(y, m)
        # TODO: save a file that's already filtered?
        oqs_df = filter(:ts => dat.filter_tsx_prob, oqs_df)
        sdfs = groupby(oqs_df, :ts)
        itr = isnothing(inds) ? sdfs :
              inds isa Integer ? sdfs[inds:end] : sdfs[inds]
        count = length(itr)
        println("Processing $(count) in $(y)-$(m)")
        i = 0
        stop = ThreadUtil.loop(itr) do sdf
            i += 1
            ts = first(sdf.ts)
            !skip_existing || !haskey(ALL_BEST, ts) || return
            if !dat.is_ts_normal(ts)
                println("$(ts): Skipping non-normal")
                return
            end
            if !isnothing(findfirst(ismissing, sdf.call_xpir_value)) ||
                    !isnothing(findfirst(ismissing, sdf.call_xpir_price_long))
                    println("$(ts): Skipping due to missing xpir pricing")
                    return
            end
            # println("Processing ts:$(ts)")
            curp = dat.pents_to_c(dat.lup_under(tsi_index, ts))
            ctx = ore.make_ctx(ts, kde, curp)
            # TODO: could be more efficient leveraging group by below? get keys on xpir_lookup?
            xpirtss = filter!(d -> Date(d) > ctx.date, unique(sdf.expiration))
            sort!(xpirtss)
            xpirtss = first(xpirtss, 2)

            # xpir_lookup = Dict(map(sdfs_xpir) do sdf_xpir
            #     return first(sdfx_xpir.expiration) => sdf_xpir
            # end)
            # for sdf_xpir in groupby(sdf, :expiration)
            #     xpir_lookup[first(sdf_xpir.expiration)] = sdf_xpir
            # end
            xpir_lookup = Dict([first(sdf_xpirts.expiration) => sdf_xpirts for sdf_xpirts in groupby(sdf, :expiration)])

            # global kdf = sdf
            res = ore.findkel(ctx, xpirtss, 1:3; use_problup) do xpirts, curp
                ChainUtil.oqssEntry(to_oqs(xpir_lookup[xpirts]), curp)
            end

            if !isempty(res) # && res[1].evrate > 1e-4
                if length(res) < 100
                    println("$(ts): Found $(length(res)) < 100 results")
                end
                # showres(tsi_index, ts, sdf, curp, res[end])
                # showres(tsi_index, ts, sdf, curp, res[1])
                r1 = res[1]
                all = df_calc_pnl(sdf, r1.lms, dat.market_close(SH.getExpir(r1.lms)))
                ThreadUtil.runSync(lock_proc) do
                    ALL_BEST[ts] = (;ts, curp, r1, all)
                end
            else
                println("$(ts): No results")
            end
            if (i % 100) == 0
                println("Completed $(count) in $(y)-$(m)")
            end
        end
        !stop || break
    end
end

using DictUtil, StatsBase
function drawbal(kelrat = 1.0)
    all = collect(values(ALL_BEST))
    d = Dict{Date,Vector}()
    for r in all
        v = get!(d, SH.getExpir(r.r1.lms), Vector())
        push!(v, r)
        # addToKey(d, SH.getExpir(r.r1.lms), r.r1.kel * r.all.pnl_value / r.r1.risk)
    end
    xpirs = sort!(collect(keys(d)))
    drets = Dict()
    for xpir in xpirs
        # rs = d[xpir]
        # balcommitted = CZ
        # rets = []
        # for r in rs
        #     date = Date(r.r1.ts)
        #     days = DateUtil.bdays(date, xpir)
        #     maxadd = days*5
        #     remainingkel = (1 - balcommitted) - kelrat * r.r1.kel
        #     remainingkel > 0 || continue
        #     balcommit = remainingkel / maxadd
        #     balcommitted += balcommit
        #     ret = balcommit * r.all.pnl_value / r.r1.risk
        #     push!(rets, ret)
        # end
        # drets[xpir] = rets

        rs = d[xpir]
        rets = map(rs) do r
        end
        drets[xpir] = sum(rets)
        # k =mean(map(r -> r.r1.kel, rs))

    end
    # vals = map(x -> d[x], xpirs)
    # acc = accumulate(+, vals)
    # acc = accumulate((a,x) -> (x+1)*a, vals; init=100)
    # DrawUtil.draw(:scatter, xpirs, acc)
    rets = map(x -> sum(drets[x]), xpirs)
    # DrawUtil.draw(:scatter, xpirs, rets)
    acc = accumulate((a,x) -> (x+1)*a, rets; init=100)
    DrawUtil.draw(:scatter, xpirs, acc)
end

lurp(date::Date) = lurp(dat.market_close(date))
function lurp(ts::DateTime)
    return dat.pents_to_c(dat.lup_under(ts))
end
#endregion Public

#region Local
const ALL_BEST = SortedDict{DateTime,Any}()
const lock_proc = ReentrantLock()

function showres(tsi_index, ts, sdf, under, r1)
    # global kshowres_args = (tsi_index, ts, sdf, under, r1)
    lms = r1.lms
    xpirts = dat.market_close(SH.getExpir(lms))
    under_xpir = dat.pents_to_c(dat.lup_under(tsi_index, xpirts))
    target = Pricing.price(Action.open, lms)
    bdaysout = DateUtil.bdays(Date(ts), SH.getExpir(lms))
    all = df_calc_pnl(sdf, lms, xpirts)

    println("$(ts): Found evrate:$(r1.evrate) probprofit:$(r1.probprofit) min:$(r1.neto) target:$(target) risk:$(r1.risk) daysout:$(bdaysout)");
    println("  estimated pnl: $(all.pnl) ($(all) under:$(under) -> $(under_xpir)")
    ore.drawres(lms)
    DrawUtil.draw!(:vlines, under_xpir; label="under_xpir")
    return lms
end

function df_calc_pnl(df, lms, xpirts)
    strikes = SH.getStrike.(lms)
    rows = dfrow.(Ref(df), xpirts, strikes)
    neto = sum(dfneto.(rows, lms))
    netc = sum(dfnetc.(rows, lms))
    netc_value = sum(dfnetc_value.(rows, lms))
    return (;neto, netc, netc_value, pnl=neto + netc, pnl_value=neto + netc_value)
end
function dfrow(df, xpirts, strike)
    s = dat.topents(strike)
    return only(df[(df.expiration .== xpirts) .& (df.strike .== s),:])
end
dfneto(dfrow, lm) = dat.pents_to_c(dfrow[dfsym(SH.getStyle(lm), SH.getSide(lm), Action.open)])
dfnetc(dfrow, lm) = dat.pents_to_c(dfrow[dfsym_xpir_price(SH.getStyle(lm), SH.toOther(SH.getSide(lm)))])
dfnetc_value(dfrow, lm) = Int(SH.getSide(lm)) * dat.pents_to_c(dfrow[dfsym_xpir_value(SH.getStyle(lm))])

dfsym(style, side, action) = Symbol("$(style)_$(side)_$(SH.toCode(action))")
dfsym_xpir_price(style, side) = Symbol("$(style)_xpir_price_$(side)")
dfsym_xpir_value(style) = Symbol("$(style)_xpir_value")

using OptionTypes, OptionQuoteTypes, OptionMetaTypes, QuoteTypes
to_oqs(df::DataFrames.AbstractDataFrame) = collect(Iterators.flatten(map(to_oq_pair, eachrow(df))))
function to_oq_pair(row)
    oc = Option(Style.call, Date(row.expiration), dat.pents_to_c(row.strike))
    op = Option(Style.put, Date(row.expiration), dat.pents_to_c(row.strike))
    qc = Quote(dat.pents_to_c(row.call_bid), dat.pents_to_c(row.call_ask))
    qp = Quote(dat.pents_to_c(row.put_bid), dat.pents_to_c(row.put_ask))
    return OptionQuote(oc, qc, OptionMetaTypes.MetaZero), OptionQuote(op, qp, OptionMetaTypes.MetaZero)
end
#endregion Local

end