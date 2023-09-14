module DataFilesExplore
using Dates, DataStructures, DataFrames
using BaseTypes, SmallTypes, ChainTypes
using DateUtil, ChainUtil
import SH, Pricing, Calendars
import DataFiles as dat
import Explore as ore
import ProbMultiKde as pmk
import DrawUtil, ThreadUtil
using OutputUtil

# TODO: check how close :under_at_xpir and :xpir_value are
# TODO: because selling at xpir is lossy, punish the outcomes harshly for that section

#region Public
function explore(;inds=nothing, yms=dat.make_yms(), skip_existing=true, use_problup=false)
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
        # stop = ThreadUtil.loop(itr) do sdf
        # rs_for_xpirts = Dict{DateTime,Vector}()
        for sdf_ts in itr
            ts = first(sdf_ts.ts)
            rs_ts = Vector()
            !skip_existing || !haskey(TOP_TS, ts) || continue
            if !dat.is_ts_normal(ts)
                println("$(ts): Skipping non-normal")
                continue
            end
            if !isnothing(findfirst(ismissing, sdf_ts.call_xpir_value)) ||
                    !isnothing(findfirst(ismissing, sdf_ts.call_xpir_price_long))
                    println("$(ts): Skipping due to missing xpir pricing")
                    continue
            end
            # println("Processing ts:$(ts)")
            curp = dat.pents_to_c(dat.lup_under(ts))
            ctx = ore.make_ctx(ts, kde, curp)
            # TODO: could be more efficient leveraging group by below? get keys on xpir_lookup?
            # xpirtss = filter!(d -> Date(d) > ctx.date, unique(sdf.expiration))
            # sort!(xpirtss)
            # xpirtss = first(xpirtss, 4)

            # # xpir_lookup = Dict(map(sdfs_xpir) do sdf_xpir
            # #     return first(sdfx_xpir.expiration) => sdf_xpir
            # # end)
            # # for sdf_xpir in groupby(sdf, :expiration)
            # #     xpir_lookup[first(sdf_xpir.expiration)] = sdf_xpir
            # # end
            # xpir_lookup = Dict([first(sdf_xpirts.expiration) => sdf_xpirts for sdf_xpirts in groupby(sdf, :expiration)])

            # # global kdf = sdf
            # res = ore.findkel(ctx, xpirtss, 1:3; use_problup) do xpirts, curp
            #     ChainUtil.oqssEntry(to_oqs(xpir_lookup[xpirts]), curp; minlong=C(0.05), minshort=C(0.05))
            # end

            sdf_xpir = groupby(sdf_ts, :expiration)

            for sdf in sdf_xpir[1:3]
                xpirts = sdf.expiration[1]
                xpir = Date(xpirts)
                # 0 < bdays(ts, xpir) <= 8 || continue
                0 < bdays(ts, xpir) || continue
                oqss = ChainUtil.oqssEntry(to_oqs(sdf), curp; minlong=C(0.05), minshort=C(0.05))

                top_xpir = get!(TOP_XPIRTS, xpirts) do; Vector() end

                res = ore.findkel(ctx, xpirts, oqss, top_xpir, 1:3; use_problup)
                # isnothing(res) && @goto stop
                global kres = res

                if !isempty(res) # && res[1].evrate > 1e-4
                    expres = 10
                    if length(res) < expres
                        println("$(ts) -> $(xpirts): Found $(length(res)) < $(expres) results")
                    end
                    # showres(tsi_index, ts, sdf, curp, res[end])
                    # showres(tsi_index, ts, sdf, curp, res[1])
                    r1 = res[1]
                    all = df_calc_pnl(sdf, r1.lms, dat.market_close(SH.getExpir(r1.lms)))
                    # ThreadUtil.runSync(lock_proc) do
                        r = (;ts, curp, r1, all)
                        # push!(top_xpir, r)
                        # push!(rs_ts, r)
                    # end
                else
                    println("$(ts): No results for $(xpirts)")
                end
            end
            # TOP_TS[ts] = rs_ts
            println("Completed ts:$(ts) at $(DateUtil.nowz())")
        end
    end
    # @label stop
end

using DictUtil, StatsBase
function drawbal(kelrat = 1.0, newfig=false)
    # all = collect(values(ALL_BEST))
    # global dxpirs = Dict{Date,Vector}()
    # for r in all
    #     v = get!(dxpirs, SH.getExpir(r.r1.lms), Vector())
    #     push!(v, r)
    #     # addToKey(d, SH.getExpir(r.r1.lms), r.r1.kel * r.all.pnl_value / r.r1.risk)
    # end
    # xpirs = sort!(collect(keys(dxpirs)))
    xpirtss = sort!(collect(keys(TOP_XPIRTS)))
    global drets = Dict()
    for xpirts in xpirtss
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

        # rs = dxpirs[xpir]
        rs = TOP_XPIRTS[xpirts]
        count = length(rs)
        rets = map(rs) do r
            r.r1.balrat * r.r1.kel * r.all.pnl_value / r.r1.risk
        end
        # drets[xpir] = sum(rets) * kelrat / count
        drets[xpirts] = sum(rets) * kelrat
    end
    # vals = map(x -> d[x], xpirs)
    # acc = accumulate(+, vals)
    # acc = accumulate((a,x) -> (x+1)*a, vals; init=100)
    # DrawUtil.draw(:scatter, xpirs, acc)
    # rets = map(x -> sum(drets[x]), xpirs)
    rets = map(x -> sum(drets[x]), xpirtss)
    # DrawUtil.draw(:scatter, xpirs, rets)
    acc = accumulate((a,x) -> (x+1)*a, rets; init=100)
    if newfig
        DrawUtil.draw(:scatter, xpirtss, acc)
    else
        DrawUtil.draw!(:scatter, xpirtss, acc)
    end
end

function updown()
    all = get_all()
    tot_count = length(all)
    # sort!(all; by=r -> )
    up = filter(r -> r.all.pnl_value >= 0, all)
    down = filter(r -> r.all.pnl_value < 0, all)
    println("up:")
    stats(up, tot_count)
    println("down:")
    stats(down, tot_count)
end

using StatsBase
function stats(rs, tot_count)
    count = length(rs)
    println("count/total: $(r4(count / tot_count))")
    styles = countmap([SH.getStyle.(r.r1.lms) for r in rs])
    display(styles)
    bdays_out = countmap([bdout(r) for r in rs])
    display(bdays_out)
    neto_sign = countmap([sign(r.r1.neto) for r in rs])
    display(neto_sign)
end

bdout(r) = bdays(r.ts, SH.getExpir(r.r1.lms))

get_all() = collect(Iterators.flatten(values(TOP_XPIRTS)))

lurp(date::Date) = lurp(dat.market_close(date))
function lurp(ts::DateTime)
    return dat.pents_to_c(dat.lup_under(ts))
end

using LRUCache
const OQS_TS_CACHE = LRUCache.LRU{DateTime, Vector{OptionQuote}}(;maxsize=100)
function get_oqs(ts::DateTime)
    return get!(OQS_TS_CACHE, ts) do
        y, m = dat.get_ym(ts)
        oqs_df = dat.oqs_df(y, m)
        return to_oqs(filter(:ts => ts -> ts == ts, oqs_df))
    end
end

makeprob(ts, lms) = makeprob(ts, SH.getExpir(lms))
makeprob(ts::DateTime, xpir::Date) = makeprob(ts, dat.market_close(xpir))
function makeprob(ts::DateTime, xpirts::DateTime)
    kde = pmk.get_kde(ts)
    curp = lurp(ts)
    oqs = get_oqs(ts)
    return pmk.makeprob(kde, curp, ts, xpirts, oqs)
end

calckel(ts::DateTime, lms) = ore.calckel(makeprob(ts, SH.getExpir(lms)), lms)

function add_lms(ts, lms1, lms2)
    xpir = SH.getExpir(lms1)
    @assert xpir == SH.getExpir(lms2)
    k1 = calckel(ts, lms1)
    k2 = calckel(ts, lms2)
    lms12 = sort!([lms1..., lms2...]; by=SH.getStrike)
    k12 = calckel(ts, lms12)
    @show k1 k2 k12
    DrawUtil.drawprob(makeprob(ts, lms2); label="prob")
    DrawUtil.draw!(:lines, SH.toDraw(lms1); label="k1")
    DrawUtil.draw!(:lines, SH.toDraw(lms2); label="k2")
    DrawUtil.draw!(:lines, SH.toDraw(lms12); label="k12")
    return lms12
end

using StaticArrays

function add_rs(rs)
    xpir = SH.getExpir(rs[1].r1.lms)
    @assert isnothing(findfirst(r -> SH.getExpir(r.r1.lms) != xpir, rs))
    r = rs[1]
    lmss = SVector(r.r1.lms)
    k = calckel(r.ts, lmss)
    @show k
    DrawUtil.drawprob(makeprob(r.ts, lmss); label="prob-1")
    DrawUtil.draw!(:lines, SH.toDraw(lmss); label="lms-1")
    for i in eachindex(rs)[2:end]
        r = rs[i]
        ts = r.ts
        lms = r.r1.lms
        kpre = calckel(ts, lmss)
        # lmss = sort!(append!(lmss, lms); by=SH.getStrike)
        lmss = sort(vcat(lmss, SVector(lms)); by=SH.getStrike)
        kn = calckel(ts, lms)
        kall = calckel(ts, lmss)
        @show kpre kn kall

        DrawUtil.drawprob!(makeprob(ts, lms); label="prob-$(i)")
        DrawUtil.draw!(:lines, SH.toDraw(lms); label="lms-$(i)")
        DrawUtil.draw!(:lines, SH.toDraw(lmss); label="lmss-$(i)")
    end
    xpirts = dat.market_close(xpir)
    curp_xpirts = lurp(xpirts)
    DrawUtil.draw!(:vlines, curp_xpirts; color=:white, label="curp_xpir")
end

using JLD2, FileIO
function save_state()
    save_state("top_xpirts", TOP_XPIRTS)
    save_state("top_ts", TOP_TS)
end
function save_state(name, obj)
    file = File(format"JLD2", "D:/data/tmp/$(name).jld2")
    save(file, name, obj)
end

function load_state()
    loadinto("top_xpirts", TOP_XPIRTS)
    loadinto("top_ts", TOP_TS)
end
function loadinto(name, into)
    obj = FileIO.load("D:/data/tmp/$(name).jld2", name)
    empty!(into)
    merge!(into, obj)
end
#endregion Public

#region Local
function reset()
    empty!(TOP_TS)
    empty!(TOP_XPIRTS)
    ore.MAX_LEN[] = 0
end
const TOP_TS = SortedDict{DateTime,Vector}()
const TOP_XPIRTS = SortedDict{DateTime,Vector}()
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