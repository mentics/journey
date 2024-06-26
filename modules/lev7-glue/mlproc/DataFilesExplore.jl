module DataFilesExplore
using Dates, DataStructures, DataFrames
using BaseTypes, SmallTypes, ChainTypes
using BaseUtil, DateUtil, ChainUtil
import SH, Pricing, Calendars
import Explore as ore
import DrawUtil, ThreadUtil
using OutputUtil
import Calendars as cal
import Paths
import MLyze

using DataRead

# TODO: check how close :under_at_xpir and :xpir_value are
# TODO: because selling at xpir is lossy, punish the outcomes harshly for that section

#region Public
const EMPTY_VECTOR = Vector()

is_ts_backtest(ts_min) = function(ts)
    date = Date(ts)
    return ts >= ts_min && ts != cal.getMarketOpen(date) && ts != cal.getMarketClose(date)
end

function run_long(;xpir_inds=1:8, max_bdays_out=32, incs=4:4, restart=false)
    restart && reset(;max_bdays_out)
    yms = vcat([(2020,i) for i in 1:12], [(2021,i) for i in 1:12], [(2022,i) for i in 1:12], [(2023,i) for i in 1:12])
    explore(;yms, use_pos=false, xpir_inds, max_bdays_out, incs);
end

import DataFiles as dat
function explore(;inds=nothing, yms=dat.make_yms(), skip_existing=true, use_pos=true, xpir_inds=1:2, max_bdays_out=8, incs=1:4)
    price_lup = DataRead.price_lookup(;age=DateUtil.FOREVER2)
    vix_lup = DataRead.vix_lookup(;age=DateUtil.FOREVER2)
    # price_lup_xpirts = DataRead.price_lookup_xpirts()
    # ts_min, prob_for_tsxp = DataRead.prob_for_tsxp(;age=Day(2))
    ts_min = dat.estimate_min_ts() + Month(1)
    # y_pmfk = Paths.load_data(Paths.db_output("y_pmfk"), "y_pmfk")
    # prob_for_tsxp = (ts, xpirts) -> y_pmfk
    ts_set = Set(DataRead.get_ts(;age=DateUtil.FOREVER2))
    for (y, m) in yms
        # TODO: could create this filtered file in advance
        oqs_df = filter(:ts => is_ts_backtest(ts_min), DataRead.get_options(y, m))
        if isempty(oqs_df)
            println("No data found for $(y)-$(m)")
            continue
        end
        sdfs = groupby(oqs_df, :ts)
        itr = isnothing(inds) ? sdfs :
              inds isa Integer ? sdfs[inds:end] : sdfs[inds]
        count = length(itr)
        println("Processing $(count) in $(y)-$(m)")
        # stop = ThreadUtil.loop(itr) do sdf
        # rs_for_xpirts = Dict{DateTime,Vector}()
        for sdf_ts in itr
            ts = first(sdf_ts.ts)
            # TODO: clean up tsx so it doesn't have any after hours things, such as on 2020-11-27
            if !(ts in ts_set)
                println("Skipping extraneous ts $(ts)")
                continue
            end
            ts in ts_set || continue

            curp = C(price_lup[ts])
            vix = C(vix_lup[market_date(ts)])
            ore.update(ts, curp, vix) || continue
            rs_ts = Vector()
            !skip_existing || !haskey(TOP_TS, ts) || continue
            # println("Processing ts:$(ts)")

            # ctx = ore.make_ctx(ts, xpirts -> prob_for_tsxp(ts, xpirts), curp)
            ctx = ore.make_ctx(ts, curp, vix, use_pos)

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

            sdf_xpir = groupby(sdf_ts, :expir)

            for sdf in sdf_xpir[xpir_inds]
                xpirts = first(sdf.expir)
                xpir = Date(xpirts)
                xpir_price = price_lup[xpirts]
                0 < bdays(ts, xpir) <= max_bdays_out || continue
                oqs = try
                    to_oqs(sdf)
                catch e
                    if e isa AssertionError
                        println("AssertionError for $(ts) $(xpirts)")
                        continue
                    else
                        rethrow(e)
                    end
                end
                oqss = ChainUtil.oqssEntry(oqs, curp; minlong=C(0.03), minshort=C(0.03))

                # pos = get!(ore.pos_new, XPIR_POS, xpirts)
                top_xpir = get!(Vector, TOP_XPIRTS, xpirts) # do; Vector() end
                # pos = use_pos ? top_xpir : EMPTY_VECTOR
                res = ore.findkel(ctx, xpirts, oqs, oqss, top_xpir, incs)
                # isnothing(res) && @goto stop
                # global kres = res

                if !isempty(res) # && res[1].evrate > 1e-4
                    # expres = 10
                    # if length(res) < expres
                    #     println("$(ts) -> $(xpirts): Found $(length(res)) < $(expres) results")
                    # end
                    # showres(tsi_index, ts, sdf, curp, res[end])
                    # showres(tsi_index, ts, sdf, curp, res[1])
                    r1 = res[1]
                    result = df_calc_pnl(r1, sdf, r1.lqs, xpirts; xpir_price) # dat.market_close(SH.getExpir(r1.lms)))
                    # ThreadUtil.runSync(lock_proc) do
                        r = (;ts, curp, r1, result)
                        push!(top_xpir, r)
                        # pos_add(pos, r)
                        push!(rs_ts, r)
                    # end
                else
                    # println("$(ts): No results for $(xpirts)")
                end
            end
            TOP_TS[ts] = rs_ts
            println("Completed ts:$(ts) at $(DateUtil.nowz())")
        end
    end
    # @label stop
end

using LegQuoteTypes
function walk_through(; xpr=1, kws...)
    reset()
    DrawUtil.draw(:hlines, 0; label="zero")
    lqs_all = LegQuote[]
    global klqs = lqs_all
    for i in 1:8
        explore(; kws..., inds=i:i)
        rs = last(collect(TOP_XPIRTS)[xpr])
        push!(lqs_all, rs[i].r1.lqs...)
        sort!(lqs_all; by=SH.getStrike)
        DrawUtil.draw!(:lines, SH.toDraw(lqs_all); label="lqs$(i)")
        DrawUtil.draw!(:vlines, rs[i].curp; label="curp$(i)")
    end
end

function compare_month(y, m)
    reset()
    yms = [(y, m)]
    explore(;yms)
    drawbal(;newfig=true, kelrat=0.5, close_early=false)
    drawbal(;newfig=false, kelrat=0.5, close_early=true)
end

using DictUtil, StatsBase
function drawbal(; newfig=false, kelrat = 0.5, xpirtss=sort!(collect(keys(TOP_XPIRTS))), close_early=true)
    # all = collect(values(ALL_BEST))
    # global dxpirs = Dict{Date,Vector}()
    # for r in all
    #     v = get!(dxpirs, SH.getExpir(r.r1.lms), Vector())
    #     push!(v, r)
    #     # addToKey(d, SH.getExpir(r.r1.lms), r.r1.kel * r.all.pnl_value / r.r1.risk)
    # end
    # xpirs = sort!(collect(keys(dxpirs)))
    # xpirtss = sort!(collect(keys(TOP_XPIRTS)))
    drets = Dict()
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
        rs = filter(TOP_XPIRTS[xpirts]) do r
            # r.r1.probprofit >= .92
            if r.r1.balrat < 0 || r.r1.balrat > 1
                println("broken")
            end
            # r.r1.probprofit >= .95
            # r.r1.balrat < 1
            # r.r1.kelt >= 100.0
            true
        end
        # count = length(rs)
        rets = calc_rets(rs; close_early)
        # drets[xpir] = sum(rets) * kelrat / count
        drets[xpirts] = isempty(rets) ? 0.0 : sum(rets) * kelrat
    end
    # vals = map(x -> d[x], xpirs)
    # acc = accumulate(+, vals)
    # acc = accumulate((a,x) -> (x+1)*a, vals; init=100)
    # DrawUtil.draw(:scatter, xpirs, acc)
    # rets = map(x -> sum(drets[x]), xpirs)
    rets = map(x -> sum(drets[x]), xpirtss)
    # DrawUtil.draw(:scatter, xpirs, rets)
    acc = accumulate((a,x) -> (x+1)*a, rets; init=100)
    global kdrets = drets
    global krets = rets
    global kacc = acc
    if newfig
        DrawUtil.draw(:scatter, xpirtss, acc)
    else
        DrawUtil.draw!(:scatter, xpirtss, acc; label="$(DateUtil.nowz())")
    end
end

function calc_rets(rs; close_early=true)
    return map(rs) do r
        if invalid(r.r1.commit) || invalid(r.r1.kel)
            # @show r.r1.commit r.result.pnl_value r.r1.kel
            @show r.r1.commit r.result.pnl r.r1.kel
            error("invalid commit or kel")
        end
        pnl = if close_early
            close = find_close(r)
            # isnothing(close) ? r.result.pnl_value : close.pnl
            isnothing(close) ? r.result.pnl : close.pnl
        else
            # r.result.pnl_value
            r.result.pnl
        end
        if !(pnl > (-r.r1.commit - 0.0001))
            global kr = r
            # TODO: this found a case where a condor was formed with a long/short of the same contract:
            # ts = DateTime("2020-02-20T20:30:00")
            # xpir = 2020-02-24
            error(@str "pnl less than -commit" pnl r.r1.commit)
        end
        # return (r.r1.balrat * r.r1.kel * pnl / r.r1.commit, pnl)
        return calc_ret(r, pnl)
    end
end

calc_ret(r, pnl=r.result.pnl) = r.r1.balrat * r.r1.kel * pnl / r.r1.commit

invalid(x) = !isfinite(x) || x < 0

function updown(all=get_all())
    vix_lup = DataRead.vix_lookup(;age=DateUtil.FOREVER2)
    # all = get_all()
    tot_count = length(all)
    println("total: $(tot_count)")
    # sort!(all; by=r -> )
    # up = filter(r -> r.result.pnl_value >= 0, all)
    # down = filter(r -> r.result.pnl_value < 0, all)
    up = filter(r -> r.result.pnl >= 0, all)
    down = filter(r -> r.result.pnl < 0, all)
    println("up:")
    stats(up, tot_count; rev=true, vix_lup)
    println("\ndown:")
    stats(down, tot_count; rev=false, vix_lup)
    return
end

Base.show(io::IO, x::Vector{Style.T}) = print(io, '[', join(x, ','), ']')

using StatsBase
function stats(rs, tot_count; rev, vix_lup)
    count = length(rs)
    println("  count/total: $(count) - $(r4(count / tot_count))")
    show_f("styles", r -> SH.getStyle.(r.r1.lqs), rs; rev)
    show_f("bdays_out", bdout, rs; rev)
    show_f("neto_sign", r -> sign(r.r1.neto), rs; rev)
    show_f("spread_width", r -> Pricing.get_spread_width(r.r1.lqs), rs; rev)

    # styles = countmap([SH.getStyle.(r.r1.lqs) for r in rs])
    # print("styles: "); display(styles)
    # bdays_out = countmap([bdout(r) for r in rs])
    # print("bdays_out: "); display(bdays_out)
    # neto_sign = countmap([sign(r.r1.neto) for r in rs])
    # print("neto_sign: "); display(neto_sign)
    # spread_width = countmap([Pricing.get_spread_width(r.r1.lqs) for r in rs])
    # print("spread_width: "); display(spread_width)

    if isempty(rs)
        println("no data")
        return
    end
    kel = quantile(map(r -> r.r1.kel, rs))
    @show kel
    kelt = quantile(map(r -> r.r1.kelt, rs))
    @show kelt
    keltprob = quantile(map(r -> ore.to_val_kelt_prob2(r.r1), rs))
    @show keltprob
    evret = quantile(map(r -> r.r1.evret, rs))
    @show evret
    evrate = quantile(map(r -> r.r1.evrate, rs))
    @show evrate
    probprofit = quantile(map(r -> r.r1.probprofit, rs))
    @show probprofit
    commit = quantile(map(r -> r.r1.commit, rs))
    @show commit
    vix = quantile(map(r -> vix_lup[market_date(r.ts)], rs))
    @show vix
    div_dir = quantile(map(r -> Dates.value(DateUtil.next_ex_date(r.ts) - DateUtil.market_date(r.ts)), rs))
    @show div_dir
    hour_of_day = quantile(map(r -> hour(r.ts), rs))
    @show hour_of_day
    # TODO: look for how many cases of isolated losses, vs. all for that expiration
    return
end

function show_f(label, f_key::Function, v::AbstractArray; agg=sum_pnl_rs, rev=true)
    d = DictUtil.to_dict_of_v(f_key, v)
    global kd = d
    items = sort!(collect(show_d(x, d[x]; agg) for x in keys(d)); rev, by=(x -> x.a))
    println("  ", label)
    for item in items
        println("    $(item.key): $(item.len) - $(item.a)")
    end
end
function show_d(k, v; agg)
    return (;key=k, len=length(v), a=agg(v))
end
sum_pnl_rs(x) = sum(r.result.pnl for r in x)

bdout(r) = bdays(r.ts, SH.getExpir(r.r1.lqs))

get_all() = collect(Iterators.flatten(values(TOP_XPIRTS)))

lurp(date::Date) = lurp(dat.market_close(date))
function lurp(ts::DateTime)
    return dat.pents_to_c(dat.lup_under(ts))
end

using LRUCache
const OQS_TS_CACHE = LRUCache.LRU{DateTime, Vector{OptionQuote}}(;maxsize=100)
function get_oqs(ts::DateTime)
    oqs = get!(OQS_TS_CACHE, ts) do; load_oqs(ts) end
    if isempty(oqs)
        println("WARN: empty oqs in cache")
        oqs = load_oqs(ts)
        OQS_TS_CACHE[ts] = oqs
    end
    return oqs
end
load_oqs(ts) = to_oqs(load_oqs_df(ts))
load_oqs_df(ts::DateTime) = filter(:ts => t -> t == ts, dat.oqs_df(dat.get_ym(ts)...))

makeprob(ts, lms) = makeprob(ts, SH.getExpir(lms))
makeprob(ts::DateTime, xpir::Date) = makeprob(ts, dat.market_close(xpir))
function makeprob(ts::DateTime, xpirts::DateTime)
    kde = pmk.get_kde(ts)
    curp = lurp(ts)
    oqs = get_oqs(ts)
    xpir = Date(xpirts)
    oqs = filter(oq -> SH.getExpir(oq) == xpir, oqs)
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
function save_state(name)
    save_state("top_xpirts-$(name)", TOP_XPIRTS)
    save_state("top_ts-$(name)", TOP_TS)
end
function save_state(name, obj)
    file = File(format"JLD2", "D:/data/tmp/dfe/$(name).jld2")
    save(file, name, obj)
end

function load_state(name)
    loadinto("top_xpirts-$(name)", TOP_XPIRTS)
    loadinto("top_ts-$(name)", TOP_TS)
end
function loadinto(name, into)
    obj = FileIO.load("D:/data/tmp/dfe/$(name).jld2", name)
    empty!(into)
    merge!(into, obj)
end
#endregion Public

#region Local
function reset(;max_bdays_out=8)
    ore.reset()
    empty!(TOP_TS)
    empty!(TOP_XPIRTS)
    # ore.MAX_LEN[] = 0
    ore.init(;max_bdays_out)
    return
end
const TOP_TS = SortedDict{DateTime,Vector}()
const TOP_XPIRTS = SortedDict{DateTime,Vector}()
# const XPIR_POS = Dict{DateTime,Any}()
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

import OptionUtil
quotes_at(xpirts, ts, lqs; kws...) = quotes_at(DataRead.get_options(year(ts), month(ts)), xpirts, ts, lqs; kws...)
function quotes_at(df, xpirts, ts, lqs; xpir_price=nothing)
    df = df[df.ts .== ts .&& df.expir .== xpirts, :]
    # global kargs = (;df, lqs, xpirts)
    return map(lqs) do lq
        style = Int8(SH.getStyle(lq))
        strike = Float32(SH.getStrike(lq))
        r = df[df.style .== style .&& df.strike .== strike, :]
        if size(r, 1) != 1
            println("Unexpected row count looking up quotes for $((;count=size(r, 1), ts, xpirts, style, strike))")
            # global kquotesat = (;df, xpirts, ts, lqs)
            if !isnothing(xpir_price)
                val = OptionUtil.value_at_xpir(style, strike, xpir_price)
                # TODO: is this the right way to make it 0.01 worse
                return (;bid=val, ask=val+0.01f0)
            else
                error("Can't handle missing data")
            end
        end
        r = only(r)
        return (;r.bid, r.ask)
    end
end

function df_calc_pnl(r1, df_ts, lqs, xpirts; xpir_price)
    global kargs = (;df_ts, lqs, xpirts)
    # ts = first(df_ts.ts)
    # at_ts = quotes_at(df_ts, xpirts, ts, lqs)
    # at_xpirts = quotes_at(xpirts, xpirts, lqs; kws...)
    # neto = sum(calc_neto.(at_ts, SH.getSide.(lqs)))
    # netc = sum(calc_netc.(at_xpirts, SH.getSide.(lqs)))

    neto = r1.neto
    # neto = calc_neto.(SH.getQuote.(lqs), SH.getSide.(lqs))
    action_dir = Int.(SH.getSide.(lqs)) .* Int(Action.close)
    netcs = -action_dir .* OptionUtil.value_at_xpir.(SH.getStyle.(lqs), Float32.(SH.getStrike.(lqs)), xpir_price)
    # println(netcs)
    netc = sum(netcs)
    res = (;neto, netc, pnl=(neto + netc), xpir_price)
    return res

    # strikes = SH.getStrike.(lqs)
    # rows = dfrow.(Ref(df), xpirts, strikes)
    # neto = sum(dfneto.(rows, lqs))
    # netc = sum(dfnetc_xpir.(rows, lqs))
    # netc_value = sum(dfnetc_xpir_value.(rows, lqs))
    # curp = dat.pents_to_c(dat.lup_under(xpirts))
    # return (;neto, netc, netc_value, pnl=neto + netc, pnl_value=neto + netc_value, curp)
end
# function dfrow(df, xpirts, strike)
#     s = strike # dat.topents(strike)
#     return only(df[(df.expir .== xpirts) .& (df.strike .== s),:])
# end
# dfneto(dfrow, lm) = dat.pents_to_c(dfrow[dfsym(SH.getStyle(lm), SH.getSide(lm), Action.open)])
# dfnetc(dfrow, lm) = dat.pents_to_c(dfrow[dfsym(SH.getStyle(lm), SH.toOther(SH.getSide(lm)), Action.close)])

import PricingBase
function calc_neto(qt, side)
    dir = Int(side) * Int(Action.open)
    return PricingBase.quote_dir(dir, qt.bid, qt.ask)
end
function calc_netc(qt, side)
    dir = Int(side) * Int(Action.close)
    return PricingBase.quote_dir(dir, qt.bid, qt.ask)
end

# function dfnetc(dfrow, lm)
#     # dir = Int(SH.toOther(SH.getSide(lm))) * Int(Action.close)
#     dir = Int(SH.getSide(lm)) * Int(Action.close)
#     bidask_col = dfsym_bidask(SH.getStyle(lm), dir)
#     bidask = dfrow[bidask_col]
#     res = -dir * dat.pents_to_c(bidask)
#     # @show dir bidask_col bidask res
#     return res
# end

dfnetc_xpir(dfrow, lm) = dat.pents_to_c(dfrow[dfsym_xpir_price(SH.getStyle(lm), SH.toOther(SH.getSide(lm)))])
dfnetc_xpir_value(dfrow, lm) = Int(SH.getSide(lm)) * dat.pents_to_c(dfrow[dfsym_xpir_value(SH.getStyle(lm))])

# dfsym(style, side, action) = Symbol("$(style)_$(side)_$(SH.toCode(action))")
dfsym_bidask(style, dir) = Symbol("$(style)_$(dir == 1 ? "ask" : "bid")")
dfsym_xpir_price(style, side) = Symbol("$(style)_xpir_price_$(side)")
dfsym_xpir_value(style) = Symbol("$(style)_xpir_value")

using OptionTypes, OptionQuoteTypes, OptionMetaTypes, QuoteTypes
function to_oqs(df)
    gdfs = groupby(df, [:ts, :expir, :strike])
    oqs = Vector{OptionQuote}()
    sizehint!(oqs, length(gdfs))
    for gdf in gdfs
        oq1, oq2 = to_oq(gdf)
        push!(oqs, oq1, oq2)
    end
    return oqs
end
function to_oq(gdf)
    # each group should have one put and one call, in that order.
    @assert gdf.style == [-1, 1]
    op = Option(Style.put, Date(gdf.expir[1]), gdf.strike[1])
    oc = Option(Style.call, Date(gdf.expir[2]), gdf.strike[2])
    qp = Quote(gdf.bid[1], gdf.ask[1])
    qc = Quote(gdf.bid[2], gdf.ask[2])
    return OptionQuote(oc, qc, OptionMetaTypes.MetaZero), OptionQuote(op, qp, OptionMetaTypes.MetaZero)
end

# to_oqs(df::DataFrames.AbstractDataFrame) = collect(Iterators.flatten(map(to_oq_pair, eachrow(df))))
# function to_oq_pair(row)
#     oc = Option(Style.call, Date(row.expiration), dat.pents_to_c(row.strike))
#     op = Option(Style.put, Date(row.expiration), dat.pents_to_c(row.strike))
#     qc = Quote(dat.pents_to_c(row.call_bid), dat.pents_to_c(row.call_ask))
#     qp = Quote(dat.pents_to_c(row.put_bid), dat.pents_to_c(row.put_ask))
#     return OptionQuote(oc, qc, OptionMetaTypes.MetaZero), OptionQuote(op, qp, OptionMetaTypes.MetaZero)
# end
#endregion Local

#region CheckClosing
using CollUtil
import Kelly
import LinesLeg as LL

function find_close(r)
    # tss = filter(:ts => ts -> r.ts < ts < r.r1.xpirts, dat.ts_df()).ts
    # Can't close same day as open
    tss = filter(:ts => ts -> Date(ts) > Date(r.ts) && ts < r.r1.xpirts, dat.ts_df()).ts
    evrate_orig = r.r1.evrate
    for close in (calc_close(r, ts) for ts in tss)
        # TODO: evrate has kel in it? so this comparison isn't quite right.
        # TODO: Maybe give some lenience based on proximity to xpir?
        if !isnothing(close) && (close.rate >= evrate_orig || (isSomething(close.kel) && isnan(close.kel.kel)))
            return close
        end
    end
    return nothing
end

function calc_close(r, ts)
    no_worse_than = -r.r1.commit # r.result.pnl_value
    try
        dfoqs = dat.oqs_df(year(ts), month(ts))
        lqs = r.r1.lqs
        strikes = dat.topents.(SH.getStrike.(lqs))
        df = filter(:ts => t -> t == ts, dfoqs)
        rows = [only(df[(df.expiration .== r.r1.xpirts) .& (df.strike .== s),:]) for s in strikes]
        # rows = dfrow.(Ref(df), xpirts, strikes)
        # rows = filter([:ts, :expiration, :strike] => filter_txs(ts, r.r1.xpirts, strikes), dfoqs)
        # netc = sum(dfnetc.(eachrow(rows), lqs))
        netc = sum(calc_netc.(rows, lqs))
        pnl = r.r1.neto + netc
        if pnl < no_worse_than
            return nothing
        end
        rate = DateUtil.timult(Date(ts), Date(r.r1.xpirts)) * pnl / r.r1.commit
        kel = calc_kel(r, ts)
        return (;ts, netc, pnl, rate, kel)
    catch e
        global kcalc_close = (;r, ts)
        rethrow(e)
    end
end
filter_txs(ts0, xpirts0, strikes0) = (ts, xpirts, strike) -> ts == ts0 && xpirts == xpirts0 && strike in strikes0

function calc_kel(r, ts)
    prob = makeprob(ts, r.r1.xpirts)
    isSomething(prob) || return missing
    buf = Kelly.make_buf()
    segs = LL.toSegments(r.r1.lqs, r.r1.netos)
    return ore.calckel(buf, prob, r.r1.commit, segs)
end
#endregion CheckClosing

function check_r(r)
    buf = Kelly.make_buf()
    segs = LL.toSegments(r.r1.lqs, r.r1.netos)
    df = filter(:ts => ts -> r.ts < ts < DateTime(Date(r.r1.xpirts)) && dat.is_ts_normal(ts), dat.ts_df())
    # (;kel, evret, ev, probprofit)
    res = CollUtil.maparray(df.ts) do ts
        prob = makeprob(ts, r.r1.xpirts)
        return ore.calckel(buf, prob, r.r1.commit, segs)
    end
    insertcols!(df, pairs(res)...)
    transform!(df, :under => (x -> dat.pents_to_c.(x)) => :under)
    # return DataFrame((;tss, calcs...))
end

end
