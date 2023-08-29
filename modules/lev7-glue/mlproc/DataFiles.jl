module DataFiles
#region Imports
using Dates, Base.Threads
import ThreadPools
using DelimitedFiles, DataFrames
using Arrow
using DataStructures
import StatsBase:mean
using DateUtil
using BaseTypes, SmallTypes
import SH, OptionUtil, CollUtil, Pricing
import Calendars
using ThreadUtil
#endregion


############## TODO: 2010, 1 is not loading things that are within xpirs range of the next month? #######


#region Public
ts_df() = DataFrame(ts_table(); copycols=false)
tsx_df() = DataFrame(tsx_table(); copycols=false)
# oq_df() = DataFrame(oq_table(); copycols=false)

function tsx_for_prob(ts::DateTime; len=10000)
    df = get_tsx()
    ind = searchsortedlast(df.ts, ts - Second(1))
    if ind < len
        @show len length(df.ts) ind ts
        error("Insuffient events previous to ts for prob")
    end
    df = df[(ind - len + 1):ind,:]
    rename!(df, :vol => :extrindt, :logvol => :vol)
end

filter_tsx_prob(ts) = second(ts) == 0 && ts != Calendars.getMarketClose(Date(ts))
const TSX_CACHE2 = Ref{Union{DataFrame,Nothing}}(nothing)
function get_tsx()
    if isnothing(TSX_CACHE2[])
        df = filter(:ts => filter_tsx_prob, tsx_df())
        dropmissing!(df, [:ret, :logret])
        TSX_CACHE2[] = df
    else
        df = TSX_CACHE2[]
    end
    return df
end

estimate_min_ts(;len=10000) = ts_table().ts[len+1]
#endregion Public

#region Local

#region Consts
const XPIRS_RANGE2 = 0:20
const NTM_INDS = 1:4
const TS_MAX = DateTime("2022-09-30T20:00:00")
#endregion

#region Loaders

#region Paths
basepath() = joinpath("C:\\", "data", "db")
baseincoming() = joinpath(basepath(), "incoming", "optionsdx")
path_chains() = joinpath(basepath(), "market", "spy", "chains")
path_calls(y, m) = joinpath(path_chains(), "calls", "calls-$(datestr(y, m)).arrow")
path_puts(y, m) = joinpath(path_chains(), "puts", "puts-$(datestr(y, m)).arrow")
path_tsxs() = joinpath(basepath(), "market", "spy", "tsxs")
path_tsxs(y, m) = joinpath(path_tsxs(), "tsx-$(datestr(y, m)).arrow")
path_oqs_pre() = joinpath(basepath(), "market", "spy", "oqs_pre")
path_oqs_pre(y, m) = joinpath(path_oqs_pre(), "oq-$(datestr(y, m)).arrow")
path_oqs() = joinpath(basepath(), "market", "spy", "oqs")
path_oqs(y, m) = joinpath(path_oqs(), "oq-$(datestr(y, m)).arrow")

path_ts() = joinpath(basepath(), "market", "spy", "ts.arrow")
path_xpir() = joinpath(basepath(), "market", "spy", "xpir.arrow")
path_tsx() = joinpath(basepath(), "market", "spy", "tsx.arrow")
path_oq() = joinpath(basepath(), "market", "spy", "oq.arrow")
#endregion

#region Tables
calls_table(y, m) = Arrow.Table(path_calls(y, m))
puts_table(y, m) = Arrow.Table(path_puts(y, m))
tsxs_table(y, m) = Arrow.Table(path_tsxs(y, m))
tsxs_df(y, m) = DataFrame(tsxs_table(y, m); copycols=false)
oqs_pre_table(y, m) = Arrow.Table(path_oqs_pre(y, m))
oqs_pre_df(y, m) = DataFrame(oqs_pre_table(y, m); copycols=false)
oqs_table(y, m) = Arrow.Table(path_oqs(y, m))
oqs_df(y, m) = DataFrame(oqs_table(y, m); copycols=false)

ts_table() = Arrow.Table(path_ts())
xpir_table() = Arrow.Table(path_xpir())
tsx_table() = Arrow.Table(path_tsx())
oq_table() = Arrow.Table(path_oq())
#endregion

#region Indexes
function ts_indexed()
    df = ts_df()
    index = SortedDict{DateTime,Int}(zip(df.ts, eachindex(df.ts)))
    return (;df, index)
end
ts_rows(dftsi, tss) = dftsi.df[getindex.(Ref(dftsi.index), tss),:]
lup_under(dftsi, tss) = map(lup_under, Ref(dftsi), tss)
function lup_under(dftsi, ts::DateTime)
    ind = get(dftsi.index, ts, missing)
    return ismissing(ind) ? missing : dftsi.df.under[ind]
end
# dftsi.df.under[get.(Ref(dftsi.index), tss, missing)]

function tsx_indexed()
    df = tsx_df()
    index = SortedDict{Tuple{DateTime,DateTime},Int}(zip(zip(df.ts, df.expiration), eachindex(df.ts)))
    return (;df, index)
end
tsx_rows(dftsxi, tss, xpirtss) = dftsxi.df[getindex.(Ref(dftsxi.index), tuple.(tss, xpirtss)),:]
# tex(tsxi, ts, xpirts) = tsxi.df.tex[tsxi.index[(ts, xpirts)]]

# function oq_indexed(df = oq_df())
#     index = SortedDict{Tuple{DateTime,DateTime,Int},Int}(zip(zip(df.ts, df.expiration, df.strike), eachindex(df.ts)))
#     return (;df, index)
# end
# # oqi_rows(dfoqi, tss, xpirtss, strikes) = dfoqi.df[getindex.(Ref(dfoqi.index), tuple.(tss, xpirtss, strikes)),:]
# oqi_row(dfoqi, tss, xpirtss, strikes) = map(eachindex(tss)) do i
#     ind = get(dfoqi.index, tuple(tss[i], xpirtss[i], strikes[i]), missing)
#     return ismissing(ind) ? missing : dfoqi.df[ind,:]
# end

import LRUCache
const OQI_TYPE = NamedTuple{(:df, :index), Tuple{DataFrame, SortedDict{Tuple{DateTime, DateTime, Int64}, Int64}}}
const oqi_cache2 = Ref(LRUCache.LRU{Tuple{Int,Int}, OQI_TYPE}(;maxsize=6))
# oqi_rows(dfoqi, tss, xpirtss, strikes) = dfoqi.df[getindex.(Ref(dfoqi.index), tuple.(tss, xpirtss, strikes)),:]
# map(eachindex(tss)) do i
function oqi_row(ts, xpirts, strike)
    ym = get_ym(ts)
    dfoqi = get!(oqi_cache2[], ym) do
        println("Loading oqs_df $(ym)")
        df = oqs_pre_df(ym...)
        return (;df, index=SortedDict{Tuple{DateTime,DateTime,Int},Int}(zip(zip(df.ts, df.expiration, df.strike), eachindex(df.ts))))
    end

    ind = get(dfoqi.index, tuple(ts, xpirts, strike), missing)
    return ismissing(ind) ? missing : dfoqi.df[ind,:]
end

#endregion Indexes

#endregion Loaders

#endregion Local

#region ChainsToOq
function oq_post(yms=make_yms())
    ThreadUtil.loop(yms) do (y, m)
        df = oqs_pre_df(y, m)
        transform!(df, [:strike, :expiration] => calc_xpir_price => [:call_xpir_price_long, :call_xpir_price_short, :put_xpir_price_long, :put_xpir_price_short])
        println("Added pricing for $((;y, m))")
        Arrow.write(path_oqs(y, m), df)
    end
end

function chains_to_oq_pre(yms=make_yms())
    stop = false
    ThreadPools.twith(ThreadPools.QueuePool(2, 11)) do pool
        ThreadPools.@tthreads pool for (y, m) in yms
            !stop || return
            thid = threadid()
            try
                df = proc_oq(y, m)
                Arrow.write(path_oqs_pre(y, m), df)
                println("Expiration count for $((y,m)): $(size(unique(df.expiration), 1))")
                println("Thread $(thid) completed $((y,m)) - df size $(size(df, 1))")
                yield()
            catch e
                stop = true
                println("Exception thrown for $((y,m)), thid:$(thid)")
                rethrow(e)
            end
        end
    end
    return
end

function proc_oq(y, m)
    tslup = ts_indexed()
    f_xpir_value = calc_xpir_value(tslup)
    dfcalls = filter([:ts, :expiration] => filter_xpirs, make_oq_df(calls_table(y, m)))
    dfputs = filter([:ts, :expiration] => filter_xpirs, make_oq_df(puts_table(y, m)))
    df = innerjoin(dfcalls, dfputs; on=[:ts,:expiration,:strike], renamecols = prefix("call_") => prefix("put_"))
    # transform!(df, [:call_bid, :call_ask] => price_all => [:call_long_o, :call_long_c, :call_short_o, :call_short_c])
    # transform!(df, [:put_bid, :put_ask] => price_all => [:put_long_o, :put_long_c, :put_short_o, :put_short_c])
    # transform!(df, [:strike, :expiration] => f_xpir_value => [:call_xpir_value, :put_xpir_value])
    transform!(df, [:call_bid, :call_ask] => price_all => [:call_long_o, :call_long_c, :call_short_o, :call_short_c],
                   [:put_bid, :put_ask] => price_all => [:put_long_o, :put_long_c, :put_short_o, :put_short_c],
                   [:strike, :expiration] => f_xpir_value => [:call_xpir_value, :put_xpir_value])
    return df
end

using StructArrays
calc_xpir_value(tslup) = function(strikes, xpirtss)
    under_at_xpirs = lup_under(tslup, xpirtss)
    m = Matrix{Union{Int32,Missing}}(undef, length(strikes), 2)
    for i in eachindex(strikes)
        under = under_at_xpirs[i]
        try
        if ismissing(under)
            m[i,1] = missing
            m[i,2] = missing
        else
            strike = strikes[i]
            m[i,1] = Pricing.netExpired(Style.call, strike, under)
            m[i,2] = Pricing.netExpired(Style.put, strike, under)
        end
        catch e
            println("exception inside value, $(under)")
            rethrow(e)
        end
    end
    return m

    # # TODO: maybe use previous under if expir under not there?
    # under_at_xpirs = lup_under(tslup, xpirtss)
    # i = 0
    # res = map(StructArray(u=under_at_xpirs, s=strikes)) do sa
    #     i += 1
    #     yield()
    #     # if i > 10000
    #     #     error("stop")
    #     # end
    #     # if ismissing(sa.u)
    #     #     println("missing")
    #     #     return missing, missing
    #     # else
    #     #     println("not missing")
    #     # end
    #     !ismissing(sa.u) || return (missing, missing)
    #     return Pricing.netExpired(Style.call, sa.s, sa.u),
    #            Pricing.netExpired(Style.put, sa.s, sa.u)
    # end
    # println("after map")
    # yield()
    # ret = StructArrays.components(res)
    # println("returning: $(typeof(ret))")
    # return nothing
    # return ret


    # # !ismissing(under_at_xpirs) || return fill(missing, length(strikes), 2)
    # # # if ismissing(under_at_xpir)
    # #     println("what $(under_at_xpirs) $(ismissing(under_at_xpirs))")
    # # # end
    # # callval = Pricing.netExpired.(Style.call, strikes, under_at_xpirs)
    # # putval = Pricing.netExpired.(Style.put, strikes, under_at_xpirs)
    # # return [callval putval]
end

function calc_xpir_price(strikes, xpirtss)
    # rs = oqi_row(oqlup, xpirtss, xpirtss, strikes)
    m = Matrix{Union{Int32,Missing}}(undef, length(strikes), 4)
    for i in eachindex(strikes)
        xpirts = xpirtss[i]
        xpirts <= TS_MAX || ( m[i,:] .= missing ; continue )
        r = oqi_row(xpirts, xpirts, strikes[i])
        if ismissing(r)
            m[i,:] .= missing
        else
            m[i,:] .= [r.call_long_c, r.call_short_c, r.put_long_c, r.put_short_c]
            # m[i,1] = r.call_long_c # price_long_pents.(r.call_bid, r.call_ask, true)
            # m[i,2] = r.call_short_c # price_short_pents.(r.call_bid, r.call_ask, true)
            # m[i,3] = r.put_long_c # price_long_pents.(r.put_bid, r.put_ask, true)
            # m[i,4] = r.put_short_c # price_short_pents.(r.put_bid, r.put_ask, true)
            # [:call_xpir_price_long, :call_xpir_price_short, :put_xpir_price_long, :put_xpir_price_short]
        end
    end
    return m

    # under_at_xpirs = lup_under(tslup, xpirtss)
    # m = Matrix{Int32}(undef, length(strikes), 2)
    # under_at_xpirs = lup_under(tslup, xpirtss)
    # for i in eachindex(strikes)
    #     under = under_at_xpirs[i]
    #     if ismissing(under)
    #         m[i,1] = missing
    #         m[i,2] = missing
    #     else
    #         strike = strikes[i]
    #         m[i,1] = Pricing.netExpired(Style.call, strike, under)
    #         m[i,2] = Pricing.netExpired(Style.put, strike, under)
    #     end
    # end
    # return m


    # rs = oqi_row(oqlup, xpirtss, xpirtss, strikes)
    # call_long = price_long_pents.(rs.call_bid, rs.call_ask, true)
    # call_short = price_short_pents.(rs.call_bid, rs.call_ask, true)
    # put_long = price_long_pents.(rs.put_bid, rs.put_ask, true)
    # put_short = price_short_pents.(rs.put_bid, rs.put_ask, true)
    # return [call_long call_short put_long put_short]
end

function make_oq_df(tbl)
    return DataFrame(:ts => tbl.ts, :expiration => tbl.expiration, :strike => tbl.strike, :bid => tbl.bid, :ask => tbl.ask; copycols=false)
end
#endregion

#region ChainsToTsx
combine_tsx() = combine_dfs(tsxs_df, [:ts, :expiration], path_tsx())

const negative_extrin_count = ThreadUtil.Atomic{Int}(0)

mid(x1, x2) = (x1 .+ x2) ./ 2
logdot(x) = log.(x)
# calc_vol(extrin, tex) = extrin ./ sqrt(tex)
calc_extrindt(extrin, tex) = extrin ./ tex
calc_extrindt(extrin_c::Real, extrin_p::Real, tex::Real) = mid(extrin_c, extrin_p) / tex
calc_vol(extrindts) = log.(extrindts)

function calc_vol(curp::Real, tex::Real, oqs)
    oqs_call = filter(isCall, oqs)
    oqs_put = filter(isPut, oqs)
    xtrin_call = calc_xtrin(Style.call, curp, oqs_call)
    xtrin_put = calc_xtrin(Style.put, curp, oqs_put)
    @show xtrin_call xtrin_put
    return calc_vol(calc_extrindt(xtrin_call, xtrin_put, tex))
end

function chains_to_tsx(yms=make_yms())::Nothing
    @atomic negative_extrin_count.count = 0
    lupp = ts2under_lupp()

    ThreadPools.twith(ThreadPools.QueuePool(2, 11)) do pool
        ThreadPools.@tthreads pool for (y, m) in yms
            try
                thid = threadid()
                df = proc_tsx(y, m, lupp)
                Arrow.write(path_tsxs(y, m), df)
                println("Expiration count for $((y,m)): $(size(unique(df.expiration), 1))")
                println("Thread $(thid) completed $((y,m)) - df size $(size(df, 1))")
                yield()
            catch e
                println("Exception thrown for $((y,m)), thid:$(thid)")
                rethrow(e)
            end
        end
    end
    # println("Finished threads $(length.((thr_dfs_ts, thr_dfs_xpir, thr_dfs_ntm)))")
    # combineall(thr_dfs_xpir, thr_dfs_ntm)
    return
end

function proc_tsx(y, m, lupp)
    fntm_call = f_calc_xtrin(Style.call)
    fntm_put = f_calc_xtrin(Style.put)
    dfcalls = chains_to_tsx!(DataFrame(calls_table(y, m); copycols=false), lupp, fntm_call)
    dfputs = chains_to_tsx!(DataFrame(puts_table(y, m); copycols=false), lupp, fntm_put)
    df = innerjoin(dfcalls, dfputs; on=[:ts,:expiration], renamecols = "_c" => "_p")
    if !(size(dfcalls, 1) == size(dfputs, 1) == size(df, 1))
        println("DataFrame sizes didn't match")
    end
    transform!(df, [:extrin_c, :extrin_p] => mid => :extrin)
    select!(df, :ts, :expiration, :tex_c => :tex, :ret_c => :ret, :extrin,
            [:extrin, :tex_c] => calc_extrindt => :extrindt, [:iv_mean_c, :iv_mean_p] => mid => :iv_mean)
    transform!(df, :extrindt => calc_vol => :vol, :ret => logdot => :logret)
    return df
end

function chains_to_tsx!(dfro, lupp, fntm)
    df = filter([:ts, :expiration] => filter_xpirs, dfro)
    # df.strikedist = abs.(df.under .- df.strike)
    # sort!(df, [:ts,:expiration,:strikedist])
    # println("ntmdf filter/sort done $(size(ntmdf, 1))")

    gdf = groupby(df, [:ts,:expiration])
    # gdf = combine(gdf, sdf -> first(sdf, NTM_INDS); ungroup=false)
    df = combine(gdf,
        # [:ts,:expiration,:under,:strike,:bid,:ask] => fntm => [:tex,:extrin,:vol,:logvol],
        [:under,:strike,:bid,:ask] => fntm => :extrin,


        # [:ts,:expiration,:under] => ( function(tss, xpirs, unders)
        #     ret = get(lup, first(xpirs), missing) / first(unders)
        #     logret = log(ret)
        #     return [ret logret]
        # end) => [:ret,:logret],

        :iv => mean => :iv_mean

        ; threads=false
    )

    f_ret = (xpirts, under) -> calc_ret(lupp, xpirts, under)
    transform!(df, [:ts, :expiration] => (ts, xpirts) -> Calendars.calcTex => :tex,
                   [:ts, :expiration, :under] => f_ret => :ret,
    )

    return df
end

# function(tss, xpirs, unders)
#     ret = get(lupp, first(xpirs), missing) / Float64(first(unders))
#     return ret
# end
calc_ret(lupp, xpirts, under) = get(lupp, xpirts, missing) / Float64(under)

# function combineall(dfs_xpir, dfs_ntm)::Nothing
#     # TODO: maybe don't need spawn because dataframe sort might multithread?
#     _xpirs = @spawn unique!(sort!(reduce(vcat, Iterators.flatten(dfs_xpir)), :xpir))
#     _tstoxpir = @spawn sort!(reduce(vcat, Iterators.flatten(dfs_ntm)), [:ts,:expiration])
#     dfxpirs = fetch(_xpirs)
#     dftstoxpir = fetch(_tstoxpir)
#     Arrow.write(path_xpirs(), dfxpirs)
#     Arrow.write(path_tsx(), dftstoxpir)
#     @assert isnothing(findfirst(x -> x < 0, dftstoxpir.vol))
#     return
# end

# filter_xpirs(ts, xpir) = bdays(Date(ts), Date(xpir)) in XPIRS_RANGE
filter_xpirs(ts, xpir) = Dates.value(Date(xpir) - Date(ts)) in XPIRS_RANGE2

# # This must match what happens in chains_to_tsx!
# f_strikedist(curp) = strike -> abs(curp - strike)
# function prep_oqs(oqs)
#     sort(oqs; by=strikedist(curp))
# end

f_calc_xtrin(style) = (unders, strikes, bids, asks) -> calc_xtrin(style, first(unders), strikes, bids, asks)
# calc_ntm_data(style, tss, xpirs, unders, strikes, bids, asks) = calc_ntm_data(style, first(tss), first(xpirs), first(unders), strikes, bids, asks)

# function calc_xtrin(style, under::Real, strikes, bids, asks)
#     inds = sortperm(strikes; rev=true, by=x -> abs(x - curp))[NTM_INDS]
#     strikes = strikes[inds]
#     bids = bids[inds]
#     asks = asks[inds]
#     return calc_xtrin_top(style, under, strikes, bids, asks)
# end

function calc_xtrin(style, under::Real, oqs_style)
    oqs_sorted = sort(oqs_style; by=oq -> abs(under - SH.getStrike(oq)))[NTM_INDS]
    return calc_xtrin_top(style, under, SH.getStrike.(oqs_sorted), SH.getBid.(oqs_sorted), SH.getAsk.(oqs_sorted))
end

function calc_xtrin_top(style, under::Real, strikes, bids, asks)
    # @show under strikes bids asks
    extrins = OptionUtil.calc_extrin(style, under, strikes, bids, asks)
    extrins ./= under
    total = CZ
    mean_count = length(extrins)
    neg_count = 0
    for i in eachindex(extrins)
        x = extrins[i]
        if x < 0
            extrins[i] = CZ
            neg_count += 1
            mean_count -= 1
        else
            total += x
        end
    end
    # TODO: track down if neg xtrins is just bad data, or if a bug
    # if !iszero(neg_count)
    #     @atomic negative_extrin_count.count += 1
    #     if Second(ts) != 10 && ts != Calendars.getMarketClose(Date(ts))
    #         # Only expect weird values at open or close
    #         println("**** Found neg extrin not market open/close $(ts) ****")
    #         global kerr = (;style, ts, xpirts, under, extrins, strikes, bids, asks)
    #     end
    # end
    # TODO: maybe should use geometric/harmonic mean?
    extrin = iszero(mean_count) ? CZ : total / mean_count
    # vol = extrin / tex
    # logvol = log(vol)
    # return [tex extrin vol logvol]
    return extrin
end

# function combine_ntmdf(dfs_ntm)
#     lup = ts2under_lup()
#     dfntm = sort!(reduce(vcat, dfs_ntm))
#     dfntm.ret = get.(lup, dfntm.expiration) ./ dfntm.under
#     dfntm.logret = log.(dfntm.ret)
#     combine(groupby(dfntm, [:ts,:expiration]),  ; renamecols=false)
#     ret, tex, volatility
#     return (dfntm, other)
# end

# function combine_to4_call(sdf)
#     sort!(sdf, :strikedist)
#     keepat!(sdf, 1:4)
#     sdf4.extrin = OptionUtil.extrin_call(sdf4.under, sdf4.strike, sdf4.bid, sdf4.ask)
#     sdf4.vol = mean(sdf4.extrin)
#     # TODO: remove when confirm it works
#     @assert isnothing(findfirst(x -> x < 0; sdf4.vol))
#     # return sdf4
#     return
# end

# function combine_to4_put(sdf)
#     sort!(sdf, :strikedist)
#     keepat!(sdf, 1:4)
#     sdf4.extrin = OptionUtil.extrin_put(sdf4.under, sdf4.strike, sdf4.bid, sdf4.ask)
#     sdf4.vol = mean(sdf4.extrin)
#     # TODO: remove when confirm it works
#     @assert isnothing(findfirst(x -> x < 0; sdf4.vol))
#     # return sdf4
#     return
# end
#endregion

#region ChainsToArrow
function chains_to_arrow(yms::Coll{NTuple{2,Int}})
    # ThreadPools.twith(ThreadPools.QueuePool(2, floor(Int, (Threads.nthreads() - 1)/2))) do pool
    ThreadPools.twith(ThreadPools.QueuePool(2, 12)) do pool
        ThreadPools.@tthreads pool for (y, m) in yms
            println("Thread $(Threads.threadid()) processing $((;y,m))...")
            chains_to_arrow(y, m)
            println("... thread $(Threads.threadid()) completed $((;y,m))")
        end
    end
    return
end

function chains_to_arrow(y::Int, m::Int)
    mkpath(path_chains())
    df = dfincoming(y, m)
    println("Loaded dataframe for $((;y,m))")
    transformraw(df)
    println("Transformed dataframe for $((;y,m))")
    dfcalls = extractdf(df, "c_")
    dfputs = extractdf(df, "p_")
    println("calls and puts extracted for $((;y,m))")
    Arrow.write(path_calls(y, m), dfcalls)
    Arrow.write(path_puts(y, m), dfputs)
    println("arrow files written for $((;y,m))")
end
#endregion

#region ChainsToArrowLocal
const DF_FILE = DateFormat("yyyymm")

datestr(y, m) = Dates.format(Date(y, m, 1), DF_FILE)
function pathincoming(y, m)
    if y >= 2016
        joinpath(baseincoming(), "spy_15x_$(y)", "spy_15x_$(datestr(y, m)).txt")
    else
        joinpath(baseincoming(), "spy_30x_$(y)", "spy_30x_$(datestr(y, m)).txt")
    end
end

# "quote_unixtime, underlying_last, expire_unix, c_delta, c_gamma, c_vega, c_theta, c_rho, c_iv, c_volume, c_last, c_size, c_bid, c_ask, strike, p_bid, p_ask"
COLS_FINAL = [:ts, :under, :expiration, :strike, :bid, :ask, :last, :volume, :size, :delta, :gamma, :vega, :theta, :rho, :iv]
COLS_COMMON = Dict(:quote_unixtime => :ts, :underlying_last => :under, :expire_unix => :expiration, :strike => :strike)
function extractdf(df, prefix)
    colnames = vcat(collect(keys(COLS_COMMON)), Symbol.(filter(x -> startswith(x, prefix), names(df))))
    # df = filter!(r -> !ismissing(r[prefix * "bid"]) && !ismissing(r[prefix * "ask"]), select(df, colnames))
    df = select(df, colnames)
    rename!(df) do colname
        startswith(colname, prefix) ? colname[3:end] : COLS_COMMON[Symbol(colname)]
    end
    dropmissing!(df, [:bid, :ask]; disallowmissing=true)
    return select!(df, COLS_FINAL)
end

import CSV
# ArrowTypes.toarrow(x::SubString{CSV.PosLenString}) = String(x)
function dfincoming(y, m)
    path = pathincoming(y, m)
    # data, header = readdlm(path, ','; header=true)
    # return DataFrame(data, vec(header))
    return CSV.read(path, DataFrame; stringtype=String)
end

fixcolname(col) = occursin('[', col) ? lowercase(strip(col)[2:end-1]) : col

tofloat(x::Float64) = Float32(x)
tofloat(x::AbstractString) = isempty(strip(x)) ? missing : tofloat_(x)
tofloat_(x) = parse(Float32, x)
topents(x::Float64) = round(Int32, x * 1000)
topents(x::AbstractString) = isempty(strip(x)) ? missing : topents(parse(Float64, x))
totimestamp(x::Int) = unix2datetime(x)
topents(x::Currency) = Int32(x * 1000)
pents_to_c(x::Int32) = C(x / 1000)

# [QUOTE_UNIXTIME], [QUOTE_READTIME], [QUOTE_DATE], [QUOTE_TIME_HOURS], [UNDERLYING_LAST], [EXPIRE_DATE], [EXPIRE_UNIX], [DTE], [C_DELTA], [C_GAMMA], [C_VEGA], [C_THETA], [C_RHO], [C_IV], [C_VOLUME], [C_LAST], [C_SIZE], [C_BID], [C_ASK], [STRIKE], [P_BID], [P_ASK], [P_SIZE], [P_LAST], [P_DELTA], [P_GAMMA], [P_VEGA], [P_THETA], [P_RHO], [P_IV], [P_VOLUME], [STRIKE_DISTANCE], [STRIKE_DISTANCE_PCT]
COLS_DROP = ["quote_readtime", "quote_date", "quote_time_hours", "expire_date", "dte", "strike_distance", "strike_distance_pct"]
COLS_PROC = Dict(
    "quote_unixtime" => totimestamp,
    "expire_unix" => totimestamp,
    "underlying_last" => topents,
    "strike" => topents
)

function transformraw(df)
    rename!(fixcolname, df)
    select!(df, Not(Symbol.(COLS_DROP)))
    for colname in names(df)
        if haskey(COLS_PROC, colname)
            df[!,colname] = (COLS_PROC[colname]).(df[!,colname])
        elseif occursin("size", colname)
            # do nothing
            df[!,colname] = strip.(df[!,colname])
        elseif occursin("bid", colname) || occursin("ask", colname)
            df[!,colname] = topents.(df[!,colname])
        elseif occursin("c_", colname) || occursin("p_", colname)
            df[!,colname] = tofloat.(df[!,colname])
        else
            error("Unexpected col: $(colname)")
        end
    end
end
#endregion

#region ChainsTs
function chains_to_ts()
    thr_tss = [Set{Tuple{DateTime,Int32}}() for _ in 1:nthreads()]
    stop = false
    ThreadPools.twith(ThreadPools.QueuePool(2, 11)) do pool
        ThreadPools.@tthreads pool for (y, m) in make_yms()
            !stop || return
            try
                thid = threadid()
                calls = calls_table(y, m)
                puts = puts_table(y, m)
                union!(thr_tss[thid], zip(calls.ts, calls.under))
                union!(thr_tss[thid], zip(puts.ts, puts.under))
                println("$(thid): Completed $((;y, m))")
                yield()
            catch e
                stop = true
                println("Exception thrown for thid:$(thid), $((;y, m))")
                rethrow(e)
            end
        end
    end
    !stop || return
    println("Finished threads, now combining...")
    tss = reduce(union!, thr_tss)
    df = DataFrame(tss, [:ts,:under])
    sort!(df, :ts)
    # unique!(df, [:ts]) # puts and calls are separate files so will duplicate
    global kdf_tss = df
    @assert isunique(df.ts)
    Arrow.write(path_ts(), df)
    kdf_tss = nothing # if above line throws exception, it's not cleared and we can try writing manually
    return df
end
#endregion

#region ChainsToXpir
function chains_to_xpir()
    thr_xpirs = [Vector{DateTime}() for _ in 1:nthreads()]
    ThreadPools.twith(ThreadPools.QueuePool(2, 11)) do pool
        ThreadPools.@tthreads pool for (y, m) in make_yms()
            thid = Threads.threadid()
            calls = calls_table(y, m)
            puts = puts_table(y, m)
            # CollUtil.pushsortedunique!(thr_xpirs[thid], calls.expiration)
            # CollUtil.pushsortedunique!(thr_xpirs[thid], puts.expiration)
            # unique!(thr_xpirs[thid])
            append!(thr_xpirs[thid], intersect(calls.expiration, puts.expiration))
            println("$(thid): Completed $((;y, m))")
            yield()
        end
    end
    println("Finished threads, now combining...")
    xpirs = reduce(vcat, thr_xpirs)
    sort!(xpirs)
    unique!(xpirs)
    df = DataFrame([xpirs], [:xpir]; copycols=false)
    Arrow.write(path_xpir(), df)
    return df
end
#endregion

#region Pricing
function price_long_pents(bid::Int32, ask::Int32, closing::Bool)
    try
        topents(Pricing.price_long(pents_to_c(-ask), pents_to_c(-bid), closing))
    catch e
        e isa DomainError && return missing
        rethrow(e)
    end
end
function price_short_pents(bid::Int32, ask::Int32, closing::Bool)
    try
        topents(Pricing.price_short(pents_to_c(bid), pents_to_c(ask), closing))
    catch e
        e isa DomainError && return missing
        rethrow(e)
    end
end

# price_long_pents(bids::AbstractVector, asks::AbstractVector, opening::Bool) = map(price_long_pents, bids, asks, opening)
# price_short_pents(bids::AbstractVector, asks::AbstractVector, opening::Bool) = map(price_short_pents, bids, asks, opening)

function price_all(bids::AbstractVector, asks::AbstractVector)
    len = length(bids)
    # res = [Vector{Union{Int32,Missing}}(undef, len) for _ in 1:4]
    res = Matrix{Union{Int32,Missing}}(undef, (len, 4))
    for i in eachindex(bids)
        bid = bids[i]
        ask = asks[i]
        res[i,1] = price_long_pents(bid, ask, false)
        res[i,2] = price_long_pents(bid, ask, true)
        res[i,3] = price_short_pents(bid, ask, false)
        res[i,4] = price_short_pents(bid, ask, true)
    end
    return res
end
#endregion

#region Misc
function make_yms()
    return sort!(filter!(vec([(y, m) for y in 2010:2022, m in 1:12])) do (y, m)
        y != 2022 || m <= 9
    end)
end

function combine_dfs(f_load, sort_check, path=nothing; yms=make_yms())
    dfs = Vector{DataFrame}(undef, length(yms))
    i = 0
    for (y, m) in yms
        i += 1
        dfs[i] = f_load(y, m)
        println("Loaded $((;y, m))")
    end
    println("Loaded dfs")
    df = reduce(vcat, dfs)
    println("Combined dfs")
    if !issorted(df, sort_check)
        println("WARN: TODO: wasn't sorted. Need to fix.")
    end
    isnothing(path) || Arrow.write(path, df)
    return df
end

prefix(pre::AbstractString) = s::AbstractString -> pre * s

get_ym(ts) = (year(ts), month(ts))
#endregion Misc

#region Old
# function update(df, path)
#     if isfile(path)
#         ts1 = df.quote_unixtime[1]
#         a = Arrow.Table(path)
#         if ts1 in a.quote_unixtime
#             println("data already in $(path)")
#         else
#             Arrow.append(path, df)
#         end
#     else
#         open(Arrow.Writer, path) do writer
#             Arrow.write(writer, df)
#         end
#     end
# end
#endregion

end