module DataFiles
#region Imports
using Dates, Base.Threads, ThreadPools
using StatsBase, DelimitedFiles, DataStructures, DataFrames, Arrow
import LRUCache
using BaseTypes, SmallTypes
using DateUtil, ThreadUtil
import SH, DictUtil, OptionUtil, CollUtil, Pricing
import Calendars
using Paths

using Memoization, ThreadSafeDicts
#endregion


############## TODO: 2010, 1 is not loading things that are within xpirs range of the next month? #######
## If tsx_for_prob is missing any times, it will mess up kde


#region Public
# TODO: can remove this after rebuilding from CSV where I added the filter.
filter_bday(df) = filter(:ts => ts -> DateUtil.isBusDay(Date(ts)), df)

ts_df() = loading_currency!(filter_bday(DataFrame(ts_table(); copycols=false)), :under)
ts_allperiods_df(;period=Minute(30)) = loading_currency!(DataFrame(ts_allperiods_table(;period); copycols=false), :under)

tsx_df() = filter_bday(DataFrame(tsx_table(); copycols=false))

vix_df() = loading_currency!(DataFrame(vix_table(); copycols=false), :open, :high, :low, :close)
vix_alldates_df(;period=Minute(30)) = loading_currency!(DataFrame(vix_alldates_table(); copycols=false), :open, :high, :low, :close)

# oq_df() = DataFrame(oq_table(); copycols=false)

function loading_currency!(df, cols...)
    for col in cols
        df[!,col] = pents_to_c.(df[!,col])
    end
    return df
end

function saving_currency!(df, cols...)
    for col in cols
        df[!,col] = topents.(df[!,col])
    end
    return df
end

function convert_cols!(f, df, cols...)
    for col in cols
        df[!,col] = f.(df[!,col])
    end
    return df
end

function tsx_for_prob(ts::DateTime, len::Int)
    df = get_tsx_for_prob()
    ind = searchsortedlast(df.ts, ts - Second(1))
    if ind < len
        @show len length(df.ts) ind ts
        error("Insuffient events previous to ts for prob")
    end
    df = df[(ind - len + 1):ind,:]
    # rename!(df, :vol => :extrindt, :logvol => :vol)
end

filter_tsx_prob(ts) = is_ts_normal(ts)
const TSX_CACHE2 = Ref{Union{DataFrame,Nothing}}(nothing)
function get_tsx_for_prob()
    if isnothing(TSX_CACHE2[])
        df = filter(:ts => filter_tsx_prob, tsx_df())
        dropmissing!(df, [:ret, :logret, :extrin, :extrindt, :vol])
        TSX_CACHE2[] = df
    else
        df = TSX_CACHE2[]
    end
    return df
end

const MIN_TS_EST2 = Dict{Int,DateTime}()
function estimate_min_ts(;len=200000)
    get!(MIN_TS_EST2, len) do
        println("estimate_min_ts: Loading tsx")
        tsx_table().ts[len + div(len, 5) + 1]
    end
end
#endregion Public

#region Local

#region Consts
const XPIRS_RANGE2 = 0:20
const NTM_COUNT = 4
const TS_MAX2 = DateTime("2023-06-30T20:00:00")
#endregion

#region Loaders

#region Paths
baseincoming() = joinpath(PATHS.db_old(), "incoming", "optionsdx")
path_chains() = joinpath(PATHS.db_old(), "market", "spy", "chains")
path_calls(y, m) = joinpath(path_chains(), "calls", "calls-$(datestr(y, m)).arrow")
path_puts(y, m) = joinpath(path_chains(), "puts", "puts-$(datestr(y, m)).arrow")
path_tsxs() = joinpath(PATHS.db_old(), "market", "spy", "tsxs")
path_tsxs(y, m) = joinpath(path_tsxs(), "tsx-$(datestr(y, m)).arrow")
path_oqs_pre() = joinpath(PATHS.db_old(), "market", "spy", "oqs_pre")
path_oqs_pre(y, m) = joinpath(path_oqs_pre(), "oq-$(datestr(y, m)).arrow")
path_oqs() = joinpath(PATHS.db_old(), "market", "spy", "oqs")
path_oqs(y, m) = joinpath(path_oqs(), "oq-$(datestr(y, m)).arrow")

path_ts() = joinpath(PATHS.db_old(), "market", "spy", "ts.arrow")
path_ts_allperiods(;period=Minute(30)) = joinpath(PATHS.db_old(), "market", "spy", "ts-$(Dates.value(period)).arrow")
path_xpir() = joinpath(PATHS.db_old(), "market", "spy", "xpir.arrow")
path_tsx() = joinpath(PATHS.db_old(), "market", "spy", "tsx.arrow")
path_oq() = joinpath(PATHS.db_old(), "market", "spy", "oq.arrow")

path_vix() = joinpath(PATHS.db_old(), "market", "vix", "vix.arrow")
path_vix_alldates() = joinpath(PATHS.db_old(), "market", "vix", "vix_alldates.arrow")
#endregion

#region Tables
calls_table(y, m) = Arrow.Table(path_calls(y, m))
puts_table(y, m) = Arrow.Table(path_puts(y, m))
tsxs_table(y, m) = Arrow.Table(path_tsxs(y, m))
tsxs_df(y, m) = DataFrame(tsxs_table(y, m); copycols=false)
oqs_pre_table(y, m) = Arrow.Table(path_oqs_pre(y, m))
oqs_pre_df(y, m) = DataFrame(oqs_pre_table(y, m); copycols=false)
oqs_table(y, m) = Arrow.Table(path_oqs(y, m))
oqs_df_load(y, m) = filter_bday(DataFrame(oqs_table(y, m); copycols=false))

ts_table() = Arrow.Table(path_ts())
ts_allperiods_table(;period=Minute(30)) = Arrow.Table(path_ts_allperiods(;period))
vix_table() = Arrow.Table(path_vix())
vix_alldates_table() = Arrow.Table(path_vix_alldates())
xpir_table() = Arrow.Table(path_xpir())
tsx_table() = Arrow.Table(path_tsx())
oq_table() = Arrow.Table(path_oq())

# TODO: make it 14
const OQS_CACHE = Ref(LRUCache.LRU{Tuple{Int,Int}, DataFrame}(;maxsize=6))
function oqs_df(y, m)
    ym = (y, m)
    return get!(OQS_CACHE[], ym) do
        println("Loading oqs_df $(ym)")
        return oqs_df_load(y, m)
    end
end
#endregion

#region Indexes
const DFTSI = Ref{Union{Nothing,NamedTuple{(:df, :index), Tuple{DataFrames.DataFrame, DataStructures.SortedDict{DateTime, Int64, Base.Order.ForwardOrdering}}}}}(nothing)
function ts_indexed()
    isnothing(DFTSI[]) || return DFTSI[]
    df = ts_df()
    index = SortedDict{DateTime,Int}(zip(df.ts, eachindex(df.ts)))
    DFTSI[] = (;df, index)
    return DFTSI[]
end
ts_rows(dftsi, tss) = dftsi.df[getindex.(Ref(dftsi.index), tss),:]
lup_under(dftsi, tss::Vector{DateTime}) = [lup_under(dftsi, ts) for ts in tss]
#map(lup_under, Ref(dftsi), tss)
function lup_under(ts::DateTime)
    dftsi = ts_indexed()
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

const OQI_TYPE = NamedTuple{(:df, :index), Tuple{DataFrame, SortedDict{Tuple{DateTime, DateTime, Int64}, Int64}}}
const oqi_cache2 = Ref(LRUCache.LRU{Tuple{Int,Int}, OQI_TYPE}(;maxsize=6))
# oqi_rows(dfoqi, tss, xpirtss, strikes) = dfoqi.df[getindex.(Ref(dfoqi.index), tuple.(tss, xpirtss, strikes)),:]
# map(eachindex(tss)) do i
oqi_row(ts, xpir::Date, strike) = oqi_row(ts, market_close(xpir), strike)
function oqi_row(ts, xpirts::DateTime, strike)
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
    ThreadUtil.loop(yms) do (y, m)
        df = proc_oq(y, m)
        Arrow.write(path_oqs_pre(y, m), df)
        println("Expiration count for $((y,m)): $(size(unique(df.expiration), 1))")
        println("Thread $(thid) completed $((y,m)) - df size $(size(df, 1))")
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
        xpirts <= TS_MAX2 || ( m[i,:] .= missing ; continue )
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

const negative_extrin_count2 = ThreadUtil.Atomic2{Int}(0)

mid(x1, x2) = (x1 .+ x2) ./ 2
logdot(x) = log.(x)
# calc_vol(extrin, tex) = extrin ./ sqrt(tex)
calc_extrindt(extrin, tex) = extrin ./ tex
calc_extrindt(extrin_c::Real, extrin_p::Real, tex::Real) = mid(extrin_c, extrin_p) / tex
function calc_vol(extrindts)
    map(extrindts) do x
        y = log(x)
        isfinite(y) || error("Non finite vol ", x)
        return y
    end
end

function calc_vol(curp::Real, tex::Real, oqs, ts)
    global kcalc_vol_args = (;curp ,tex, oqs, ts)
    isempty(oqs) && error("empty oqs")
    oqs_call = filter(isCall, oqs)
    oqs_put = filter(isPut, oqs)
    isempty(oqs_call) && error("empty oqs")
    isempty(oqs_put) && error("empty oqs")

    xtrin_call = calc_xtrin(Style.call, curp, oqs_call, ts)
    xtrin_put = calc_xtrin(Style.put, curp, oqs_put, ts)
    (!ismissing(xtrin_call) && !ismissing(xtrin_put)) || return missing
    extrindt = calc_extrindt(xtrin_call, xtrin_put, tex)
    @assert isfinite(extrindt) && extrindt > 0 @str xtrin_call xtrin_put
    return calc_vol(extrindt)
end

function chains_to_tsx(yms=make_yms())::Nothing
    @atomic negative_extrin_count2.value = 0
    tsindex = ts_indexed()

    ThreadUtil.loop(yms) do (y, m)
        df = proc_tsx(y, m, tsindex)
        Arrow.write(path_tsxs(y, m), df)
        println("Expiration count for $((y,m)): $(size(unique(df.expiration), 1))")
        println("Thread $(Threads.threadid()) completed $((y,m)) - df size $(size(df, 1))")
    end
    # println("Finished threads $(length.((thr_dfs_ts, thr_dfs_xpir, thr_dfs_ntm)))")
    # combineall(thr_dfs_xpir, thr_dfs_ntm)
    return
end

function proc_tsx(y, m, tsindex)
    fntm_call = f_calc_xtrin(Style.call)
    fntm_put = f_calc_xtrin(Style.put)
    dfcalls = chains_to_tsx!(DataFrame(calls_table(y, m); copycols=false), tsindex, fntm_call)
    dfputs = chains_to_tsx!(DataFrame(puts_table(y, m); copycols=false), tsindex, fntm_put)
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

# TODO: Consider for xtrindt:
# transform!(tsx, [:extrin, :tex] => ((x, t) -> x ./ sqrt.(t) ) => :extrindtsq)
# transform!(tsx, [:extrin, :ts, :expiration] => ( (x, ts, xpirts) -> x ./ sqrt.( Dates.value.(xpirts .- ts) / (1000*60*60*24) ) ) => :extrincalsq)

function chains_to_tsx!(dfro, tsindex, fntm)
    df = filter([:ts, :expiration] => filter_xpirs, dfro)
    # df.strikedist = abs.(df.under .- df.strike)
    # sort!(df, [:ts,:expiration,:strikedist])
    # println("ntmdf filter/sort done $(size(ntmdf, 1))")

    gdf = groupby(df, [:ts,:expiration])
    # gdf = combine(gdf, sdf -> first(sdf, NTM_INDS); ungroup=false)
    df = combine(gdf,
        # [:ts,:expiration,:under,:strike,:bid,:ask] => fntm => [:tex,:extrin,:vol,:logvol],
        [:ts, :expiration, :under,:strike,:bid,:ask] => fntm => :extrin,


        # [:ts,:expiration,:under] => ( function(tss, xpirs, unders)
        #     ret = get(lup, first(xpirs), missing) / first(unders)
        #     logret = log(ret)
        #     return [ret logret]
        # end) => [:ret,:logret],

        :under => first => :under,

        :iv => mean => :iv_mean

        ; threads=false
    )

    f_ret = (xpirtss, unders) -> calc_ret(tsindex, xpirtss, unders)
    transform!(df, [:ts, :expiration] => ((tss, xpirtss) -> Calendars.calcTex.(tss, xpirtss)) => :tex,
                   [:expiration, :under] => f_ret => :ret
    )

    return df
end

# function(tss, xpirs, unders)
#     ret = get(lupp, first(xpirs), missing) / Float64(first(unders))
#     return ret
# end
# calc_ret(lupp, xpirts, under) = get(lupp, xpirts, missing) / Float64(under)
calc_ret(tsindex, xpirtss, unders) = lup_under(tsindex, xpirtss) ./ unders
# (@show typeof(xpirtss) typeof(unders) ;

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

f_calc_xtrin(style) = function(tss, expirations, unders, strikes, bids, asks)
    calc_xtrin(style, first(unders), strikes, bids, asks, (;ts=first(tss), xpir=Date(first(expirations))))
end

function calc_xtrin(style, under::Real, strikes, bids, asks, ctx)
    inds = first(sortperm(strikes; by=x -> abs(x - under)), NTM_COUNT)
    strikes = strikes[inds]
    bids = bids[inds]
    asks = asks[inds]
    return calc_xtrin_top(style, under, strikes, bids, asks, ctx)
end

function calc_xtrin(style, under::Real, oqs_style, ts)
    isempty(oqs_style) && error("empty oqs")
    oqs_sorted = first(sort(oqs_style; by=oq -> abs(under - SH.getStrike(oq))), NTM_COUNT)
    global koqsx = oqs_sorted
    global kxtrin_args = (;style, under, oqs_style, ts)
    @assert isnothing(findfirst(oq -> SH.getExpir(oq) != SH.getExpir(oqs_sorted[1]), oqs_sorted))
    ctx = (;ts, xpir=SH.getExpir(first(oqs_style)))
    return calc_xtrin_top(style, under, SH.getStrike.(oqs_sorted), SH.getBid.(oqs_sorted), SH.getAsk.(oqs_sorted), ctx)
end

function calc_xtrin_top(style, under::Real, strikes, bids, asks, ctx)
    # @show under strikes bids asks
    extrins = OptionUtil.calc_extrin(style, under, strikes, bids, asks)
    extrins ./= under
    total = CZ
    mean_count = length(extrins)
    neg_count = 0
    for i in eachindex(extrins)
        x = extrins[i]
        if x <= 0
            extrins[i] = CZ
            neg_count += 1
            mean_count -= 1
        else
            total += x
        end
    end

    # Missing quotes is bad data. Confirmed on ts = 2014-01-10T14:30:10.
    # neg xtrins?
    if !iszero(neg_count)
        @atomic negative_extrin_count2.value += 1
        if neg_count > 2
            # Only expect weird values at open or close
            if is_ts_normal(ctx.ts)
                println("xtrin $(ctx): Found neg extrin not market open/close: $(neg_count) / $(length(strikes))")
                @show style under strikes bids asks extrins
                # global kerr = (;style, ts, xpirts, under, extrins, strikes, bids, asks)
            end
            return missing
        end
    end
    xtrin_check_strikes(under, strikes, ctx) || return missing

    # TODO: maybe should use geometric/harmonic mean?
    extrin = iszero(mean_count) ? CZ : total / mean_count
    # vol = extrin / tex
    # logvol = log(vol)
    # return [tex extrin vol logvol]
    return extrin
end

function xtrin_check_strikes(under, strikes, ctx)
    if length(strikes) < NTM_COUNT
        is_ts_normal(ctx.ts) || println("xtrin $(ctx): Too few strikes $(length(strikes)) < $(NTM_COUNT)")
        return false
    end
    x = sum(abs.(strikes .- under))
    y = 1000 * (2 + 3 * NTM_COUNT)
    if x > y
        is_ts_normal(ctx.ts) || println("xtrin $(ctx): Strikes too far away $(x) > $(y) from under:$(under) - $(strikes)")
        return false
    end
    return true
end


using Memoization, ThreadSafeDicts
# @memoize ThreadSafeDicts.ThreadSafeDict is_ts_normal(ts::DateTime) = second(ts) < 8 && DateUtil.isBusDay(Date(ts)) && ts < market_close(ts)
@memoize is_ts_normal(ts::DateTime) = second(ts) < 8 && DateUtil.isBusDay(Date(ts)) && ts < market_close(ts)

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
function chains_to_arrow(yms::CollT{NTuple{2,Int}})
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
    df = filter_bday(dfincoming(y, m))
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

totimestamp(x::Int) = unix2datetime(x)
tofloat(x::Float64) = Float32(x)
tofloat(x::AbstractString) = isempty(strip(x)) ? missing : tofloat_(x)
tofloat_(x) = parse(Float32, x)
topents(x::Float64) = round(Int32, x * 1000)
topents(x::AbstractString) = isempty(strip(x)) ? missing : topents(parse(Float64, x))
topents(x::Currency) = Int32(x * 1000)
topents(x::Missing) = x
pents_to_c(x::Int32) = C(x / 1000)
pents_to_c(x::Missing) = x

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
    stopped = ThreadUtil.loop(yms) do (y, m)
        thid = threadid()
        calls = calls_table(y, m)
        puts = puts_table(y, m)
        union!(thr_tss[thid], zip(calls.ts, calls.under))
        union!(thr_tss[thid], zip(puts.ts, puts.under))
        println("$(thid): Completed $((;y, m))")
    end
    !stopped || return
    println("Finished threads, now combining...")
    tss = reduce(union!, thr_tss)
    df = DataFrame(tss, [:ts,:under])
    sort!(df, :ts)
    # unique!(df, [:ts]) # puts and calls are separate files so will duplicate
    global kdf_tss = df
    # @assert isunique(df.ts) # isunique doesn't exist
    Arrow.write(path_ts(), df)
    kdf_tss = nothing # if above line throws exception, it's not cleared and we can try writing manually
    return df
end
#endregion

#region ChainsToXpir
function chains_to_xpirts(yms=make_yms())
    thr_xpirtss = [Vector{DateTime}() for _ in 1:nthreads()]
    ThreadUtil.loop(yms) do (y, m)
        thid = Threads.threadid()
        calls = calls_table(y, m)
        puts = puts_table(y, m)
        append!(thr_xpirtss[thid], unique(calls.expiration))
        append!(thr_xpirtss[thid], unique(puts.expiration))
        println("$(thid): Completed $((;y, m))")
        yield()
    end
    println("Finished threads, now combining...")
    xpirtss = reduce(vcat, thr_xpirtss)
    sort!(xpirtss)
    unique!(xpirtss)
    df = DataFrame([xpirtss], [:xpirts]; copycols=false)
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
    return sort!(filter!(vec([(y, m) for y in 2010:2023, m in 1:12])) do (y, m)
        y != 2023 || m <= 6
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

find_dfrow(df, ts, xpir::Date, strike) = find_dfrow(df, ts, market_close(xpir), strike)
find_dfrow(df, ts, expiration::DateTime, strike) = only(df[(df.ts .== ts) .& (df.expiration .== expiration) .& (df.strike .== strike),:])

const XPIRS = Dict{Date,DateTime}()
market_close(ts::DateTime) = market_close(Date(ts))
@memoize ThreadSafeDicts.ThreadSafeDict function market_close(date::Date)
    if isempty(XPIRS)
        tbl = xpir_table()
        DictUtil.set_from_vals!(Date, XPIRS, tbl.xpirts)
    end
    xpirts1 = get(XPIRS, date, missing)
    xpirts2 = ts_day_last(date)
    xpirts3 = Calendars.getMarketClose(date)
    # all = (;xpirts1, xpirts2, xpirts3)
    if !ismissing(xpirts1)
        if xpirts1 != xpirts2 || xpirts1 != xpirts3
            # println("WARN: market close didn't match: $(all)")
            push!(EOD_DONT_MATCH, date)
        end
    elseif !ismissing(xpirts2)
        if xpirts2 != xpirts3
            # println("WARN: market close didn't match (not xpir): $(all)")
            push!(EOD_DONT_MATCH, date)
        end
    end
    return xpirts3
end
const EOD_DONT_MATCH = Set{Date}()

const TSS = Dict{Date,DateTime}()
ts_day_last(ts::DateTime) = ts_day_last(Date(ts))
function ts_day_last(date::Date)
    if isempty(TSS)
        tbl = ts_table()
        ts_prev, tss = Iterators.peel(tbl.ts)
        tsd_prev = Date(ts_prev)
        for ts in tss
            tsd = Date(ts)
            if tsd != tsd_prev
                @assert tsd > tsd_prev
                TSS[tsd_prev] = ts_prev
            end
            ts_prev = ts
            tsd_prev = tsd
        end
        TSS[tsd_prev] = ts_prev
    end
    return get(TSS, date, missing)
end

function save(path, df)
    mkpath(dirname(path))
    Arrow.write(path, df)
end
#endregion Misc

#region Vix
import TradierData as TD
function make_vix_alldates()
    vix_raw = TD.tradierHistQuotes("daily", Date("2010-01-01"), Date("2023-07-01"), "VIX")
    vix_raw_df = DataFrame(vix_raw)
    vix_df = select(vix_raw_df, :date => (ds -> Date.(ds)) => :date, :open, :high, :low, :close)
    saving_currency!(vix_df, :open, :high, :low, :close)
    save(path_vix(), vix_df)
    dates_df = DataFrame(:date => collect(DateUtil.all_weekdays()))
    vix_alldates_df = sort!(leftjoin(dates_df, vix_df, on=:date), [:date])
    save(path_vix_alldates(), vix_alldates_df)
    return vix_alldates_df
end

function make_ts_allperiods(;period=Minute(30))
    tsdf = ts_df()
    times_df = DataFrame(:ts => DateUtil.all_weekday_ts(;period))
    # filter out rows for other times
    inner = innerjoin(tsdf, times_df; on = :ts)
    # Add missing rows for times
    df = leftjoin(times_df, inner; on = :ts)
    sort!(df, [:ts])
    saving_currency!(df, :under)
    save(path_ts_allperiods(;period), df)
    return df
end

function valid_ts_for_seq()

end
#endregion

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

#region Util
missing_to_zero_float(x) = ismissing(x) ? 0f0 : Float32(x)
#endregion

end