module DataFiles
using Dates, Base.Threads
using DelimitedFiles, DataFrames
using Arrow
import StatsBase:mean
using DateUtil
using BaseTypes, SmallTypes
import OptionUtil
import Calendars
import ThreadPools

#region Consts
const XPIRS_RANGE = 1:11
#endregion

#region Paths
basepath() = joinpath("C:\\", "data", "db")
baseincoming() = joinpath(basepath(), "incoming", "optionsdx")
path_tss() = joinpath(basepath(), "market", "spy", "tss.arrow")
path_xpirs() = joinpath(basepath(), "market", "spy", "xpirs.arrow")
pathchains() = joinpath(basepath(), "market", "spy", "chains")
pathcalls(y, m) = joinpath(pathchains(), "calls-$(datestr(y, m)).arrow")
pathputs(y, m) = joinpath(pathchains(), "puts-$(datestr(y, m)).arrow")
#endregion

#region Loaders
callstable(y, m) = Arrow.Table(pathcalls(y, m))
putstable(y, m) = Arrow.Table(pathcalls(y, m))
xpirstable() = Arrow.Table(path_xpirs())

using DataStructures
tssdf() = DataFrame(Arrow.Table(path_tss()); copycols=false)
function ts2under_lup()
    tbl = Arrow.Table(path_tss())
    return SortedDict{DateTime,Currency}(zip(tbl.ts, tbl.under))
end
#endregion

#region Public
function update()
    updateall()
end

function proc(yms)
    @threads for (y, m) in yms
        procincoming(y, m)
    end
end

function procincoming(y, m)
    mkpath(pathchains())
    df = dfincoming(y, m)
    transformraw(df)
    dfcalls = extractdf(df, "c_")
    dfputs = extractdf(df, "p_")
    Arrow.write(pathcalls(y, m), dfcalls)
    Arrow.write(pathputs(y, m), dfputs)
end
#endregion

#region UpdateAll
function updateall()::Nothing
    lup = ts2under_lup()

    thr_dfs_ts = [Vector{DataFrame}() for _ in 1:nthreads()]
    thr_dfs_xpir = [Vector{DataFrame}() for _ in 1:nthreads()]
    thr_dfs_ntm = [Vector{DataFrame}() for _ in 1:nthreads()]
    paths = readdir(pathchains(); join=true)[1:1]
    @threads for path in paths
        df = DataFrame(Arrow.Table(path))

        style = occursin("calls", path) ? Style.call : Style.put

        # tsdf = unique(select(df, [:ts,:under]), [:ts])
        # push!(thr_dfs_ts[threadid()], tsdf)
        # println("ts done $(size(tsdf, 1))")

        xpirdf = unique(select(df, [:expiration]), [:expiration])
        push!(thr_dfs_xpir[threadid()], xpirdf)
        println("xpir done $(size(xpirdf, 1))")

        ntmdf = filter([:ts, :expiration] => filter_xpirs, df)
        ntmdf.strikedist = abs.(ntmdf.under .- ntmdf.strike)
        sort!(ntmdf, [:ts,:expiration,:strikedist])
        println("ntmdf filter/sort done $(size(ntmdf, 1))")

        f_extrin = style == Style.call ? OptionUtil.extrin_call : OptionUtil.extrin_put
        gdf = groupby(ntmdf, [:ts,:expiration])
        gdf = combine(gdf, sdf -> first(sdf, 4); ungroup=false)
        ntmdf = combine(gdf,
            # [:ts,:expiration,:under,:strike,:bid,:ask] => ( function(ts, xpir, under, strike, bid, ask)
            #     tex = Calendars.calcTex(first(ts), first(xpir))
            #     extrin = mean(f_extrin(under, strike, bid, ask))
            #     vol = extrin / tex
            #     return [tex, extrin, vol] # ::Vector{Float64} # (tex, extrin, vol)::NTuple{3,Float64}
            # end) => [:tex,:extrin,:vol],

            [:ts,:expiration,:under] => ( function(ts, xpir, under)
                ret = get.(lup, xpir) ./ under
                logret = log.(ret)
                return [ret, logret]::Vector{Float64}
            end) => [:ret,:logret],

            :iv => mean => :iv_mean

            ; threads=false
        )
        ntmdf.style .= style
        push!(thr_dfs_ntm[threadid()], ntmdf)
        println("ntmdf done $(size(ntmdf, 1))")
    end
    # global kthr_dfs_ts = thr_dfs_ts
    global kthr_dfs_xpir = thr_dfs_xpir
    global kthr_dfs_ntm = thr_dfs_ntm
    println("Finished threads $(length.((thr_dfs_ts, thr_dfs_xpir, thr_dfs_ntm)))")
    combineall(thr_dfs_ts, thr_dfs_xpir, thr_dfs_ntm)
    return
end

function combineall(dfs_ts, dfs_xpir, dfs_ntm)::Nothing
    # TODO: maybe don't need spawn because dataframe sort might multithread?
    # _tss = @spawn unique!(sort!(reduce(vcat, dfs_ts), :ts))
    _xpirs = @spawn unique!(sort!(reduce(vcat, dfs_xpir), :xpir))
    _tstoxpir = @spawn sort!(dfs_ntm, [:ts,:expiration])
    # dftss = fetch(_tss)
    dfxpirs = fetch(_xpirs)
    dftstoxpir  = fetch(_tstoxpir)
    # Arrow.write(path_tss(), dftss)
    Arrow.write(path_xpirs(), dfxpirs)
    Arrow.write(path_tstoxpir(), dftstoxpir)
    @assert isnothing(findfirst(x -> x < 0; dftstoxpir.ivol))
    return
end

# filter_xpirs(ts, xpir) = bdays(Date(ts), Date(xpir)) in XPIRS_RANGE
filter_xpirs(ts, xpir) = Dates.value(Date(xpir) - Date(ts)) in XPIRS_RANGE

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

#region Incoming
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

function dfincoming(y, m)
    path = pathincoming(y, m)
    data, header = readdlm(path, ','; header=true)
    return DataFrame(data, vec(header))
end

fixcolname(col) = occursin('[', col) ? lowercase(strip(col)[2:end-1]) : col

tofloat(x::Float64) = Float32(x)
tofloat(x::AbstractString) = isempty(strip(x)) ? missing : tofloat_(x)
tofloat_(x) = parse(Float32, x)
topents(x::Float64) = round(Int32, x * 1000)
topents(x::AbstractString) = isempty(strip(x)) ? missing : topents_(x)
topents_(x) = topents(Float64(x))
totimestamp(x::Int) = unix2datetime(x)

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

# # TODO: could write optimized loop for sorted one
# uniqueidx(v) = unique(i -> v[i], eachindex(v))

# function pushsortedunique!(into::AbstractVector{T}, vals::AbstractVector{T}, into2::AbstractVector{B}, vals2::AbstractVector{B}) where {T,B}
function pushsortedunique!(into, vals, into2, vals2)
    last = first(vals)
    push!(into, last)
    push!(into2, first(vals2))
    for i in eachindex(vals)
        x = vals[i]
        if x !== last
            push!(into, x)
            push!(into2, vals2[i])
            last = x
        end
    end
    return nothing
end

# Need this because catch 22: need lup for updateall, but updateall makes data for lup
function updatetss()
    thr_tss = [Vector{DateTime}() for _ in 1:nthreads()]
    thr_unders = [Vector{Int}() for _ in 1:nthreads()]
    ThreadPools.twith(ThreadPools.QueuePool(1, 2)) do pool
        ThreadPools.@tthreads pool for path in readdir(pathchains(); join=true)
            # df = unique(select(DataFrame(Arrow.Table(path); copycols=false), [:ts,:under]), [:ts])
            # push!(threaddfs[threadid()], df)
            thid = threadid()
            tbl = Arrow.Table(path)
            pushsortedunique!(thr_tss[thid], tbl.ts, thr_unders[thid], tbl.under)
            println("Processed: $(path) on thid:$(Threads.threadid())")
            yield()
        end
    end
    println("Finished threads, now combining...")
    tss = reduce(vcat, thr_tss)
    unders = reduce(vcat, thr_unders)
    df = DataFrame([tss, unders], [:ts, :under]; copycols=false)
    sort!(df, :ts)
    unique!(df, [:ts]) # puts and calls are separate files so will duplicate
    global kdf_tss = df
    Arrow.write(path_tss(), df)
    kdf_tss = nothing # if above line throws exception, it's not cleared and we can try writing manually
    return df
end
#endregion

end