module SimpleStore
using Dates, DataStructures
using SH, BaseTypes, SmallTypes, OptionTypes, QuoteTypes, OptionQuoteTypes, OptionMetaTypes, ChainTypes
using DateUtil, DictUtil, CollUtil, ChainUtil
import Calendars as cal

export TimeInfo

#region ConstAndTypes
const HINT_TSS = 24 * 28
const HINT_OQS = 100
const HINT_XPIRS = 40

const DirOut = "C:/data/db/hand/"
const FileTss = joinpath(DirOut, "tss.ser")
const DF_FILE = DateFormat("yyyymm")

struct TimeInfo
    ts::DateTime
    date::Date
    firstOfDay::Bool
    lastOfDay::Bool
    atClose::Bool
end
#endregion

#region Public
ChainUtil.getCurp(chain::ChainInfo) = chain.under.under

run(f, from::DateLike, to::DateLike; maxSeconds=10, max_iterations=typemax(Int)) = run(f, getTss(from, to); maxSeconds, max_iterations)
function run(f, tss; maxSeconds=10, max_iterations=typemax(Int))
    start = time()
    lasti = lastindex(tss)
    firstOfDay = true
    lastOfDay = false
    for i in eachindex(tss)
        ts = tss[i]
        date = Date(ts)
        lastOfDay = i == lasti || day(tss[i+1]) != day(ts)
        atClose = Time(ts) == Time(cal.getMarketClose(Date(ts)))

        chain = loadChainInfo(ts)
        f(TimeInfo(ts, date, firstOfDay, lastOfDay, atClose), chain)

        firstOfDay = i == lasti || day(tss[i+1]) != day(ts)

        if i >= max_iterations
            break
        end
        if time() > start + maxSeconds
            break
        end
    end
end
# makeTimFirst(ts) = TimeInfo(ts, Date(ts), true, false, false)
function runFirst(from::DateLike)
    tim, chain = nothing, nothing
    run(from, tsLast(); max_iterations=1) do t, c
        @assert isnothing(tim)
        tim, chain = t, c
    end
    return tim, chain
end

# function getOq(ts::DateTime, xpir::Date, style::Style.T, strike::Currency)::Union{Nothing,OptionQuote}
#     oqs = getOqs(ts, xpir)
#     i = findfirst(oq -> matches(oq, style, strike), oqs)
#     return isnothing(i) ? nothing : oqs[i]
# end

# getCalls(ts::DateTime, xpir::Date)::Vector{OptionQuote} = loadChainInfo(ts)[xpir].calls
# getPuts(ts::DateTime, xpir::Date)::Vector{OptionQuote} = loadChainInfo(ts)[xpir].puts

# getSoqs(ts::DateTime, xpir::Date)::Styles{Vector{OptionQuote}} = loadChain(ts).xpirChain[xpir]
# getXsoqs(ts::DateTime) = loadChain(ts).xsoqs
# getUnder(ts::DateTime) = loadChain(ts).under

# getExpirs(ts::DateTime)::Vector{Date} = getExpirs(load(ts), ts)
# getExpirs(data::HistChain, ts::DateTime)::Vector{Date} = sort(collect(filter(x -> x < Date(2025, 1, 1), keys(data.chain[ts]))))

getTss(from::DateLike, to::DateLike)::Vector{DateTime} = CollUtil.sublist(Tss[], from, to)
countTss(from::DateLike, to::DateLike)::Int = CollUtil.countSublist(Tss[], from, to)
getTss(filt, from::DateLike, to::DateLike)::Vector{DateTime} = filter(ts -> from <= ts <= to && filt(ts), Tss[])
# TODO: test the above two return the same for no filter

tsFirst(dts::DateLike)::DateTime = Tss[][searchsortedfirst(Tss[], dts)]
tsLast()::DateTime = Tss[][end]

ChainUtil.toOtoq(chi::ChainInfo) = ChainUtil.toOtoq(chi.xsoqs)
ChainUtil.getXpirs(chi::ChainInfo) = chi.xpirs

chain_for(ts::DateTime)::Otoq = OtoqCache[ts]
function chains_for(from, to)
    @assert from < to "chains_for invalid input: $from < $to"
    loadChainInfo(from)
    loadChainInfo(to)
    st1 = searchsortedfirst(OtoqCache, from)
    st2 = searchsortedlast(OtoqCache, to)
    res = inclusive(OtoqCache, st1, st2)
    if isempty(res)
        @show from to extrema(keys(OtoqCache))
        println("ERROR: no chains for $from - $to")
    end
    return res
end

function ts_prev(ts::DateTime)::DateTime
    tk = searchsortedfirst(OtoqCache, ts)
    tk_prev = regress((OtoqCache, tk))
    return deref_key((OtoqCache, tk_prev))
end
#endregion

#region Local
function __init__()
    # if ccall(:jl_generating_output, Cint, ()) != 1
    #     println("Loading SimpleStore")
    #     sizehint!(ChainCache, HINT_TSS * 12 * 12) # 12 months by 12 years
    #     # sizehint!(XpirCache, HINT_TSS * 12 * 12)
    #     loadTss()
    # end
end
#endregion

#region Load
const Tss = Ref{Vector{DateTime}}()
function loadTss()
    len = div(filesize(FileTss),sizeof(DateTime))
    tss = Vector{DateTime}(undef, len)
    open(FileTss) do io
        read!(io, tss)
    end

    # Ignore first ts of the day because data might be questionnable, and just use 30 minute because that's all we have for older dates
    Tss[] = filter!(ts -> minute(ts) != 15 && minute(ts) != 45 && Time(ts) != Time(cal.getMarketOpen(Date(ts))) + Second(10), tss)
end

function paths(y, m)
    dateStr = Dates.format(Date(y, m, 1), DF_FILE)
    baseDir = mkpath(joinpath(DirOut, dateStr))
    pathCall = joinpath(baseDir, "call.ser")
    pathPut = joinpath(baseDir, "put.ser")
    pathUnder = joinpath(baseDir, "under.ser")
    return pathCall, pathPut, pathUnder
end

const ChainCache = Dict{DateTime,ChainInfo}()
function clearCache()
    empty!(ChainCache)
end
# const XpirCache = Dict{DateTime,Vector{Date}}()
function loadChainInfo(ts::DateTime)::ChainInfo
    get(ChainCache, ts) do
        loadMonth(year(ts), month(ts))
        return ChainCache[ts]
    end
end

first_ts_for(date::Date) = cal.getMarketOpen(date) + Second(10)
last_ts_for(date::Date) = cal.getMarketClose(date)
is_ts_eod(ts) = ts == cal.getMarketClose(Date(ts))

const OtoqCache = SortedDict{DateTime,Otoq}()
function make_otoqs()
    empty!(OtoqCache)
    for (ts, ch) in ChainCache
        OtoqCache[ts] = ChainUtil.toOtoq(ch.xsoqs)
    end
end

function loadMonth(y, m; cache=true)
    pathCall, pathPut, pathUnder = paths(y, m)
    if !isfile(pathCall)
        println("WARN: Tried to load SimpleStore $y-$m but skipping because file not found: $pathCall")
        return
    end
    calls = Dict{DateTime,Dict{Date,Vector{OptionQuote}}}()
    sizehint!(calls, HINT_TSS)
    loadOpt(proc(calls, Style.call), pathCall)
    puts = Dict{DateTime,Dict{Date,Vector{OptionQuote}}}()
    sizehint!(puts, HINT_TSS)
    loadOpt(proc(puts, Style.put), pathPut)
    tunder = Dict{DateTime,UnderTime}()
    sizehint!(tunder, HINT_TSS)
    loadUnder(toUnd(tunder), pathUnder)
    # There are many cases of missing data, so tunder won't match, but should be a superset
    keysCalls = keys(calls)
    keysPuts = keys(puts)
    keysUnder = keys(tunder)
    if !(keysCalls == keysPuts && issubset(keysCalls, keysUnder))
        global keepCalls = calls
        global keepPuts = puts
        global keepUnder = tunder
        error("calls, puts, under keys don't match, saved globals")
    end
    tss = union(keysCalls, keysPuts, keysUnder)
    if cache
        for ts in tss
            callsts = calls[ts]
            putsts = puts[ts]
            xpirsCalls = keys(callsts)
            xpirsPuts = keys(putsts)
            xpirs = xpirsCalls
            if xpirsCalls != xpirsPuts
                global keepXpirsCalls = xpirsCalls
                global keepXpirsPuts = xpirsPuts
                xpirs = intersect(xpirsCalls, xpirsPuts)
                println("Unexpected xpirsCalls != xpirsPuts for year/month: $(y)/$(m) $(length(xpirsCalls)) $(length(xpirsPuts)), saved globals")
            end
            xsoqs = Dict{Date,Styles{Vector{OptionQuote}}}()
            xpirs = filter!(x -> x < Date(2025, 1, 1), sort!(collect(xpirs)))
            for xpir in xpirs
                xsoqs[xpir] = Styles(callsts[xpir], putsts[xpir])
            end
            ChainCache[ts] = ChainInfo(xsoqs, tunder[ts], xpirs)
        end
    end
    return tss
end

# function loadRaw(y, m)::HistChain
#     data = HistChain()
#     pathCall, pathPut, pathUnder = paths(y, m)
#     loadOpt(proc(data, Style.call), pathCall)
#     loadOpt(proc(data, Style.put), pathPut)
#     loadUnder(toUnd(data), pathUnder)
#     append!(data.tss, keys(data.under))
#     sort!(data.tss)
#     return data
# end

function proc(info, style)
    return function(ts, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv)
        v = get!(newv, get!(newch, info, ts), xpir)
        push!(v, toOq(style, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv))
    end
end

newch() = ( d = Dict{Date,Vector{OptionQuote}}() ; sizehint!(d, HINT_XPIRS) ; return d )
newv() = ( v = Vector{OptionQuote}() ; sizehint!(v, HINT_OQS) ; return v )

function toOq(style, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv)::OptionQuote
    return OptionQuote(
        Option(style, xpir, strike), Quote(bid, ask),
        OptionMeta(delta, theta, NaN, vega, rho, gamma, iv, iv, iv)
    )
end

function toUnd(data)
    return function(ts, under, open, hi, lo)
        data[ts] = UnderTime(under, open, hi, lo)
    end
end

import Calendars
function loadOpt(f, path)
    open(path) do io
        while !eof(io)
            ts = unix2datetime(read(io, Int))
            xpir = toDateMarket(unix2datetime(read(io, Int)))
            strike = reinterpret(Currency, read(io, Int))
            bid = reinterpret(Currency, read(io, Int))
            ask = reinterpret(Currency, read(io, Int))
            last = reinterpret(Currency, read(io, Int))
            vol = read(io, Int)
            delta = read(io, Float64)
            gamma = read(io, Float64)
            vega = read(io, Float64)
            theta = read(io, Float64)
            rho = read(io, Float64)
            iv = read(io, Float64)
            # TODO: Is there ever a situation where knowing how negative it was would matter?
            if bid <= 0.0
                bid = CZ
            end
            if ask <= 0.0
                ask = CZ
            end
            if bid > ask
                date = Date(ts)
                if ts != first_ts_for(date) && ts != last_ts_for(date)
                    println("WARN: bid < ask for non-first, non-last ts")
                    @show ts xpir strike bid ask
                end
            end
            if 1 < strike < 1000 # TODO: can remove this after reload data filtering it
                f(ts, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv)
            else
                # println("Ignoring bad strike $(stike)")
            end
        end
    end
    return
end

function loadUnder(f, path)
    open(path) do io
        while !eof(io)
            ts = unix2datetime(read(io, Int))
            under = reinterpret(Currency, read(io, Int))
            open = reinterpret(Currency, read(io, Int))
            hi = reinterpret(Currency, read(io, Int))
            lo = reinterpret(Currency, read(io, Int))
            f(ts, under, open, hi, lo)
        end
    end
    return
end
#endregion

#region Maintenance
function updateTssFile()
    # tss = Vector{DateTime}()
    # for dirName in readdir(DirOut; sort=false)
    #     path = joinpath(DirOut, dirName)
    #     isdir(path) || continue
    #     println("processing $(path)")
    #     dt = Date(dirName, DF_FILE)
    #     data = loadRaw(year(dt), month(dt))
    #     append!(tss, data.tss)
    # end
    tssLoaded = sort!(collect(keys(ChainCache)))
    notFound = lastindex(tssLoaded) + 1
    global tss = DateTime[]
    for date in Date(2016,1):Month(1):Date(2023,1)
        println("processing $(date)")
        i = searchsortedfirst(tssLoaded, date)
        if i == notFound || year(tssLoaded[i]) != year(date) || month(tssLoaded[i]) != month(date)
            tss_add = loadMonth(year(date), month(date); cache=false)
            isnothing(tss_add) || append!(tss, tss_add)
        end
    end
    # tssLoaded = sort!(collect(keys(ChainCache)))
    tssLoaded = sort!(tss)
    open(FileTss; write=true) do io
        write(io, tss)
    end
end
#endregion

#region Util
matches(oq, style, strike) = getStyle(oq) == style && getStrike(oq) == strike
#endregion

#region Testing
curpFor(ts::DateTime) = ChainUtil.getCurp(loadChainInfo(ts))
quoteFor(ts::DateTime, xpir::Date, style::Style.T, strike::Currency)::OptionQuote = find(x -> getStrike(x) == strike, loadChainInfo(ts).xsoqs[xpir][style])
quoteFor(ts::DateTime, has)::OptionQuote = quoteFor(ts, getExpir(has), getStyle(has), getStrike(has))
lupFor(ts::DateTime) = (args...) -> quoteFor(ts, args...)
# find(x -> getStrike(x) == getStrike(has), loadChainInfo(ts)[getExpir(has)][getStyle(has)])
#endregion

end