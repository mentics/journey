module SimpleStore
using Dates
using SH, BaseTypes, SmallTypes, OptionTypes, QuoteTypes, OptionQuoteTypes, OptionMetaTypes
using DateUtil, DictUtil, CollUtil, ChainUtil
import Calendars as cal

export ChainInfo, UnderTime, TimeInfo

#region ConstAndTypes
const HINT_TSS = 24 * 28
const HINT_OQS = 100
const HINT_XPIRS = 40

const DirOut = "C:/data/db/hand/"
const FileTss = joinpath(DirOut, "tss.ser")
const DF_FILE = DateFormat("yyyymm")

struct UnderTime
    under::Currency
    open::Currency
    hi::Currency
    lo::Currency
end

struct ChainInfo # <: Chain
    xsoqs::Dict{Date,Styles{Vector{OptionQuote}}}
    under::UnderTime
    xpirs::Vector{Date}
end

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

function run(f, from::DateLike, to::DateLike; maxSeconds=10)
    tss = getTss(from, to)
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

        if time() > start + maxSeconds
            break
        end
    end
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
getTss(filt, from::DateLike, to::DateLike)::Vector{DateTime} = filter(ts -> from <= ts <= to && filt(ts), Tss[])
# TODO: test the above two return the same for no filter

tsFirst(dts::DateLike)::DateTime = Tss[][searchsortedfirst(Tss[], dts)]

ChainUtil.toOtoq(chi::ChainInfo) = ChainUtil.toOtoq(chi.xsoqs)
ChainUtil.getXpirs(chi::ChainInfo) = chi.xpirs
#endregion

#region Local
function __init__()
    if ccall(:jl_generating_output, Cint, ()) != 1
        println("Loading SimpleStore")
        sizehint!(ChainCache, HINT_TSS * 12 * 12) # 12 months by 12 years
        # sizehint!(XpirCache, HINT_TSS * 12 * 12)
        loadTss()
    end
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
# const XpirCache = Dict{DateTime,Vector{Date}}()
function loadChainInfo(ts::DateTime)::ChainInfo
    get(ChainCache, ts) do
        loadMonth(year(ts), month(ts))
        return ChainCache[ts]
    end
end

function loadMonth(y, m)
    pathCall, pathPut, pathUnder = paths(y, m)
    calls = Dict{DateTime,Dict{Date,Vector{OptionQuote}}}()
    sizehint!(calls, HINT_TSS)
    loadOpt(proc(calls, Style.call), pathCall)
    puts = Dict{DateTime,Dict{Date,Vector{OptionQuote}}}()
    sizehint!(puts, HINT_TSS)
    loadOpt(proc(puts, Style.put), pathPut)
    tunder = Dict{DateTime,UnderTime}()
    sizehint!(tunder, HINT_TSS)
    loadUnder(toUnd(tunder), pathUnder)
    @assert keys(calls) == keys(puts) == keys(tunder)
    for ts in keys(calls)
        callsts = calls[ts]
        putsts = puts[ts]
        xpirsCalls = keys(callsts)
        xpirsPuts = keys(putsts)
        if xpirsCalls != xpirsPuts
            global keepXpirsCalls = xpirsCalls
            global keepXpirsPuts = xpirsPuts
            println("Unexpected xpirsCalls != xpirsPuts for year/month: $(y)/$(m)")
        end
        xsoqs = Dict{Date,Styles{Vector{OptionQuote}}}()
        xpirs = filter!(x -> x < Date(2025, 1, 1), sort!(collect(xpirsCalls)))
        for xpir in xpirs
            xsoqs[xpir] = Styles(callsts[xpir], putsts[xpir])
        end
        ChainCache[ts] = ChainInfo(xsoqs, tunder[ts], xpirs)
    end
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
            if bid <= 0.0 && ask <= 0.0
                # TODO: Is just ignoring the right solution? Could skew results. Probably should double check the original data to see if maybe it was a parsing issue.
                # println("Ignoring bad quote: $((;ts,strike,bid,ask))")
                continue
            end
            if 1 < strike < 1000 # TODO: can remove this after reload data filtering it
                f(ts, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv)
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
    tss = Vector{DateTime}()
    for dirName in readdir(DirOut; sort=false)
        path = joinpath(DirOut, dirName)
        isdir(path) || continue
        println("processing $(path)")
        dt = Date(dirName, DF_FILE)
        data = loadRaw(year(dt), month(dt))
        append!(tss, data.tss)
    end
    sort!(tss)
    open(FileTss; write=true) do io
        write(io, tss)
    end
end
#endregion

#region Util
matches(oq, style, strike) = getStyle(oq) == style && getStrike(oq) == strike
#endregion

end