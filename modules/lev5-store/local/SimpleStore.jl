module SimpleStore
using Dates
using SH, BaseTypes, SmallTypes, OptionTypes, QuoteTypes, OptionMetaTypes, ChainTypes
using DateUtil, DictUtil
import Calendars as cal

#region ConstAndTypes
const HINT_TSS = 24 * 28
const HINT_OQS = 100
const HINT_XPIRS = 40

const DirOut = "C:/data/db/hand/"
const FileTss = joinpath(DirOut, "tss.ser")
const DF_FILE = DateFormat("yyyymm")

const NoFirst2 = x -> Time(x) != Time(cal.getMarketOpen(Date(x))) + Second(10)

struct UnderTime
    under::Currency
    open::Currency
    hi::Currency
    lo::Currency
end

struct HistChain
    chain::Dict{DateTime,Dict{Date,Vector{OptionQuote}}}
    under::Dict{DateTime,UnderTime}
    tss::Vector{DateTime}
end
function HistChain()
    chain = Dict{DateTime,Dict{Date,Vector{OptionQuote}}}()
    sizehint!(chain, HINT_TSS)
    under = Dict{DateTime,UnderTime}()
    sizehint!(under, HINT_TSS)
    tss = Vector{DateTime}()
    sizehint!(tss, HINT_TSS)
    return HistChain(chain, under, tss)
end

function __init__()
    if ccall(:jl_generating_output, Cint, ()) != 1
        loadTss()
    end
end
#endregion

#region Public
matches(oq, style, strike) = getStyle(oq) == style && getStrike(oq) == strike
function getOq(ts::DateTime, xpir::Date, style::Style.T, strike::Currency)::Union{Nothing,OptionQuote}
    oqs = getOqs(ts, xpir)
    i = findfirst(oq -> matches(oq, style, strike), oqs)
    return isnothing(i) ? nothing : oqs[i]
end
getOqs(ts::DateTime, xpir::Date)::Vector{OptionQuote} = getOqs(load(ts), ts, xpir)
getOqs(data::HistChain, ts::DateTime, xpir::Date)::Vector{OptionQuote} = data.chain[ts][xpir]

getXoqs(ts::DateTime) = getXoqs(load(ts), ts)
getXoqs(data::HistChain, ts::DateTime) = data.chain[ts]

getUnder(ts::DateTime) = getUnder(load(ts), ts)
getUnder(data::HistChain, ts::DateTime) = data.under[ts]

getExpirs(ts::DateTime)::Vector{Date} = getExpirs(load(ts), ts)
getExpirs(data::HistChain, ts::DateTime)::Vector{Date} = sort(collect(filter(x -> x < Date(2025, 1, 1), keys(data.chain[ts]))))

getTss(from::DateLike, to::DateLike)::Vector{DateTime} = filter(ts -> from <= ts <= to, Tss[])
getTss(filt, from::DateLike, to::DateLike)::Vector{DateTime} = filter(ts -> from <= ts <= to && filt(ts), Tss[])
getTss(data::HistChain) = data.tss

function tsFirst(dts::DateLike; filt=NoFirst2)::DateTime
    data = load(dts)
    i = findfirst(ts -> ts >= dts && filt(ts), data.tss)
    return data.tss[i]
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
    Tss[] = tss
end

function paths(y, m)
    dateStr = Dates.format(Date(y, m, 1), DF_FILE)
    baseDir = mkpath(joinpath(DirOut, dateStr))
    pathCall = joinpath(baseDir, "call.ser")
    pathPut = joinpath(baseDir, "put.ser")
    pathUnder = joinpath(baseDir, "under.ser")
    return pathCall, pathPut, pathUnder
end

const Cache = Dict{Date,HistChain}()
load(dts::DateLike)::HistChain = load(year(dts), month(dts))
function load(y, m)::HistChain
    return get!(() -> loadRaw(y, m), Cache, Date(y, m))
end

function loadRaw(y, m)::HistChain
    data = HistChain()
    pathCall, pathPut, pathUnder = paths(y, m)
    loadOpt(proc(data, Style.call), pathCall)
    loadOpt(proc(data, Style.put), pathPut)
    loadUnder(tound(data), pathUnder)
    append!(data.tss, keys(data.under))
    sort!(data.tss)
    # data.tss = sort!(collect(keys(data.under)))
    return data
end

function proc(data, style)
    return function(ts, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv)
        v = get!(newv, get!(newch, data.chain, ts), xpir)
        push!(v, tooq(style, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv))
    end
end

newch() = ( d = Dict{Date,Vector{OptionQuote}}() ; sizehint!(d, HINT_XPIRS) ; return d )
newv() = ( v = Vector{OptionQuote}() ; sizehint!(v, HINT_OQS) ; return v )

function tooq(style, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv)::OptionQuote
    return OptionQuote(
        Option(style, xpir, strike), Quote(bid, ask),
        OptionMeta(delta, theta, NaN, vega, rho, gamma, iv, iv, iv)
    )
end

function tound(data)
    return function(ts, under, open, hi, lo)
        data.under[ts] = UnderTime(under, open, hi, lo)
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

end