module SimpleStore
using Dates
using BaseTypes, SmallTypes, OptionTypes, QuoteTypes, OptionMetaTypes, ChainTypes
using DateUtil, DictUtil

const HINT_TSS = 24 * 28
const HINT_OQS = 100
const HINT_XPIRS = 40

const OutDir = "C:/data/db/hand/"
const DF_FILE = DateFormat("yyyymm")

struct UnderTime
    under::Currency
    open::Currency
    hi::Currency
    lo::Currency
end

struct HistChain
    chain::Dict{DateTime,Dict{Date,Vector{OptionQuote}}}
    under::Dict{DateTime,UnderTime}
end
function HistChain()
    hc = HistChain(Dict{DateTime,Dict{Date,Vector{OptionQuote}}}(), Dict{DateTime,UnderTime}())
    sizehint!(hc.chain, HINT_TSS)
    sizehint!(hc.under, HINT_TSS)
    return hc
end

# const DataType = Dict{DateTime,Dict{Date,Vector{OptionQuote}}}

function getOqs(data::HistChain, ts::DateTime, xpir::Date)::Vector{OptionQuote}
    return data.chain[ts][xpir]
end

getUnder(data::HistChain, ts::DateTime) = data.under[ts]

getExpirs(data::HistChain, ts::DateTime) = filter(x -> x < Date(2025, 1, 1), keys(data.chain[ts]))

function getTss(data::HistChain)
    return sort(collect(keys(data.under)))
end

# function getXoqss(data, ts::DateTime, xpirs, curp::Currency, legsCheck=LEGS_EMPTY)
#     dictFromKeys(xpirs) do xpir
#         all = ChainUtil.oqssAll(getChain(data, ts, xpir))
#         entry = ChainUtil.filtEntry(all, curp, legsCheck)
#         return (;all, entry)
#     end
# end

function paths(y, m)
    dateStr = Dates.format(Date(y, m, 1), DF_FILE)
    baseDir = mkpath(joinpath(OutDir, dateStr))
    pathCall = joinpath(baseDir, "call.ser")
    pathPut = joinpath(baseDir, "put.ser")
    pathUnder = joinpath(baseDir, "under.ser")
    return pathCall, pathPut, pathUnder
end

function load(y, m)::HistChain
    data = HistChain()
    pathCall, pathPut, pathUnder = paths(y, m)
    loadOpt(proc(data, Style.call), pathCall)
    loadOpt(proc(data, Style.put), pathPut)
    loadUnder(tound(data), pathUnder)
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

end