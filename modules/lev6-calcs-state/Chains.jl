module Chains
using Dates
using CollUtil, DictUtil, DateUtil, CalcUtil, LogUtil, VectorCalcUtil
using Globals, BaseTypes, SH, Bins, SmallTypes, ChainTypes, QuoteTypes, OptionTypes, OptionMetaTypes, LegTypes, LegMetaTypes
using TradierData, Caches
using DataHelper, Markets, Calendars, Expirations

export CH
const CH = @__MODULE__

export chains, chain, ivs, calcNearIv
export quoter, optQuoter, isQuotable

using ChainUtil
getOqss(i::Int, curp::Currency, legsCheck=LegMeta[])::Oqss = ChainUtil.getOqss(chain(expir(i)).chain, curp, legsCheck)
getOqss(expr::Date, curp::Currency, legsCheck=LegMeta[])::Oqss = ChainUtil.getOqss(chain(expr).chain, curp, legsCheck)

# ftrue(_) = true

# distRatio(x1, x2) = x2 == 0.0 ? 0.0 : abs(1.0 - x1 / x2)

# const OQSS_SNAP2 = Dict{String,Oqss}()
# oqssSnap(snapName::String, exp::Date)::Oqss = (key = string(snapName, '-', exp) ; useKey(OQSS_SNAP2, key) do
#     runSync(lock) do
#         chains = chainSnap(snapName, false)
#         res = getOqss(chains[exp].chain)
#         println("Cached oqss for ", key)
#         return res
#     end
# end)

# using ThreadUtil
# const lock = ReentrantLock()
# const CHAINS_SNAP = Dict{String,ChainsType}()
# chainSnap(snapName::String, revert=true)::ChainsType = useKey(CHAINS_SNAP, snapName) do
#     runSync(lock) do
#         back = snap()
#         !isnothing(back) || error("Don't chainsSnap when not snapped")
#         snap(snapName)
#         res = chains()
#         !revert || snap(back)
#         println("Cached chains for ", snapName)
#         return res
#     end
# end

# const OQ_EMPTY = Vector{OptionQuote}()
# chain(i::Int)::Vector{OptionQuote} = chain(expir(i))
# chain(expr::Date)::Vector{OptionQuote} = ( chs = chains() ; haskey(chs, expr) ? chs[expr].chain : OQ_EMPTY )
# function chains(; up=false)::ChainsType
#     cache!(ChainsType, CHAINS, tooOld(PERIOD_UPDATE, isMarketOpen()); up) do
#         up || @log error "Chains not up to date"
#         newVal()
#     end
# end

using ThreadPools
chain(xpir::Date; kws...)::OptionChain = chains([xpir]; kws...)["SPY"][xpir]
chain(xpir::Date, sym::String; kws...)::OptionChain = chains([xpir], [sym]; kws...)[sym][xpir]
# chain(xprs, sym::String; kws...)::OptionChain = chains(xprs, [sym]; kws...)[sym]
function chains(xpirs, syms=("SPY",); age=PERIOD_UPDATE3)::SymChainsType
    @assert !isempty(xpirs) # applicable(length, xprs)
    @assert applicable(length, syms)
    @assert eltype(xpirs) == Date
    @assert eltype(syms) == String string(eltype(syms), " != ", String)
    # @show syms xprs
    _, chs = logqbmap((sym, xpr) for sym in syms, xpr in xpirs) do (sym, xpr)
        try
            # println("Refreshing chain for ", sym, ' ', xpr, " in thread ", Threads.threadid())
            return (sym, xpr, refreshChain(sym, xpr, age))
        catch e
            println("Exception in multithreaded get chains")
            prerr(e)
            return
        end
    end

    res = SymChainsType()
    if isempty(chs)
        @logerr "No chains found" sym xpirs
    else
        for (sym, xpr, ch) in chs
            dictSym = get!(Dict, res, sym)
            dictSym[xpr] = ch
        end
    end
    return res
end

function refreshChain(sym::String, xpr::Date, age::Period)::OptionChain
    return cache!(OptionChain, Symbol("chain-", sym, '-', xpr), age) do
        return loadChain(sym, xpr)
    end
end

canQuoteXpr(xpr::Date)::Bool = xpr >= market().startDay
quoter(x, act::Action.T=Action.open)::Quote = calcQuote(chainLookup, x, act)
optQuoter(x, act::Action.T=Action.open)::Union{Nothing,OptionQuote} = calcOptQuote(chainLookup, x, act)
function SH.calcQuote(lookup, legs::Coll, act::Action.T=Action.open)::Quote
    sumQuotes(calcQuote(lookup, leg, act) for leg in legs)
end
function SH.calcQuote(lookup, leg::T, act::Action.T=Action.open)::Quote where T # <:Union{Leg,LegMeta,<:LegTrade}
    oq = lookup(getExpiration(leg), getStyle(leg), getStrike(leg))
    dir = DirSQA(getSide(leg), getQuantity(leg), act)
    return newQuote(getQuote(oq), dir)
end
function SH.calcOptQuote(lookup, leg::T, act::Action.T=Action.open)::OptionQuote where T # <:Union{Leg,LegMeta,<:LegTrade}
    oq = lookup(getExpiration(leg), getStyle(leg), getStrike(leg))
    dir = DirSQ(getSide(leg), getQuantity(leg))
    return OptionQuote(getOption(leg), newQuote(getQuote(oq), DirSQA(dir, act)), newOptionMeta(getOptionMeta(oq), dir))
end
# isQuotable(o::Option)::Bool = isQuotable(getStyle(o), getExpiration(o), getStrike(o))
# function isQuotable(style::Style.T, exp::Date, strike::Currency)::Bool
#     res = find(chains()[exp].chain) do x; getStyle(x) === style && getStrike(x) === strike end
#     return !isnothing(res) && getBid(res) > 0.0
# end

function ivs(exps::AVec{Date}=expirs(), curp::Real=market().curp, chs::ChainsType=chains())
    error("ivs not impled")
    return [(exp, calcNearIv(exp, curp, chs)) for exp in exps]
end

#region Local
const CHAINS = :chains
const PERIOD_UPDATE3 = Minute(1)

whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = whenMarket(from, isMktOpen, nextMktChange, PERIOD_UPDATE2)

canTrade() = ( delta = now(UTC) - market().tsUpdate ; delta <= PERIOD_UPDATE3+Second(10) || error("Don't trade when chains data out of date: ", delta, " seconds") )

function update()::Nothing
    @log debug "updateChains"
    setCache!(CHAINS, newVal())
    return
end
function newVal()::ChainsType
    error("newVal deprecated")
    chs = loadChains(expirs())
    updateIvs(market().curp, chs)
    return chs
end
function updateIvs(curp, chs)::Nothing
    error("ivs not impled")
    @log debug "updateIvs"
    Globals.set(:vtyAvg, Dict(r[1] => r[2] for r in ivs(expirs(), curp, chs))) # TODO: need to lock global? or just hope because it updates so seldom
    return
end

# TODO: what's the right way to aggregate?
# TODO: change, or add, to the calc for VIX
function calcNearIv(dt::Date, curp::Real, chs::ChainsType=chains())::Union{Nothing,Float64}
    ivs = getIv.(getMeta.(nearOqs(curp, dt, chs)))
    isnothing(findfirst(x -> x == 0.0, ivs)) || @logret "No ivs in calcNearIv" dt length(chs) curp
    # ivs = filter(x -> x != 0.0, )
    !isempty(ivs) || @logret "No ivs in calcNearIv" dt length(chs) curp
    # @assert length(ivs) > 0 "No ivs in calcNearIv($(dt), $(length(chs))) curp=$(curp)"
    round(avg(ivs); digits=4)
end
nearOqs(curp::Currency, d::Date, chs, dist::Int=20)::Vector{OptionQuote} = nearOqs(curp, chs[d].chain, dist)
nearOqs(curp::Currency, oqs, dist::Int=20)::Vector{OptionQuote} = filter(x -> abs(getStrike(x) - curp) <= dist, oqs)

chainLookup(opt::Option) = chainLookup(getExpiration(opt), getStyle(opt), getStrike(opt))
function chainLookup(exp::Date, style::Style.T, strike::Currency)::Union{Nothing,OptionQuote}
    res = find(chain(exp).chain) do x; getStyle(x) === style && getStrike(x) === strike end
    !isnothing(res) || println("WARN: Could not quote $(exp), $(style), $(strike)")
    return res
end
# lup(ch::OptionChain; style=nothing, strike=nothing) = filter(x -> (isnothing(style) || getStyle(x) == style) && (isnothing(strike) || getStrike(x) == strike), ch.chain)

function loadChains(expirations::AVec{Date}, symbol="SPY")::ChainsType
    error("loadChains deprecated")
    chains = ChainsType()
    for xpr in expirations
        chains[xpr] = loadChain(symbol, xpr)
    end
    return chains
end

function loadChain(sym::String, xpr::Date)::OptionChain
    try
        t1 = @timed tradChain = tradierOptionChain(xpr, sym)
        t2 = @timed res = procChain(xpr, tradChain)
        @log info "Loaded chain" sym xpr Threads.threadid() t1.time t2.time
        return res
    catch e
        rethrow(e)
        # if isnothing(snap())
        #     rethrow(e)
        # else
        #     # TODO: log it
        #     println("Exception loading chain $(exp) in $(expirations)")
        #     showerror(stdout, e, catch_backtrace())
        # end
    end
end

function procChain(exp::Date, data::Vector{Dict{String,Any}})::OptionChain
    res = Vector{OptionQuote}()
    for raw in data
        strike = Currency(raw["strike"])
        # if abs(strike - cp) > 48
        # # if abs(1.0 - strike / cp) > 0.1
        #     continue
        # end
        style = occursin("Call", raw["description"]) ? Style.call : Style.put
        if isnothing(raw["bid"]) || isnothing(raw["ask"])
            if !isnothing(snap()) || isMarketOpen()
                @log debug "nothing bid found" exp strike style
            end
            continue
        end
        qt = Quote(C(raw["bid"]), C(raw["ask"]))
        opt = Option(style, exp, strike)
        # TODO: cleanup
        if !haskey(raw, "greeks") || isnothing(raw["greeks"])
            @log debug "Missing greeks" opt qt
        end
        newOpt = OptionQuote(opt, qt, toOptionMeta(raw["greeks"]))
        push!(res, newOpt)
    end
    sort!(res, by=x -> (getStrike(x), -Int(getStyle(x))))
    return OptionChain(res, nowMs())
end

toOptionMeta(::Nothing) = OptionMeta()
function toOptionMeta(greeks::Dict{String,T}) where T
    # val = tryKeys(greeks, 0.0, "mid_iv", "ask_iv", "bid_iv")
    # @assert val >= 0.0 "toOptionMeta $(val) $(greeks)"
    return OptionMeta(greeks)
end
#endregion

end