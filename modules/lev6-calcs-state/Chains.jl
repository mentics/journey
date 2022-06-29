module Chains
using Dates
using CollUtil, DictUtil, DateUtil, CalcUtil, LogUtil, VectorCalcUtil
using Globals, BaseTypes, SH, SmallTypes, ChainTypes, QuoteTypes, OptionTypes, OptionMetaTypes
using TradierData, Caches
using DataHelper, Markets, Calendars, Expirations

export chains, ivs, calcNearIv
export quoter, optQuoter

function chains(; up=false)::CHAINS_TYPE
    cache!(CHAINS_TYPE, CHAINS, tooOld(PERIOD_UPDATE, isMarketOpen()); up) do
        up || @log error "Chains not up to date"
        newVal()
    end
end

quoter(x, act::Action.T=Action.open)::Quote = calcQuote(chainLookup, x, act)
optQuoter(x, act::Action.T=Action.open)::OptionQuote = calcOptQuote(chainLookup, x, act)

ivs(exps=expirs(), chs=chains()) = [(exp, round(calcNearIv(exp, chs); digits=4)) for exp in exps]

#region Local
const CHAINS = :chains
const CHAINS_TYPE = Dict{Date,OptionChain}
const PERIOD_UPDATE = Second(31)

whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = whenMarket(from, isMktOpen, nextMktChange, PERIOD_UPDATE)

canTrade() = ( delta = now(UTC) - market().tsUpdate ; delta <= PERIOD_UPDATE+Second(5) || error("Don't trade when chains data out of date: ", delta, " seconds") )

function update()::Nothing
    @log debug "updateChains"
    setCache!(CHAINS, newVal())
    return
end
function newVal()::CHAINS_TYPE
    chs = loadChains(expirs(), market().curp)
    updateIvs(chs)
    return chs
end
function updateIvs(chs)::Nothing
    @log debug "updateIvs"
    Globals.set(:vtyAvg, Dict(r[1] => r[2] for r in ivs(expirs(), chs))) # TODO: need to lock global? or just hope because it updates so seldom
    return
end

# TODO: what's the right way to aggregate?
# TODO: change, or add, to the calc for VIX
calcNearIv(dt::Date, chs=chains())::Float64 = avg(filter(x -> x != 0.0, getIv.(getMeta.(nearOqs(market().curp, dt, chs)))))
nearOqs(curp::Currency, d::Date, chs, dist::Int=20)::Vector{OptionQuote} = nearOqs(curp, chs[d].chain, dist)
nearOqs(curp::Currency, oqs, dist::Int=20)::Vector{OptionQuote} = filter(x -> abs(getStrike(x) - curp) <= dist, oqs)

function chainLookup(exp::Date, style::Style.T, strike::Currency)::OptionQuote
    res = find(chains()[exp].chain) do x; getStyle(x) === style && getStrike(x) === strike end
    isnothing(res) ? error("Could not quote $(exp), $(style), $(strike)") : res
end
# lup(ch::OptionChain; style=nothing, strike=nothing) = filter(x -> (isnothing(style) || getStyle(x) == style) && (isnothing(strike) || getStrike(x) == strike), ch.chain)

function loadChains(expirations::AVec{Date}, cp::Currency)::CHAINS_TYPE
    chains = CHAINS_TYPE()
    for exp in expirations
        try
            tradChain = tradierOptionChain(exp)
            chains[exp] = procChain(exp, cp, tradChain)
        catch e
            if isnothing(snap())
                rethrow(e)
            else
                # TODO: log it
                println("Exception loading chain $(exp) in $(expirations)")
                println(e)
            end
        end
    end
    return chains
end

function procChain(exp::Date, cp::Currency, data::Vector{Dict{String,Any}})::OptionChain
    res = Vector{OptionQuote}()
    for raw in data
        strike = Currency(raw["strike"])
        # if abs(strike - cp) > 48
        # # if abs(1.0 - strike / cp) > 0.1
        #     continue
        # end
        style = isCall(raw) ? Style.call : Style.put
        if isnothing(raw["bid"]) || isnothing(raw["ask"])
            if !isnothing(snap()) || isMarketOpen()
                @log debug "nothing bid found" exp strike style
            end
            continue
        end
        qt = Quote(Action.open, C(raw["bid"]), C(raw["ask"]))
        opt = Option(style, exp, strike)
        # TODO: cleanup
        if !haskey(raw, "greeks") || isnothing(raw["greeks"])
            @log debug "Missing greeks" opt qt
        end
        newOpt = OptionQuote(opt, qt, toOptionMeta(raw["greeks"]))
        push!(res, newOpt)
    end
    sort!(res, by=getStrike)
    return OptionChain(res, nowMs())
end

isCall(chainItem) = occursin("Call", chainItem["description"])
toOptionMeta(::Nothing) = OptionMeta(0.0)
function toOptionMeta(greeks::Dict{String,T}) where T
    val = tryKeys(greeks, 0.0, "mid_iv", "ask_iv", "bid_iv")
    @assert val >= 0.0 "toOptionMeta $(val) $(greeks)"
    OptionMeta(val)
end
#endregion

end