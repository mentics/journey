module Chains
using Dates
using CollUtil, DictUtil, DateUtil, CalcUtil, LogUtil, VectorCalcUtil
using Globals, BaseTypes, SH, SmallTypes, ChainTypes, QuoteTypes, OptionTypes, OptionMetaTypes, LegTypes, LegMetaTypes
using TradierData, Caches
using DataHelper, Markets, Calendars, Expirations

export chains, getOqs, ivs, calcNearIv
export quoter, optQuoter, isQuotable
export Oqss

const Oqss = Styles{Sides{Vector{OptionQuote}}}
const CHAINS_TYPE = Dict{Date,OptionChain}

ftrue(_) = true

function getOqss(oqs::Vector{OptionQuote}, curp::Currency, legsCheck=LegMeta[], noLimit=false)::Oqss
    fconl = !isConflict(legsCheck, Side.long)
    fcons = !isConflict(legsCheck, Side.short)
    fcans = noLimit ? ftrue : canShort(Globals.get(:Strats), curp)
    oqsValid = filter(isValid(curp), oqs)
    oqsLong = filter(fconl, oqsValid)
    oqsCallLong = filter(SmallTypes.isCall, oqsLong)
    oqsPutLong = filter(SmallTypes.isPut, oqsLong)
    oqsShort = filter(x -> fcons(x) && fcans(x), oqsValid)
    oqsCallShort = filter(SmallTypes.isCall, oqsShort)
    oqsPutShort = filter(SmallTypes.isPut, oqsShort)
    return Styles(Sides(oqsCallLong, oqsCallShort), Sides(oqsPutLong, oqsPutShort))
end
function getOqss(oqs)::Oqss
    oqsValid = Iterators.filter(isValid, oqs)

    oqsLong = oqsValid # Iterators.filter(isLong, oqsValid)
    oqsShort = oqsValid # Iterators.filter(isShort, oqsValid)

    oqsCallLong = collect(Iterators.filter(SmallTypes.isCall, oqsLong))
    oqsPutLong = collect(Iterators.filter(SmallTypes.isPut, oqsLong))
    oqsCallShort = collect(Iterators.filter(SmallTypes.isCall, oqsShort))
    oqsPutShort = collect(Iterators.filter(SmallTypes.isPut, oqsShort))
    return Styles(Sides(oqsCallLong, oqsCallShort), Sides(oqsPutLong, oqsPutShort))
end

function findOqs(oqs, curp::Currency, dists; maxDiff=1.0)
    res = Vector{OptionQuote}(undef, length(dists))
    diffs = fill(Inf, length(dists)) # Vector{Currency}(Inf, length(dists))
    for oq in oqs
        dist = getStrike(oq) - curp
        for i in eachindex(diffs)
            diff = abs(dists[i] - dist)
            if diff < diffs[i]
                diffs[i] = diff
                res[i] = oq
            end
        end
    end
    isnothing(findfirst(x -> x > maxDiff, diffs)) || return nothing
    return res
end

const OQSS_SNAP2 = Dict{String,Oqss}()
oqssSnap(snapName::String, exp::Date)::Oqss = (key = string(snapName, '-', exp) ; useKey(OQSS_SNAP2, key) do
    runSync(lock) do
        chains = chainSnap(snapName, false)
        res = getOqss(chains[exp].chain)
        println("Cached oqss for ", key)
        return res
    end
end)

using ThreadUtil
const lock = ReentrantLock()
const CHAINS_SNAP = Dict{String,CHAINS_TYPE}()
chainSnap(snapName::String, revert=true)::CHAINS_TYPE = useKey(CHAINS_SNAP, snapName) do
    runSync(lock) do
        back = snap()
        !isnothing(back) || error("Don't chainsSnap when not snapped")
        snap(snapName)
        res = chains()
        !revert || snap(back)
        println("Cached chains for ", snapName)
        return res
    end
end

function chains(; up=false)::CHAINS_TYPE
    cache!(CHAINS_TYPE, CHAINS, tooOld(PERIOD_UPDATE, isMarketOpen()); up) do
        up || @log error "Chains not up to date"
        newVal()
    end
end

quoter(x, act::Action.T=Action.open)::Quote = calcQuote(chainLookup, x, act)
optQuoter(x, act::Action.T=Action.open)::OptionQuote = calcOptQuote(chainLookup, x, act)
isQuotable(o::Option)::Bool = isQuotable(getStyle(o), getExpiration(o), getStrike(o))
function isQuotable(style::Style.T, exp::Date, strike::Currency)::Bool
    res = find(chains()[exp].chain) do x; getStyle(x) === style && getStrike(x) === strike end
    return !isnothing(res) && getBid(res) > 0.0
end

ivs(exps::AVec{Date}=expirs(), curp::Real=market().curp, chs::CHAINS_TYPE=chains()) = [(exp, round(calcNearIv(exp, curp, chs); digits=4)) for exp in exps]

#region Local
const CHAINS = :chains
const PERIOD_UPDATE = Second(31)

whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = whenMarket(from, isMktOpen, nextMktChange, PERIOD_UPDATE)

canTrade() = ( delta = now(UTC) - market().tsUpdate ; delta <= PERIOD_UPDATE+Second(5) || error("Don't trade when chains data out of date: ", delta, " seconds") )

function update()::Nothing
    @log debug "updateChains"
    setCache!(CHAINS, newVal())
    return
end
function newVal()::CHAINS_TYPE
    curp = market().curp
    chs = loadChains(expirs(), curp)
    updateIvs(curp, chs)
    return chs
end
function updateIvs(curp, chs)::Nothing
    @log debug "updateIvs"
    Globals.set(:vtyAvg, Dict(r[1] => r[2] for r in ivs(expirs(), curp, chs))) # TODO: need to lock global? or just hope because it updates so seldom
    return
end

# TODO: what's the right way to aggregate?
# TODO: change, or add, to the calc for VIX
function calcNearIv(dt::Date, curp::Real, chs::CHAINS_TYPE=chains())::Float64
    ivs = filter(x -> x != 0.0, getIv.(getMeta.(nearOqs(curp, dt, chs))))
    @assert length(ivs) > 0 "No ivs in calcNearIv($(dt), $(length(chs))) curp=$(curp)"
    avg(ivs)
end
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
        style = occursin("Call", raw["description"]) ? Style.call : Style.put
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

toOptionMeta(::Nothing) = OptionMeta(0.0)
function toOptionMeta(greeks::Dict{String,T}) where T
    val = tryKeys(greeks, 0.0, "mid_iv", "ask_iv", "bid_iv")
    @assert val >= 0.0 "toOptionMeta $(val) $(greeks)"
    OptionMeta(val)
end
#endregion

end