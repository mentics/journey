module Theo
using Dates
using SH, BaseTypes, SmallTypes, ChainTypes
using Globals, OptionUtil, SnapUtil
using Calendars, Chains, Markets
using Snapshots

Loaded = BitSet()
AllCalls = NamedTuple[]
AllPuts = NamedTuple[]
DictCalls = Dict{Date,Vector{NamedTuple}}()
DictPuts = Dict{Date,Vector{NamedTuple}}()

function reset()
    empty!(Loaded)
    empty!(AllCalls)
    empty!(AllPuts)
    empty!(DictCalls)
    empty!(DictPuts)
end

function run()
    Calendars.ensureCal(SnapUtil.earliestSnap(), today() + Month(3))
    numSnaps = SnapUtil.countSnaps()
    for i in 1:numSnaps
        i in Loaded && continue
        nam = snap(i)
        println("proc snap: ", nam)
        resc, resp = process(Snapshots.snapToTs(nam))
        append!(AllCalls, resc)
        append!(AllPuts, resp)
        push!(Loaded, i)
    end
end

function toSpreadPricing(data)::Vector{Tuple}
    res = similar(data, Tuple)
    for i in 1:length(data)
        # ts, oq1, oq2, absTex, strikeDist, insic=getExtrinsic(oq1, curp), mid, netLo, netSho, strikeDiff
        o = data[i]
        # res[i] = (; ts=o.ts, curp=o.curp, toTuple(o.oq1)..., toTuple(o.oq2)...)
        res[i] = (o.ts, o.curp, toTuple(o.oq1)..., toTuple(o.oq2)...)
    end
    return res
end

function toPricing(data)::Vector{NamedTuple}
    res = similar(data, NamedTuple)
    for i in 1:length(data)
        # ts, oq1, absTex, strikeDist, insic=getExtrinsic(oq1, curp), mid, netLo, netSho, strikeDiff
        o = data[i]
        res[i] = (; ts=o.ts, curp=o.curp, toTuple(o.oq1)...)
    end
    return res
end

using FileUtil
function writeCsvPricing()
    @assert length(AllCalls) > 100
    basePath = "C:/data/tmp/pricing"
    writeCsv(joinpath(basePath, "calls.csv"), toPricing(AllCalls))
    writeCsv(joinpath(basePath, "puts.csv"), toPricing(AllPuts))
    writeCsv(joinpath(basePath, "spreads", "spread-calls.csv"), toSpreadPricing(AllCalls))
    writeCsv(joinpath(basePath, "spreads", "spread-puts.csv"), toSpreadPricing(AllPuts))
end

using ChainTypes, OptionTypes, QuoteTypes, OptionMetaTypes
toTuple(oq::OptionQuote) = (;
    toTuple(oq.option)...,
    toTuple(oq.quot)...,
    toTuple(oq.meta)...
)
toTuple(opt::Option) = (;
    style=opt.style,
    expiration=opt.expiration,
    strike=opt.strike
)
toTuple(q::Quote) = (;
    action=q.action,
    bid=q.bid,
    ask=q.ask
)
toTuple(om::OptionMeta) = (; iv=om.iv)

using DrawUtil
function drawStrikeDist(expr::Date, inter, style::Style.T=Style.call)
    nts = dict(style)[expr]
    ptsLo = ptsLong(nts, inter)
    ptsSho = ptsShort(nts, inter)
    ptsLoInv = ptsLong(nts, -inter)
    ptsShoInv = ptsShort(nts, -inter)
    # display(draw(ptsLo))
    # draw!(ptsSho)
    # draw!(ptsLoInv)
    # draw!(ptsShoInv)
    # return (ptsLo, ptsSho)
    condorLo = [(ptsLo[i][1], ptsLo[i][2] + ptsShoInv[i][2]) for i in eachindex(ptsLo)]
    condorSho = [(ptsLo[i][1], ptsLoInv[i][2] + ptsSho[i][2]) for i in eachindex(ptsLo)]
    display(draw(condorLo))
    display(draw!(condorSho))
    return (condorLo, condorSho)
end
ptsLong(nts, inter) = [(x.absTex.value/1000/60/60, x.netLo) for x in filter(x -> x.strikeDist in inter, nts)]
ptsShort(nts, inter) = [(x.absTex.value/1000/60/60, x.netSho) for x in filter(x -> x.strikeDist in inter, nts)]
dateCounts() = sort!([(expr, length(Theo.DictCalls[expr])) for expr in keys(DictCalls)]; by=x->x[1])

#region Local
dict(style::Style.T) = Style.call == style ? DictCalls : DictPuts

function process(ts::Dates.AbstractDateTime)::Tuple{Vector{NamedTuple},Vector{NamedTuple}}
    curp = market().curp
    calls = NamedTuple[]
    puts = NamedTuple[]
    for (expr, chain) in chains()
        haskey(DictCalls, expr) || (DictCalls[expr] = NamedTuple[])
        haskey(DictPuts, expr) || (DictPuts[expr] = NamedTuple[])
        resc = NamedTuple[]
        resp = NamedTuple[]
        exprDT = getMarketClose(expr)
        absTex = exprDT - ts
        procExpr(ts, curp, absTex, filter(x -> Style.call == getStyle(x), chain.chain), resc)
        procExpr(ts, curp, absTex, filter(x -> Style.put == getStyle(x), chain.chain), resp)
        append!(DictCalls[expr], resc)
        append!(DictPuts[expr], resp)
        append!(calls, resc)
        append!(puts, resp)
    end
    return (calls, puts)
end

function procExpr(ts::DateTime, curp::Currency, absTex::Period, oqs::Vector{OptionQuote}, res::Vector{NamedTuple})
    for i in 1:(length(oqs)-1)
        oq1, oq2 = oqs[i], oqs[i+1]
        s1, s2 = getStrike(oq1), getStrike(oq2)
        strikeDiff = abs(s1 - s2)
        netLo = calcNetLong(oq1, oq2) / strikeDiff
        netSho = calcNetShort(oq1, oq2) / strikeDiff
        strikeDist = getStrike(oq1) - curp
        mid = (s1 + s2)/2
        push!(res, (;
            ts, curp, oq1, oq2, absTex, strikeDist, insic=calcExtrin(oq1, curp), mid, netLo, netSho, strikeDiff
        ))
    end
end
#endregion

end