module Theo
using Dates
using SH, BaseTypes, SmallTypes, ChainTypes
using Globals, OptionUtil
using Calendars, Chains, Markets
using Snapshots

Loaded = BitSet()
AllCalls = NamedTuple[]
AllPuts = NamedTuple[]

function reset()
    empty!(Loaded)
    empty!(AllCalls)
    empty!(AllPuts)
end

function run()
    Calendars.ensureCal(Snapshots.earliestSnap(), today() + Month(3))
    numSnaps = Snapshots.countSnaps()
    for i in 1:numSnaps
        i in Loaded && continue
        nam = snap(i)
        resc, resp = process(snapTs(nam))
        append!(AllCalls, resc)
        append!(AllPuts, resp)
        push!(Loaded, i)
    end
end

#region Local
function process(ts::DateTime)::Tuple{Vector{NamedTuple},Vector{NamedTuple}}
    curp = market().curp
    resc = NamedTuple[]
    resp = NamedTuple[]
    for (expr, chain) in chains()
        exprDT = getMarketClose(expr)
        absTex = exprDT - ts
        procExpr(ts, curp, absTex, filter(x -> Style.call == getStyle(x), chain.chain), resc)
        procExpr(ts, curp, absTex, filter(x -> Style.put == getStyle(x), chain.chain), resp)
    end
    return (resc, resp)
end

# TODO: check that we have data within a few minutes of expiration because snave sched should be giving us that now
function procExpr(ts::DateTime, curp::Currency, absTex::Period, oqs::Vector{OptionQuote}, res::Vector{NamedTuple})
    for i in 1:(length(oqs)-1)
        oq1, oq2 = oqs[i], oqs[i+1]
        s1, s2 = getStrike(oq1), getStrike(oq2)
        strikeDiff = abs(s1 - s2)
        netLo = netLong(oq1, oq2) / strikeDiff
        netSho = netShort(oq1, oq2) / strikeDiff
        strikeDist = getStrike(oq1) - curp
        mid = (s1 + s2)/2
        push!(res, (;
            ts, oq1, absTex, strikeDist, insic=getExtrinsic(oq1, curp), mid, netLo, netSho, strikeDiff
        ))
    end
end
#endregion

end