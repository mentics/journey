module Theo
using Dates
using ChainTypes
using SH, Globals
using Calendars, Chains, Markets
using Snapshots

Loaded = BitSet()
AllData = NamedTuple[]

function reset()
    empty!(Loaded)
    empty!(AllData)
end

function run()
    Calendars.ensureSince(Snapshots.earliestSnap(), today() + Month(3))
    numSnaps = Snapshots.countSnaps()
    for i in 1:numSnaps
        i in Loaded && continue
        nam = snap(i)
        process(snapTs(nam))
        push!(Loaded, i)
    end
end

#region Local
function process(ts::DateTime)
    curp = market().curp
    for (expr, chain) in chains()
        exprDT = getMarketClose(expr)
        absTimeToExpir = exprDT - ts
        for oq in chain.chain
            strikeDist = getStrike(oq) - curp
            push!(AllData, (;
                oq, absTimeToExpir, strikeDist, insic=getExtrinsic(oq, curp)
            ))
        end
    end
end
#endregion

end