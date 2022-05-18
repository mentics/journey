module ConTime
using Dates
using SH, Globals
using LegMetaTypes
using Expirations
using CmdStrats

function run(exp::Date, numSnaps::Int)
    lmsPos = LegMeta[]
    sns = numSnaps:-1:1
    for sn in sns
        snap(sn)
        found = ana(exp; headless=true, lmsPos)
        if found > 0
            append!(lmsPos, tos(LegMeta, ar(1)))
        end
    end
    return lmsPos
end

end