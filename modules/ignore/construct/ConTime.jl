module ConTime
using Dates
using SH, Globals
using LegMetaTypes
using Expirations, Snapshots

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

dateFilter(from, to) = tup -> (from < tup[2] < to) && !endswith(tup[1], "06-30")
dateHourFilter(from, to, hour) =
    tup -> (from < tup[2] < to) &&
        Hour(tup[2]).value == hour &&
        !endswith(tup[1], "06-30")

function runForSnaps(f, filt=identity)
    snapTups = filter(filt, map(s -> (s, Snapshots.snapToTs(s)), Snapshots.allSnaps(false)))
    for (name, ts) in snapTups
        snap(name)
        f(name, ts)
    end
end

end