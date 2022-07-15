module SnapUtil
using Dates
using Globals

const SnavePath = dirData("snave")

countSnaps()::Int = length(readdir(SnavePath))
earliestSnap()::Date = Date(readdir(SnavePath; sort=true, join=false)[1], SNAP_DATEFORMAT)
function snapExpirs()
    names = filter(x -> occursin("tradierOptionChain", x), readdir(joinpath(SnavePath, snap()); join=false, sort=false))
    return sort!(map(name -> Date(match(r"~(.+)~(.+).json", name).captures[2]), names))
end

const SNAP_DATEFORMAT = dateformat"yyyy-mm-dd.HH-MM"

end