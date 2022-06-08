module SnapUtil
using Globals

const SnavePath = dirData("snave")

countSnaps()::Int = length(readdir(TradierConfig.SnavePath))
earliestSnap()::Date = Date(readdir(TradierConfig.SnavePath; sort=true, join=false)[1], SNAP_DATEFORMAT)
countSnapExpirs()::Int = count(x -> occursin("tradierOptionChain", x), readdir(joinpath(SnavePath, snap()); join=false, sort=false))

end