module SnapUtil
using Dates
using Globals, Caches

const SnavePath = dirData("snave")

countSnaps()::Int = length(readdir(SnavePath))
earliestSnap()::Date = Date(readdir(SnavePath; sort=true, join=false)[1], SNAP_DATEFORMAT)

function snapExpirs(nam)::Vector{Date}
    names = filter(x -> occursin("tradierOptionChain", x), readdir(joinpath(SnavePath, nam); join=false, sort=false))
    return sort!(map(name -> Date(match(r"~(.+)~(.+).json", name).captures[2]), names))
end

function snapDateTimes()
    dts = map(x -> DateTime(x, SNAP_DATEFORMAT), readdir(SnavePath; join=false, sort=false))
    (sort!(dts) ; unique!(dts))
    return dts
end

snapDates() = unique!(map(Date, snapDateTimes()))

function snapNames()::Vector{String}
    return cache!(Vector{String}, :snapNames, Minute(50)) do
        return sort!(readdir(SnavePath; join=false, sort=false))
    end
end

function snapExpirs()::Vector{Date}
    return cache!(Vector{Date}, :snapExpirs, Hour(1)) do
        all = reduce(vcat, map(snapNames()) do nam
            snapExpirs(nam)
        end)
        (sort!(all) ; unique!(all))
        return all
    end
end

snapName(dt::DateTime)::String = Dates.format(dt, SNAP_DATEFORMAT)
function snapName(date::Date, i::Int)::Union{Nothing,String}
    dts = filter(x -> Date(x) == date, SnapUtil.snapDateTimes())
    length(dts) >= i || return nothing
    return snapName(dts[i])
end

function lastSnap(date::Date)::Union{Nothing,String}
    dts = filter(x -> Date(x) == date, SnapUtil.snapDateTimes())
    !isempty(dts) || return nothing
    return snapName(dts[end])
end

const SNAP_DATEFORMAT = dateformat"yyyy-mm-dd.HH-MM"

end