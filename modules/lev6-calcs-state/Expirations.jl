module Expirations
using Dates, OffsetArrays
using LogUtil, DateUtil, CollUtil
using Caches, TradierData, Markets
using DataHelper

export expir, expirs

export xp
const xp = @__MODULE__

expir(ex::Int=1, sym="SPY"; kws...)::Date = expirs(sym; kws...)[ex] # (ex == 0 ? expirs(;td=true)[1] : (exps = expirs(;td); !td && market().startDay == exps[1] ? exps[ex+1] : exps[ex]))
function expirs(sym="SPY"; age=Hour(12))::OffsetArray{Date}
    _expirs = cache!(EXPIRS_TYPE, Symbol("expirs", '-', sym), age) do
        newVal(sym)
    end
    return _expirs
end

xpirsinrange(bdaysmin::Integer, bdaysmax::Integer) = xpirsinrange(today(), bdaysmin, bdaysmax, expirs())
xpirsinrange(from::Date, bdaysmin::Integer, bdaysmax::Integer) = xpirsinrange(from, bdaysmin, bdaysmax, xpirs())
function xpirsinrange(from::Date, bdaysmin::Integer, bdaysmax::Integer, xpirs)
    starti = CollUtil.gtee(xpirs, bdaysAfter(from, bdaysmin))
    endi = CollUtil.gtee(xpirs, bdaysAfter(from, bdaysmax))
    return xpirs[starti:endi]
end

whichExpir(d::Date)::Int = searchsortedfirst(expirs(), d)

# xprGte(date::Date)::Int = whichExpir(expirGte(date))
# xpirGte(date::Date)::Date = gtee(expirs(), date)

#region Local
const EXPIRS_TYPE = OffsetArray{Date}

whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = nextMktChange + Second(77)

function update()::Nothing
    @log debug "updateExpirs"
    exps = newVal("SPY")
    setCache!(EXPIRS, exps)
    return
end
# TODO: clean this up
using Globals, SnapUtil
function newVal(sym::String)::OffsetArray{Date}
    exps = isnothing(snap()) ? tradierExpirations(sym) : SnapUtil.snapExpirs(snap())
    # num = isnothing(snap()) ? 20 : SnapUtil.countSnapExpirs()
    # println("num expirs: ", num)
    exps[1] == market().startDay || insert!(exps, 1, exps[1])
    return OffsetArray(exps, 0:length(exps)-1)

    # num = min(21, length(exps))
    # return OffsetArray(exps[1:num], 0:num-1)

    # if exps[1] == market().startDay
    #     # println("exps[1] was market startday")
    #     return OffsetArray(exps, 0:min(21, length(exps)-1))
    # else
    #     return OffsetArray(vcat(exps[1], exps), 0:min(21, length(exps)))
    # end
end
#endregion

end