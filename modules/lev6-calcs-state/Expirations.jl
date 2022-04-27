module Expirations
using Dates
using LogUtil
using Caches, TradierData, Markets
using DataHelper

export expir, expirs

expir(ex::Int=1; td=false)::Date = (ex == 0 ? expirs(;td=true)[1] : (exps = expirs(;td); !td && market().startDay == exps[1] ? exps[ex+1] : exps[ex]))
function expirs(; td=false, up=false)::Vector{Date}
    _expirs = cache!(EXPIRS_TYPE, EXPIRS, Hour(12); up) do
        up || @log error "Expirs not up to date"
        newVal()
    end
    return td || _expirs[1] != market().startDay ? _expirs : _expirs[2:end]
end

#region Local
const EXPIRS = :expirs
const EXPIRS_TYPE = Vector{Date}

whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = nextMktChange + Second(77)

function update()::Nothing
    @log debug "updateExpirs"
    exps = newVal()
    setCache!(EXPIRS, exps)
    return
end
newVal()::Vector{Date} = tradierExpirations()[1:20]
# getExpirations(numExpirs::Int)::EXPIRS_TYPE = tradierExpirations()[1:numExpirs]
#endregion

end