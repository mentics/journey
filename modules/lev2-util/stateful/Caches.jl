module Caches
using Dates
using DateUtil, DictUtil, ThreadUtil, LogUtil

export cache!, setCache!

function (cache!(provider::Function, ::Type{T}, sym::Symbol, period::Period; refresh=false)::T) where T
    return runSync(Lock) do
        prop = get(Props, sym, nothing)
        if refresh || isnothing(prop) || prop.ts < (now(UTC) - period)
            prop = updateCache!(sym, provider())
        end
        return prop.val
    end
end

function setCache!(sym::Symbol, val)::Nothing
    runSync(Lock) do
        updateCache!(sym, val)
    end
    return
end

#region Local
# TODO: figure out how to do this better instead of Dict
const PROP_TYPE = NamedTuple{(:val,:ts),Tuple{Any,DateTime}}
const Props = Dict{Symbol,PROP_TYPE}()
const Lock = ReentrantLock()

function updateCache!(sym::Symbol, val)
    @log info "Updating $(sym) in Caches"
    Props[sym] = (;val, ts=now(UTC))
end
#endregion

end