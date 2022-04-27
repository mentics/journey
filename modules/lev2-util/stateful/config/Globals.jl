module Globals
import Dates:now,UTC

export config
export snap, tenv, intest
export dev, devon, devoff

config()::Dict{Symbol,Any} = cfg
dev()::Bool = get(:devMode)
devon()::Bool = set(:devMode, true)
devoff()::Bool = ( set(:devoffLastRun, now(UTC)) ; set(:devMode, false) )
snap()::Union{Nothing,String} = get(:snap)
tenv()::Symbol = get(:tierEnv)
intest()::Bool = get(:testing)

#region Local
const cfg = Dict{Symbol,Any}()

function __init__()
    haskey(cfg, :vtyRatio) || push!(cfg,
        :vtyRatio => 0.8,
        :devMode => false,
        :incShortCals => false,
        :testing => false,
        :tierEnv => :paper,
        :snap => nothing,
        :Strats => Dict{Symbol,Any}(
            :maxStrikeDist => 20,
            :maxPutHeight => 4,
            :maxCallHeight => 8 # TODO: compare results if allow deeper calls
        )
    )
end

has(k::Symbol) = haskey(cfg, k)
set(k::Symbol, v::Any) = cfg[k] = v
get(k::Symbol) = cfg[k]
#endregion

end