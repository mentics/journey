module Globals
import Dates:now,UTC,DateTime

export config
export snap, snapTs, tenv, intest
export dev, devon, devoff
export dirData
export setvr, getvr

config()::Dict{Symbol,Any} = cfg
dev()::Bool = get(:devMode)
devon()::Bool = set(:devMode, true)
devoff()::Bool = ( set(:devoffLastRun, now(UTC)) ; set(:devMode, false) )
snap()::Union{Nothing,String} = get(:snap)
snapTs()::Union{Nothing,DateTime} = get(:snapTs)
tenv()::Symbol = get(:tierEnv)
intest()::Bool = get(:testing)

getvr() = Globals.get(:vtyRatio)
setvr(vr::Float64) = Globals.set(:vtyRatio, vr)

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
        :snapTs => nothing,
        :useCurp => true,
        :Strats => Dict{Symbol,Any}(
            :maxStrikeDist => 28.0,
            :maxPutHeight => 3.2,
            :maxCallHeight => 22.0 # TODO: compare results if allow deeper calls
        )
    )
end

dirData() = "G:/My Drive/sync/data/journey"
dirData(sub...) = mkpath(joinpath("G:/My Drive/sync/data/journey", sub...))

has(k::Symbol) = haskey(cfg, k)
set(k::Symbol, v::Any) = cfg[k] = v
get(k::Symbol) = cfg[k]
#endregion

end