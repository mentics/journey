module TradierConfig
using Dates
using Globals, BaseTypes, CollUtil, DictUtil, FileUtil, LogUtil

export TRADIER_DATE_FORMAT, TRADIER_EMPTY, TradierResp, TradierRespVec
export setRecording, clearOverride, setOverride
export CallInfo, Call, CallVec

const TRADIER_DATE_FORMAT = dateformat"yyyy-mm-dd"
const TradierResp = Dict{String,Any}
const TradierRespVec = Vector{TradierResp}
const TradierResps = Union{TradierResp,TradierRespVec}
const TRADIER_EMPTY = TradierRespVec()

struct CallInfo{T}
    sym::Symbol
    args::Any
end

# CallInfo{T}(sym::Symbol, args::Any=nothing) where T<:Type{TradierResps} = CallInfo{T}(sym, args)
Call(sym::Symbol) = CallInfo{TradierResp}(sym, nothing)
Call(sym::Symbol, args) = CallInfo{TradierResp}(sym, args)
CallVec(sym::Symbol) = CallInfo{TradierRespVec}(sym, nothing)
# CallVec(sym::Symbol, args) = CallInfo{TradierRespVec}(sym, args)

Globals.tenv(sym::Symbol) = ( Globals.set(:tierEnv, sym) ; push!(cfg, pairs(sym === :prod ? ENV_PROD : ENV_PAPER)...) )
# function Globals.tenv(sym::Symbol)
#     Globals.set(:tierEnv, sym)
#     eee = sym === :prod ? ENV_PROD : ENV_PAPER
#     push!(cfg, pairs(eee)...)
# end

setRecording(val::Union{Nothing,AStr})::Nothing = ( cfg[:recording] = val ; return )

setOverride(sym::Symbol, resp::TradierResps) = push!(cfg[:override], (sym, resp))
clearOverride() = empty!(cfg[:override])

#region Regional
export getAccountId, getDefaultSymbol
export getHeaders, toTradierUrl
export ifOverride, handleRecording

getAccountId()::String = cfg[:accountId]
getDefaultSymbol()::String = cfg[:defaultSymbol]
# getApiKey() = cfg[:env].apiKey
# getBaseUrl() = cfg[:env].baseUrl

getHeaders() = (("Authorization", "Bearer $(cfg[:apiKey])"),
             ("Accept", "application/json"))
            #  ("Accept-Encoding", "gzip"))

toTradierUrl(pathQuery::AStr) = cfg[:baseUrl] * pathQuery

function ifOverride(info::CallInfo{T})::Union{Nothing,T} where T
    ride = find(o -> first(o) == info.sym, cfg[:override])
    @assert isnothing(ride) || isnothing(cfg[:recording])
    if !isnothing(ride)
        walkKeys(ride[2]) do d, k
            k == "tag" ? (d["tag"] = replace(d["tag"], "tidOpenLast" => string(Globals.get(:tidOpenLast))) ; false) : true
        end
        return ride[2]
    end
    name = snap()
    return !isnothing(name) ? loadSnave(name, info) : nothing
end

function handleRecording(info::CallInfo, str::AStr)
    name = cfg[:recording]
    isnothing(name) && return
    dir = joinpath(SnavePath, name)
    mkpath(dir)
    writeStr(joinpath(dir, filename(info.sym, name, info.args)), str)
    return
end
#endregion

#region Local
const SnavePath = joinpath("data", "snave")
const cfg = Dict{Symbol,Any}()

function __init__()
    # only set default config if not already set
    haskey(cfg, :defaultSymbol) || setDefaultConfig()
end
function setDefaultConfig()
    tenv(:prod)
    push!(cfg,
        :defaultSymbol => "SPY",
        :override => Vector{Tuple{Symbol,TradierResps}}(),
        :recording => nothing, # Union{Nothing,AStr}
    )
end

const ENV_PROD = (;
    env = :prod,
    accountId = "6YA21612",
    apiKey = "***REMOVED***",
    baseUrl = "https://api.tradier.com/v1"
)

const ENV_PAPER = (;
    env = :paper,
    accountId = "VA90524539",
    apiKey = "***REMOVED***",
    baseUrl = "https://sandbox.tradier.com/v1"
)

function loadSnave(name::AStr, info::CallInfo{T})::Union{Nothing,T} where T
    path = joinpath(SnavePath, name, filename(info.sym, name, info.args))
    if isfile(path)
        @log debug "Loading: $(path)"
        return loadJson(path, T)
    else
        # if dev()
        #     @warn "loadSnave did not find file. Returning empty response because devMode" path
        #     return T()
        # else
            @log warn "loadSnave did not find file, falling back on call" path
            return nothing
        # end
    end
end

# This works because isnothing returns true for the empty tuple ()
filename(pre, name, args) = isnothing(args) ? "$(pre)~$(name).json" : "$(pre)~$(name)~$(stringifyArgs(args)).json"

stringifyArgs(arg::Union{Int,Currency,Date}) = string(arg)
stringifyArgs(args) = join([string(arg) for arg in args], "~")
#endregion

end