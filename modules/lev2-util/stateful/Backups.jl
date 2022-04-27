module Backups
using Globals, FileUtil

export logOrderRaw, loadOrderLogged

setBasePath(path) = basePath[] = path
setBasePath() = basePath[] = defBasePath()

function logOrderRaw(tord::Dict{String,Any})
    oid = tord["id"]
    path = pathOrder(oid)
    if isfile(path)
        loaded = loadJson(path)
        loaded["status"] == tord["status"] && return false
    end
    writeJson(path, tord)
    return true
end

loadOrderLogged(oid::Int)::Dict{String,Any} = loadJson(pathOrder(oid))

#region Local
defBasePath() = dirData("save")
const basePath = Ref{String}(defBasePath())
pathOrder(oid::Int)::String = joinpath(basePath[], "orders", string(oid) * ".json")
#endregion

end