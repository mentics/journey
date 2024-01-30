module FilesArrow
using Arrow, JLD2
import DataFrames:AbstractDataFrame,DataFrame
import DateUtil
import Paths:Paths,safe_hash

function Paths.save_data(path, df::AbstractDataFrame; update=false)
    mkpath(dirname(path))
    !update || Paths.handle_update(path)
    Arrow.write(path, df)
end

function Paths.save_data_params(parent_dir, params, df::AbstractDataFrame; update=false)
    dir = joinpath(parent_dir, safe_hash(params))
    # if isdir(dir)
    #     error("dir already exists for save_data, todo: verify params match: $(dir)")
    # end
    mkpath(dir)
    path_data = joinpath(dir, "data.arrow")
    path_params = joinpath(dir, "params.jld2")
    if update && isfile(path_data)
        Paths.handle_update(path_data)
    end
    JLD2.jldsave(path_params; params)
    Arrow.write(path_data, df)
end

function Paths.load_data(path, ::Type{DataFrame}; age=DateUtil.FOREVER2, asof=DateUtil.DATETIME_BEFORE, copycols=false)
    Paths.check_file_mtime(path, age, asof)
    return DataFrame(Arrow.Table(path); copycols)
end

function Paths.load_data_params(parent_dir, ::Type{DataFrame}; age=DateUtil.FOREVER2, asof=DateUtil.DATETIME_BEFORE, copycols=false, params=nothing, hash=nothing)
    if !isnothing(hash)
        dir = joinpath(parent_dir, hash)
    elseif !isnothing(params)
        dir = joinpath(parent_dir, safe_hash(params))
    else
        dir = sort!(readdir(parent_dir; join=true), by=mtime)[end]
    end

    path = joinpath(dir, "data.arrow")
    Paths.check_file_mtime(path, age, asof)
    df = DataFrame(Arrow.Table(path); copycols)
    path_params = joinpath(dir, "params.jld2")
    params = JLD2.load(path_params, "params")
    return (;df, params, path)
end

end