module FilesJLD2
using JLD2
import DateUtil
import Paths:Paths,params_hash

function Paths.save_data(path; kws...)
    !isempty(kws) || throw(ArgumentError("FilesJLD2 save_data requires keyword arguments"))
    mkpath(dirname(path))
    JLD2.jldsave(path; kws...)
end

function Paths.save_data_params(parent_dir, params; suffix::Union{Nothing,String}=nothing, kws...)
    !isempty(kws) || throw(ArgumentError("FilesJLD2 save_data_params requires keyword arguments"))
    dir = joinpath(parent_dir, params_hash(params))
    mkpath(dir)
    suffix_full = isnothing(suffix) ? "" : "-$(suffix)"
    params_path = joinpath(dir, "params.jld2")
    if !isfile(params_path)
        JLD2.jldsave(params_path; params)
    else
        # TODO: verify same params
    end
    path = joinpath(dir, "data$(suffix_full).jld2")
    JLD2.jldsave(path; kws...)
    return path
end

function Paths.load_data(path, names...; age=DateUtil.FOREVER2, asof=DateUtil.DATETIME_BEFORE)
    !isempty(names) || throw(ArgumentError("FilesJLD2 load_data requires names"))
    Paths.check_file_mtime(path, age, asof)
    return JLD2.load(path, names...)
end

function Paths.load_data_params(parent_dir, params, names...; suffix::Union{Nothing,String}=nothing, age=DateUtil.FOREVER2, asof=DateUtil.DATETIME_BEFORE, latest=false)
    !isempty(names) || throw(ArgumentError("FilesJLD2 load_data_params requires names"))
    dir = joinpath(parent_dir, params_hash(params))
    if latest
        path = sort!(filter!(s -> startswith(basename(s), "data"), readdir(dir; join=true)), by=mtime)[end]
    else
        suffix_full = isnothing(suffix) ? "" : "-$(suffix)"
        path = joinpath(dir, "data$(suffix_full).jld2")
    end
    Paths.check_file_mtime(path, age, asof)

    params_path = joinpath(dir, "params.jld2")
    params = JLD2.load(params_path, "params")
    data = JLD2.load(path, names...)
    # global kdata = data
    return (;path, data)
    # return (;params, data...)
end

end