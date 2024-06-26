module Paths
using Dates, Base64, StableHashTraits

# export db_incoming, db_input, db_checkpoint, db_infer
# export save_data, save_data_params, load_data, load_data_params

#region Public
function save_data end
function load_data end
function save_data_params end
function load_data_params end

function handle_update(path)
    if isfile(path) && Sys.iswindows()
        deldir = joinpath(splitdrive(path)[1], "delete")
        mkpath(deldir)
        mv(path, tempname(deldir))
    end
end

function db(items...)
    base = Sys.iswindows() ? joinpath("D:\\", "data", "sync", "db") : "/home/jshellman/sync/db"
    return joinpath(base, items...)
end

function db_old()
    return Sys.iswindows() ? joinpath("D:\\", "data", "db") : "/home/jshellman/data/db"
end

db_incoming(dirs...) = db("market", "incoming", dirs...)
db_thetadata(dirs...; sym) = db("market", "incoming", "thetadata", sym, dirs...)
db_models(dirs...) = db("ml", "models", dirs...)
db_input(model_name) = db_models(model_name, "input")

# function db_input_latest(model_name)
#     dirs = sort!(readdir(db_input(model_name); join=true), by=mtime)
#     return joinpath(dirs[end], "data.arrow")
# end

db_checkpoint(model_name) = db_models(model_name, "checkpoint")
db_infer(model_name) = db_models(model_name, "infer")
db_output(model_name) = db_models(model_name, "output")
#endregion

#region Nearby
function check_file_mtime(path, age, asof)
    isfile(path) || throw("File not found $(path)")
    mt = unix2datetime(mtime(path))
    mt >= asof || throw("File $(path) out of date $(mt) < $(asof)")
    a = now(UTC) - mt
    (isnothing(age) || a < age) || throw("File $(path) too old $(a) > $(age)")
    return
end

# params_hash(params) = Base64.base64encode(hash(params))
safe_hash(params) = replace(Base64.base64encode(stable_hash(params)), '+' => '-', '/' => '_', '=' => "")
#endregion

end