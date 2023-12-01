module Paths
using Dates

export save_data, load_data, db_incoming

#region Public
function save_data end
function load_data end

function db(items...)
    base = Sys.iswindows() ? joinpath("D:\\", "data", "sync", "db") : "/home/jshellman/sync/db"
    return joinpath(base, items...)
end

function db_old()
    return Sys.iswindows() ? joinpath("D:\\", "data", "db") : "/home/jshellman/data/db"
end

db_incoming(dirs...; sym) = db("market", "incoming", "thetadata", sym, dirs...)
#endregion

#region Nearby
function check_file_mtime(path, age, asof)
    isfile(path) || throw("File not found $(path)")
    mt = unix2datetime(mtime(path))
    mt >= asof || throw("File $(path) out of date $(mt) < $(asof)")
    a = now(UTC) - mt
    a < age || throw("File $(path) too old $(a) > $(age)")
    return
end
#endregion

end