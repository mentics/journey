module Paths

export PATHS
module PATHS

db() = Sys.iswindows() ? joinpath("D:\\", "data", "db") : "/home/jshellman/data/db"

end

end