module Sqlite
using SQLite, Tables
using BaseTypes
using DictUtil

export DB
const DB = SQLite.DB

const BaseDir = "C:/data/db/sqlite"

# const Dbs = Dict{String,SQLite.DB}()

open(path) = SQLite.DB(path)
close!(db) = DBInterface.close!(db)
close!(stmt::SQLite.Stmt) = DBInterface.close!(stmt)
exec(db, sql, params::Coll) = DBInterface.execute(db, sql, params)
exec(db, sql, params...) = DBInterface.execute(db, sql, params)
prep(db, sql) = SQLite.Stmt(db, sql)
run(stmt::SQLite.Stmt, params::Coll) = DBInterface.execute(stmt, params)
run(stmt::SQLite.Stmt, params...) = DBInterface.execute(stmt, params)
select(db, sql, params::Coll) = rowtable(exec(db, sql, params))
select(db, sql, params...) = rowtable(exec(db, sql, params))

# function reset!(path)
#     close(path)
#     db = open(path)
#     SQLite.drop!(db, "sample"; ifexists=true) ; cntStart = 0
#     close(path)
# end
end