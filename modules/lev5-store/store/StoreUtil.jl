module StoreUtil # TODO: rename because it's not a simple "util"
using LibPQ, Tables, Dates
using DbDef, BaseTypes
using LogUtil

export useDbTest, useDbProd, closeDb
export select, selectCol, update, inTransaction

function inTransaction(f)
    txState[] && return f()
    try
        txState[] = true
        update("begin")
        f()
        update("commit")
    catch e
        update("rollback")
        rethrow(e)
    finally
        txState[] = false
    end
end

selectCol(sql::AStr, args...) = [r[1] for r in runSql(sql, args...)]
select(sql::AStr, args...)::Vector{<:NamedTuple} = begin
    res = runSql(sql, args...)
    for r in res
        # println("row: ", r)
    end
    # @info "select" res
    rowtable(res)
end
# # selectRow(sql, args) = (res = rowtable(runSql(sql, args)) ; length(res) > 1 ? error("Unexpected rows > 1") : res[1])

useDbTest(;init=false) = (closeDb() ; connectDb(DB_TEST; init))
useDbProd(;init=false) = (closeDb() ; connectDb(DB_PROD; init))
closeDb() = isnothing(db[]) || LibPQ.close(db[])

function update(sql::AStr, args...)::Int
    res = runSql(sql, args...)
    return SubString(sql, 4) in ("upda", "inse", "dele") ? LibPQ.num_affected_rows(res) : 0
end

#region Local
const USER = "journey"
const PASSWORD = "***REMOVED***"
# const DB_USER = "mentics"
# const DB_PSW = "***REMOVED***"
# const DB_URL = "postgresql://mentics:$(DB_PSW)@free-tier4.aws-us-west-2.cockroachlabs.cloud:26257/frilly-insect-2710.defaultdb?sslmode=verify-full"
const DB_URL = "postgresql://$(USER):$(PASSWORD)@free-tier4.aws-us-west-2.cockroachlabs.cloud:26257/frilly-insect-2710.DBNAME?sslmode=verify-full"
const DB_PROD = "prod"
const DB_TEST = "test"

const db = Ref{Union{Nothing,LibPQ.Connection}}(nothing)
const txState = Ref{Bool}(false) # TODO: technically should be thread local but we're not multithreading right now
const DbName = Ref{String}("none")
const LastCall = Ref{DateTime}(DateTime(0))

dbUrl(dbName) = replace(DB_URL, "DBNAME" => dbName)

function runSql(sql::AStr, args...)
    # error("ran sql")
    connection = @timed checkConnection()
    cnt = Ref(0)
    sql2 = replace(sql, "?" =>  s -> ( cnt[] += 1 ; "\$$(cnt[])" ) )
    @log debug "runSql" sql2 args
    execution = @timed res = execute(db[], sql2, procArgs(args))
    @log sql "runSql" "connection" connection.time execution.time sql2 args
    LastCall[] = now(UTC)
    return res
end

procArgs(args) = map(procArg, args)
procArg(::Nothing) = missing
procArg(a::Union{AbstractString,Number,Date}) = a
procArg(a::Enum) = Int(a)
procArg(a::DataType) = string(a)
procArg(a::DateTime) = a # string(a)
# TODO: cleanup

tableNames() = select("show tables")

function createDb()
    @log info "initDb: Creating tables"
    foreach(DbDef.DDL()) do ddl
        try
            runSql(ddl)
        catch e
            println("Error running ", ddl)
            rethrow(e)
        end
    end
    foreach(DbDef.Inserts()) do ins
        try
            runSql(ins)
        catch e
            println("Error running ", ins)
            rethrow(e)
        end
    end
    return
end

function resetDbTest()
    connectDb(DB_TEST; init=false)
    dropAll()
    connectDb(DB_TEST; init=true)
end

function dropAll()
    DbName[] == DB_PROD && error("Don't drop all in prod")
    inTransaction() do
        tbls = select("SELECT tablename FROM pg_tables WHERE schemaname = current_schema()")
        foreach(tbls) do tbl
            update("DROP TABLE IF EXISTS $(tbl.tablename) CASCADE")
        end
    end
end

function connectDb(dbName=DB_PROD; init=false)
    DbName[] = dbName
    rawConnect()
    init && isempty(tableNames()) && createDb()
    return db[]
end

function rawConnect()
    !isnothing(db[]) && LibPQ.close(db[])
    db[] = LibPQ.Connection(dbUrl(DbName[]); options=Dict("DateStyle" => "ISO,MDY")) # DateStyle was because got error without it
    execute(db[], "set serial_normalization = sql_sequence")
end

function checkConnection()
    isnothing(db[]) && return connectDb()
    if LibPQ.status(db[]) == LibPQ.libpq_c.CONNECTION_BAD
        @log warn "runSql: db conn broken, reconnecting"
        rawConnect()
        LibPQ.status(db[]) == LibPQ.libpq_c.CONNECTION_BAD && error("Reconnecting to '$(DbName[])' failed")
    else
        if now(UTC) - Minute(6) > LastCall[] && !pingDb()
            @log warn "Db ping failed, reconnecting"
            rawConnect()
        end
    end
end

function pingDb()
    try
        return rowtable(execute(db[], "select 17 as num"))[1].num == 17
    catch e
        if e isa InterruptException
            rethrow(e)
        else
            return false
        end
    end
end
#endregion

end