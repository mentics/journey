module HistSpy
using Dates, Tables
import SQLite
import Sqlite as SQL
using SH, CollUtil, DictUtil
using BaseTypes, SmallTypes, OptionTypes, QuoteTypes, OptionMetaTypes, ChainTypes, LegTypes
using Calendars
import ChainUtil

#region Public
# TODO: UNION vs UNION ALL?
# NOTE: only call when iterating only once
const QUOTES_SQL = """
select 1 as style, * from Call where ts=? and expir=?
UNION
select -1 as style, * from Put where ts=? and expir=?
order by strike
"""
const QUOTES_STMT = Ref{SQLite.Stmt}()
quotesStmt() = prep(QUOTES_SQL)
function itrQuotes(ts::DateTime, xpir::Date)
    u = toUnix(ts)
    x = toUnix(getMarketClose(xpir))
    # for row in run(QUOTES_STMT[], u, x, u, x)

    # end
    return Iterators.map(toOptionQuote, run(QUOTES_STMT[], u, x, u, x))
end
getQuotes(ts::DateTime, xpir::Date)::Vector{OptionQuote} =
        sort!(vcat(getCalls(ts, xpir), getPuts(ts, xpir)); by=getStrike)
getCalls(ts::DateTime, xpir::Date)::Vector{OptionQuote} =
        collect(map(toOptionQuote, sel("select 1 as style,$(ColSpec) from Call where ts=? and expir=? order by strike", toUnix(ts), toUnix(getMarketClose(xpir)))))
getPuts(ts::DateTime, xpir::Date)::Vector{OptionQuote} =
        collect(map(toOptionQuote, sel("select -1 as style,$(ColSpec) from Put where ts=? and expir=? order by strike", toUnix(ts), toUnix(getMarketClose(xpir)))))
getUnder(ts::DateTime)::Currency =
        reinterpret(Currency, (sel1("select under from Under where ts=?", toUnix(ts)).under))

getQuote(ts::DateTime, xpir::Date, style::Style.T, strike::Currency)::OptionQuote =
        toOptionQuote(sel1("select 1 as style,$(ColSpec) from $(style) where ts=? and expir=? and strike=?", toUnix(ts), toUnix(getMarketClose(xpir)), Int(strike)))

getTss() = [fromUnix(first(x)) for x in sel("select distinct ts from (select distinct ts from call union all select distinct ts from put) order by ts")]
function getExpirs(ts::DateTime)::Vector{Date}
    xpirs = [toExpir(first(x)) for x in sel("select distinct expir from (select expir from call where ts=? union all select expir from put where ts=?) order by expir", toUnix(ts), toUnix(ts))]
    # NOTE: It appears we can't get calendar from tradier API too far in future, so we'll filter out beyond 2 years in the future
    return filter!(x -> x <= today() + Year(2) + Week(2), xpirs)
end
# function getRange()
#     # r = sel("select min(ts) as 'from', max(ts) as 'to' from call")[1]
#     r = sel1("select (select min(ts) from call) as 'from', (select max(ts) from call) as 'to'")
#     return (;from=fromUnix(r.from), to=fromUnix(r.to))
# end

getOqss2(ts::DateTime, xpir::Date, curp::Currency, legsCheck)::Oqss = ChainUtil.getOqss(itrQuotes(ts, xpir), curp, legsCheck)

getXoqss(ts::DateTime, curp::Currency, legsCheck=LEGS_EMPTY)::Dict{Date,Oqss} = getXoqss(ts, getExpirs(ts), curp, legsCheck)
function getXoqss(ts::DateTime, xpirs::Vector{Date}, curp::Currency, legsCheck=LEGS_EMPTY)::Dict{Date,Oqss}
    dictFromKeys(xpirs) do xpir
        getOqss2(ts, xpir, curp, legsCheck)
    end
end
#endregion

#region Local
const Db = Ref{Union{Nothing,SQL.DB}}(nothing)
const Path = joinpath(SQL.BaseDir, "options-spy.sqlite")
const TableName = "spy"
#endregion

#region Db
db() = @something Db[] opendb()
function close!()::Nothing
    if !isnothing(Db[])
        SQL.close!(QUOTES_STMT[])
        SQL.close!(Db[])
        Db[] = nothing
    end
end
function opendb()
    Db[] = SQL.open(Path)
    QUOTES_STMT[] = quotesStmt()
end
close!(stmt) = SQL.close!(stmt)
exec(sql, params::Coll) = SQL.exec(Db[], sql, params)
exec(sql, params...) = SQL.exec(Db[], sql, params...)
prep(sql) = SQL.prep(Db[], sql)
run(stmt, vals::Coll) = SQL.run(stmt, vals)
run(stmt, vals...) = SQL.run(stmt, vals...)
sel(sql, params::Coll) = SQL.sel(Db[], sql, params)
sel(sql, params...) = SQL.sel(Db[], sql, params...)
sel1(sql, params::Coll) = sel(sql, params)[1]
sel1(sql, params...) = sel(sql, params...)[1]
#endregion

#region Util
toUnix(ts::DateTime)::Int = round(Int, datetime2unix(ts))
fromUnix(unix::Integer)::DateTime = unix2datetime(unix)
toExpir(unix::Integer)::Date = Date(fromUnix(unix))
# function toOptionQuote(row)
#     OptionQuote(
#         Option(Style.T(row.style), toExpir(row.expir), reinterpret(Currency, row.strike)),
#         Quote(reinterpret(Currency, row.bid), reinterpret(Currency, row.ask)),
#         OptionMeta(row.delta, row.theta, 0.0, row.vega, row.rho, row.gamma, row.iv, row.iv, row.iv)
#     )
# end
function toOptionQuote(row::SQLite.Row)::OptionQuote
    iv = getFloat(row, 14)
    return OptionQuote(
        Option(Style.T(getInt(row, 1)), toExpir(getInt(row, 3)), reinterpret(Currency, getInt(row, 4))),
        Quote(reinterpret(Currency, getInt(row, 5)), reinterpret(Currency, getInt(row, 6))),
        OptionMeta(getFloat(row, 9), getFloat(row, 12), 0.0, getFloat(row, 11), getFloat(row, 13), getFloat(row, 10), iv, iv, iv)
    )
end

function getInt(row::SQLite.Row, i::Int)::Union{Missing,Int}
    q = SQLite.getquery(row)
    return gvi(q, i)
end

function getFloat(row::SQLite.Row, i::Int)::Union{Missing,Float64}
    q = SQLite.getquery(row)
    return gvf(q, i)
end

function gvi(q::SQLite.Query{true}, col::Int)::Int
    handle = SQLite._get_stmt_handle(q.stmt)
    t = SQLite.C.sqlite3_column_type(handle, col - 1)
    if t == SQLite.C.SQLITE_NULL
        return missing
    else
        return SQLite.C.sqlite3_column_int64(handle, col - 1)
        # return sqlitevalueint(handle, col)
    end
end

function gvf(q::SQLite.Query{true}, col::Int)::Union{Missing,Float64}
    handle = SQLite._get_stmt_handle(q.stmt)
    t = SQLite.C.sqlite3_column_type(handle, col - 1)
    if t == SQLite.C.SQLITE_NULL
        return missing
    else
        return SQLite.C.sqlite3_column_double(handle, col - 1)
        # return sqlitevaluefloat(handle, col)
    end
end

# function sqlitevalueint(handle, col)::Int
#     # println('i', typeof(SQLite.C.sqlite3_column_int64(handle, col - 1)))
#     # convert(Int, SQLite.C.sqlite3_column_int64(handle, col - 1))
#     SQLite.C.sqlite3_column_int64(handle, col - 1)
# end
# function sqlitevaluefloat(handle, col)::Float64
#     # println('f', typeof(SQLite.C.sqlite3_column_double(handle, col - 1)))
#     # convert(Float64, SQLite.C.sqlite3_column_double(handle, col - 1))
#     SQLite.C.sqlite3_column_double(handle, col - 1)
# end


const ColSpec = "ts,expir,strike,bid,ask,last,vol,delta,gamma,vega,theta,rho,iv"

function check()
    # calls = getCalls(unix2datetime(1577975410), Date(unix2datetime(1662148800)))
    p = prep("select 1 as style, * from call limit 100000")
    rows = run(p)
    for row in rows
        q = SQLite.getquery(row)
        handle = SQLite._get_stmt_handle(q.stmt)
        col = 1
        SQLite.C.sqlite3_column_int64(handle, col - 1)
        # gvi(q, 1)
    end
    # map(toOptionQuote, rows)
    return
end
#endregion

end