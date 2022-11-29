module HistSpy
using Dates, Tables
import Sqlite as SQL
using SH, CollUtil
using BaseTypes, SmallTypes, OptionTypes, QuoteTypes, OptionMetaTypes, ChainTypes
using Calendars

getQuotes(ts::DateTime, xpir::Date)::Vector{OptionQuote} =
        sort!(vcat(getCalls(ts, xpir), getPuts(ts, xpir)); by=getStrike)
getCalls(ts::DateTime, xpir::Date)::Vector{OptionQuote} =
        collect(map(toOptionQuote, select("select 1 as style, * from Call where ts=? and expir=? order by strike", toUnix(ts), toUnix(getMarketClose(xpir)))))
getPuts(ts::DateTime, xpir::Date)::Vector{OptionQuote} =
        collect(map(toOptionQuote, select("select -1 as style, * from Put where ts=? and expir=? order by strike", toUnix(ts), toUnix(getMarketClose(xpir)))))

quoteTss() = [fromUnix(first(x)) for x in select("select ts from (select distinct ts from call union all select distinct ts from put) order by ts")]

#region Local
const Db = Ref{Union{Nothing,sql.DB}}(nothing)
const Path = joinpath(SQL.BaseDir, "options-spy.sqlite")
const TableName = "spy"
#endregion

#region Db
db() = @something Db[] (Db[] = SQL.open(Path))
function close!()::Nothing
    if !isnothing(Db[])
        SQL.close!(Db[])
        Db[] = nothing
    end
end
close!(stmt) = SQL.close!(stmt)
exec(sql, params::Coll) = SQL.exec(Db[], sql, params)
exec(sql, params...) = SQL.exec(Db[], sql, params...)
prep(sql) = SQL.prep(Db[], sql)
run(stmt, vals::Coll) = SQL.run(stmt, vals)
run(stmt, vals...) = SQL.run(stmt, vals...)
select(sql, params::Coll) = SQL.select(Db[], sql, params)
select(sql, params...) = SQL.select(Db[], sql, params...)
#endregion

#region Util
toUnix(ts::DateTime)::Int = round(Int, datetime2unix(ts))
fromUnix(unix::Integer)::DateTime = unix2datetime(unix)
toExpir(unix::Integer)::Date = Date(fromUnix(unix))
function toOptionQuote(row)
    OptionQuote(
        Option(Style.T(row.style), toExpir(row.expir), reinterpret(Currency, row.strike)),
        Quote(Action.open, reinterpret(Currency, row.bid), reinterpret(Currency, row.ask)),
        OptionMeta(row.delta, row.theta, 0.0, row.vega, row.rho, row.gamma, row.iv, row.iv, row.iv),
        nothing
    )
end
#endregion

end