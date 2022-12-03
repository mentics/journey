module HistSpy
using Dates, Tables
import Sqlite as SQL
using SH, CollUtil, DictUtil
using BaseTypes, SmallTypes, OptionTypes, QuoteTypes, OptionMetaTypes, ChainTypes
using Calendars
import ChainUtil

#region Public
# TODO: UNION vs UNION ALL?
# NOTE: only call when iterating only once
# function getQuotes(ts::DateTime)::Vector{OptionQuote}
#     u = toUnix(ts)
#     return Iterators.map(toOptionQuote, sel("select 1 as style, * from Call where ts=? UNION select -1 as style, * from Put where ts=?", u, u))
# end
getQuotes(ts::DateTime, xpir::Date)::Vector{OptionQuote} =
        sort!(vcat(getCalls(ts, xpir), getPuts(ts, xpir)); by=getStrike)
getCalls(ts::DateTime, xpir::Date)::Vector{OptionQuote} =
        collect(map(toOptionQuote, sel("select 1 as style, * from Call where ts=? and expir=? order by strike", toUnix(ts), toUnix(getMarketClose(xpir)))))
getPuts(ts::DateTime, xpir::Date)::Vector{OptionQuote} =
        collect(map(toOptionQuote, sel("select -1 as style, * from Put where ts=? and expir=? order by strike", toUnix(ts), toUnix(getMarketClose(xpir)))))
getUnder(ts::DateTime)::Currency =
        reinterpret(Currency, (sel1("select under from Under where ts=?", toUnix(ts)).under))

getQuote(ts::DateTime, xpir::Date, style::Style.T, strike::Currency)::OptionQuote =
        toOptionQuote(sel1("select 1 as style, * from $(style) where ts=? and expir=? and strike=?", toUnix(ts), toUnix(getMarketClose(xpir)), Int(strike)))

getTss() = [fromUnix(first(x)) for x in sel("select distinct ts from (select distinct ts from call union all select distinct ts from put) order by ts")]
getExpirs(ts::DateTime) = [toExpir(first(x)) for x in sel("select distinct expir from (select expir from call where ts=? union all select expir from put where ts=?) order by expir", toUnix(ts), toUnix(ts))]
function getRange()
    # r = sel("select min(ts) as 'from', max(ts) as 'to' from call")[1]
    r = sel1("select (select min(ts) from call) as 'from', (select max(ts) from call) as 'to'")
    return (;from=fromUnix(r.from), to=fromUnix(r.to))
end

getOqss(ts::DateTime, xpir, curp::Currency, args...)::Oqss = ChainUtil.getOqss(getQuotes(ts, xpir), curp, args...)

getXoqss(ts::DateTime, curp::Currency, args...)::Dict{Date,Oqss} = getXoqss(ts, getExpirs(ts), curp, args...)
function getXoqss(ts::DateTime, xpirs, curp::Currency, args...)::Dict{Date,Oqss}
    dictFromKeys(xpirs) do xpir
        getOqss(ts, xpir, curp, args...)
    end
end
#endregion

#region Local
const Db = Ref{Union{Nothing,SQL.DB}}(nothing)
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
sel(sql, params::Coll) = SQL.sel(Db[], sql, params)
sel(sql, params...) = SQL.sel(Db[], sql, params...)
sel1(sql, params::Coll) = sel(sql, params)[1]
sel1(sql, params...) = sel(sql, params...)[1]
#endregion

#region Util
toUnix(ts::DateTime)::Int = round(Int, datetime2unix(ts))
fromUnix(unix::Integer)::DateTime = unix2datetime(unix)
toExpir(unix::Integer)::Date = Date(fromUnix(unix))
function toOptionQuote(row)
    OptionQuote(
        Option(Style.T(row.style), toExpir(row.expir), reinterpret(Currency, row.strike)),
        Quote(reinterpret(Currency, row.bid), reinterpret(Currency, row.ask)),
        OptionMeta(row.delta, row.theta, 0.0, row.vega, row.rho, row.gamma, row.iv, row.iv, row.iv)
    )
end
#endregion

end