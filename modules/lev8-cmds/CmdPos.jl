module CmdPos
import Dates:Date
using BaseTypes
import SH
import LegMetaTypes:LegMeta
import ProbTypes:Prob
import Rets:Ret
import CalcUtil
import Calendars
import Markets:market
import Expirations:expir
import CmdUtil:tradesToClose
import ProbKde
import Kelly

export xprob, xlms, xret, xmet, xdr
function xprob(ex::Int, curp=market().curp)
    expr = expir(ex)
    mkt = market()
    start = mkt.tsMarket
    tex = Calendars.calcTex(start, expr)
    vix = mkt.vix
    return ProbKde.probKde(F(curp), tex, F(vix))
end
xlms(expr::Date)::Vector{LegMeta} = SH.combineTo(Vector{LegMeta}, tradesToClose(expr))
xlms(ex::Int)::Vector{LegMeta} = xlms(expir(ex))
xret(ex::Int, curp::Currency=market().curp)::Ret = SH.combineTo(Ret, xlms(ex), curp)
xret(ex::Int, add::Coll{LegMeta}, curp::Currency=market().curp)::Ret = SH.combineTo(Ret, vcat(xlms(ex), collect(add)), curp)
xmet(ex::Int, curp::Currency=market().curp)::NamedTuple = ( lms = xlms(ex) ; CalcUtil.calcMetrics(xprob(lms), SH.combineTo(Ret, lms, curp)) )
xmet(ex::Int, add::Coll{LegMeta}, curp::Currency=market().curp)::NamedTuple = ( lms = vcat(xlms(ex), collect(add)) ; CalcUtil.calcMetrics(xprob(lms), SH.combineTo(Ret, lms, curp)) )
xkel(ex::Int, curp::Currency=market().curp)::Float64 = calcKelly(xprob(ex, curp), xret(ex, curp))
xkel(ex::Int, add::Coll{LegMeta}, curp::Currency=market().curp)::Float64 = calcKelly(xprob(ex, curp), xret(ex, add, curp))
# TODO: If type for metrics, then could follow the `to` pattern

# import CmdTrading # TODO: just temporary to get drt
import DrawStrat

function xdr(ex::Int, add::Union{Nothing,Coll{LegMeta}}=nothing, curp::Currency=market().curp)
    expr = expir(ex)
    # TODO: read from cache
    # TODO: this is inefficient because it converts to lms multiple times
    tod = tradesToClose(expr)
    trade = tod[1]
    DrawStrat.drawRet(SH.to(Ret, trade, curp); probs=(xprob(ex),), curp, label="t$(SH.getId(trade))")
    for i in 2:length(tod)
        trade = tod[i]
        DrawStrat.drawRet!(SH.to(Ret, trade, curp); label="t$(SH.getId(trade))")
    end
    isnothing(add) || DrawStrat.drawRet!(combineTo(Ret, lms, curp); label="add")
    lmsAll = isnothing(add) ? SH.combineTo(Vector{LegMeta}, tod) : vcat(polms, add)
    DrawStrat.drawRet!(SH.combineTo(Ret, lmsAll, curp); label="all")
end

# TODO: move?
calcKelly(prob::Prob, ret::Ret) = Kelly.calc(prob.vals, ret.vals ./ (-minimum(ret.vals)))

end