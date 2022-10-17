module CmdPos
import Dates:Date
using BaseTypes
import SH
import LegMetaTypes:LegMeta
import ProbTypes:Prob
import Rets:Ret
import CollUtil:concat
import CalcUtil
import Calendars
import Markets:market
import Expirations:expir
import CmdUtil:tradesToClose
using ProbKde
import Kelly

export cret, cmet, ckel
cret(lms::Coll{LegMeta}, curp::Currency=market().curp)::Ret = SH.combineTo(Ret, lms, curp)
cret(lms::Coll{LegMeta}, add::Coll{LegMeta}, curp::Currency=market().curp)::Ret = cret(concat(lms, add), curp)
cmet(prob::Prob, ret::Ret) = CalcUtil.calcMetrics(prob, ret)
ckel(prob::Prob, ret::Ret) = Kelly.calc(prob.vals, ret.vals ./ (-minimum(ret.vals)))

export xprob, xlms, xret, xmet, xkel, xdr, x3
xprob(ex::Int) = xprob(expir(ex))
function xprob(expr::Date)
    mkt = market()
    curp = mkt.curp
    start = mkt.tsMarket
    tex = Calendars.calcTex(start, expr)
    vix = mkt.vix
    return ProbKde.probKde(F(curp), F(vix), tex)
    # prob, _ = pk.kdeToClose(F(curp), F(vix), start, expr)
    return prob
end
xlms(expr::Date)::Vector{LegMeta} = SH.combineTo(Vector{LegMeta}, tradesToClose(expr))
xlms(ex::Int)::Vector{LegMeta} = xlms(expir(ex))
xlms(ex::Int, add::Coll{LegMeta})::Vector{LegMeta} = concat(xlms(ex), add)

xret(ex::Int, curp::Currency=market().curp)::Ret = cret(xlms(ex), curp)
xret(ex::Int, add::Coll{LegMeta}, curp::Currency=market().curp)::Ret = cret(xlms(ex, add), curp)

xmet(ex::Int, curp::Currency=market().curp)::NamedTuple = cmet(xprob(ex), xret(ex, curp))
function xmet(ex::Int, add::Coll{LegMeta}, curp::Currency=market().curp)
    prob = xprob(ex)
    return (merge((;type=z[1]), z[2]) for z in zip((:from, :add, :both),
        (xmet(ex, curp), cmet(prob, cret(add, curp)), cmet(prob, xret(ex, add, curp)))))
end

xkel(ex::Int, curp::Currency=market().curp)::Float64 = ckel(xprob(ex), xret(ex, curp))
xkel(ex::Int, add::Coll{LegMeta}, curp::Currency=market().curp)::Float64 = ckel(xprob(ex), xret(ex, add, curp))

function x3(ex::Int)
    lms = xlms(ex)
    ret = cret(lms)
    met = cmet(xprob(ex), ret)
    return (;lms, ret, met)
end
function x3(ex::Int, add::Coll{LegMeta})
    lms = xlms(ex, add)
    ret = cret(lms)
    met = cmet(xprob(ex), ret)
    return (;lms, ret, met)
end
# TODO: If type for metrics, then could follow the `to` pattern

import DrawStrat

function xdr(ex::Int, add::Union{Nothing,Coll{LegMeta}}=nothing, curp::Currency=market().curp)
    expr = expir(ex)
    # TODO: read from cache
    # TODO: this is inefficient because it converts to lms multiple times
    tod = tradesToClose(expr)
    if isempty(tod)
        if isnothing(add)
            println("No positions nor adds for ", expr)
        else
            DrawStrat.drawRet(SH.combineTo(Ret, add, curp); probs=(xprob(ex),), curp, label="add")
        end
    else
        trade = tod[1]
        DrawStrat.drawRet(SH.to(Ret, trade, curp); probs=(xprob(ex),), curp, label="t$(SH.getId(trade))")
        for i in 2:length(tod)
            trade = tod[i]
            DrawStrat.drawRet!(SH.to(Ret, trade, curp); label="t$(SH.getId(trade))")
        end
        isnothing(add) || DrawStrat.drawRet!(SH.combineTo(Ret, add, curp); label="add")
        polms = SH.combineTo(Vector{LegMeta}, tod)
        lmsAll::Vector{LegMeta} = isnothing(add) ? polms : vcat(polms, collect(add))
        DrawStrat.drawRet!(SH.combineTo(Ret, lmsAll, curp); label="all")
    end
end

end