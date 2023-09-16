module CmdPos
import Dates:Date
using BaseTypes
import SH:SH,v,isStatus,getLeg,getLegs
import StatusTypes:WithFilled
import LegQuoteTypes:LegQuote
using LegTypes
import ProbTypes:Prob
import Rets:Ret
import CollUtil:concat
import CalcUtil
using CollUtil
import Calendars
import Markets:market
import Expirations:expir
using ProbKde
import Kelly
import Between
import StoreTrade:ST

ERROR: not in use

export cret, cmet, ckel

cret(lms::Coll{LegQuote}, curp::Currency=market().curp)::Ret = SH.combineTo(Ret, lms, curp)
cret(lms::Coll{LegQuote}, add::Coll{LegQuote}, curp::Currency=market().curp)::Ret = cret(concat(lms, add), curp)
cmet(prob::Prob, ret::Ret) = CalcUtil.calcMetrics(prob, ret)
# ckel(prob::Prob, ret::Ret) = Kelly.calc(prob.vals, ret.vals ./ (-minimum(ret.vals)))
# ckel!(buf, prob::Prob, ret::Ret, mn::Float64) = Kelly.ded!(buf, prob.vals, ret.vals, mn)

export xprob, xlms, xlegs, xret, xmet, xkel, xdr, x3
xprob(ex::Int) = xprob(expir(ex))
function xprob(expr::Date)
    mkt = market()
    curp = mkt.curp
    start = mkt.tsMarket
    # tex = Calendars.calcTex(start, expr)
    vix = mkt.vix
    if iszero(vix)
        error("vix was 0")
    end
    # return ProbKde.probKde(F(curp), F(vix), tex)
    prob = pk.kdeToClose(F(curp), F(vix), start, expr)
    return prob
end

# xlms(xpir::Date)::Vector{LegQuote} = SH.combineTo(Vector{LegQuote}, ST.tradesOpen(x -> isStatus(x, WithFilled) && getTargetDate(x) == xpir))
xlms(xpir::Date)::Vector{LegQuote} = Between.tradesToLMOs(ST.tradesOpen(x -> isStatus(x, WithFilled) && getTargetDate(x) == xpir))
xlms(xpr::Int)::Vector{LegQuote} = xlms(expir(xpr))
xlms(xpr::Int, add::Coll{LegQuote})::Vector{LegQuote} = concat(xlms(xpr), add)
xlegs(xpir::Date)::Vector{Leg} = collect(mapflatmap(getLeg, getLegs, ST.tradesOpen(x -> isStatus(x, WithFilled) && getTargetDate(x) == xpir)))
xlegs(xpr::Int)::Vector{Leg} = xlegs(expir(xpr))

xret(ex::Int, curp::Currency=market().curp)::Ret = cret(xlms(ex), curp)
xret(ex::Int, add::Coll{LegQuote}, curp::Currency=market().curp)::Ret = cret(xlms(ex, add), curp)

xmet(ex::Int, curp::Currency=market().curp)::NamedTuple = cmet(xprob(ex), xret(ex, curp))
function xmet(ex::Int, add::Coll{LegQuote}, curp::Currency=market().curp)
    prob = xprob(ex)
    return (merge((;type=z[1]), z[2]) for z in zip((:from, :add, :both),
        (xmet(ex, curp), cmet(prob, cret(add, curp)), cmet(prob, xret(ex, add, curp)))))
end

xkel(ex::Int, curp::Currency=market().curp)::Float64 = ckel(xprob(ex), xret(ex, curp))
xkel(ex::Int, add::Coll{LegQuote}, curp::Currency=market().curp)::Float64 = ckel(xprob(ex), xret(ex, add, curp))

function x3(ex::Int)
    lms = xlms(ex)
    ret = cret(lms)
    met = cmet(xprob(ex), ret)
    return (;lms, ret, met)
end
function x3(ex::Int, add::Coll{LegQuote})
    lms = xlms(ex, add)
    ret = cret(lms)
    met = cmet(xprob(ex), ret)
    return (;lms, ret, met)
end
# TODO: If type for metrics, then could follow the `to` pattern

import DrawStrat
using TradeTypes
function xdr(ex::Int, add::Union{Nothing,Coll{LegQuote}}=nothing, curp::Currency=market().curp)
    xpir = expir(ex)
    tod = ST.tradesOpen(x -> isStatus(x, WithFilled) && getTargetDate(x) == xpir) # ST.tradesOpen(x -> x, expr)
    if isempty(tod)
        if isnothing(add)
            println("No positions nor adds for ", xpir)
        else
            DrawStrat.drawRet(SH.combineTo(Ret, add, curp); probs=(xprob(ex),), curp, label="add")
        end
    else
        trade = tod[1]
        DrawStrat.drawRet(SH.to(Ret, trade, curp); probs=(xprob(ex),), curp, label="t$(SH.getId(trade))")
        for i in eachindex(tod)[2:end]
            trade = tod[i]
            DrawStrat.drawRet!(SH.to(Ret, trade, curp); label="t$(SH.getId(trade))")
        end
        isnothing(add) || DrawStrat.drawRet!(SH.combineTo(Ret, add, curp); label="add")
        polms = SH.combineTo(Vector{LegQuote}, tod)
        lmsAll::Vector{LegQuote} = isnothing(add) ? polms : vcat(polms, collect(add))
        DrawStrat.drawRet!(SH.combineTo(Ret, lmsAll, curp); label="all")
    end
end

end