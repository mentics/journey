module Joe
using Dates, NamedTupleTools
using SH, BaseTypes, Bins, LegMetaTypes, RetTypes, StratTypes
using OptionUtil, CalcUtil, ThreadUtil, OutputUtil
import GenCands
using Calendars, Expirations, Chains, ProbKde, Markets
using CmdUtil
using CmdPos

export jorn

# rs = Vector(undef,20) ; [(i, (rs[i] = jorn(expir(i)); rs[i][1].roi)) for i in 1:1]

import Positions, StoreOrder, LegTypes
function legsConflicting()
    legs = vcat(getLeg.(Positions.positions()), getLeg.(StoreOrder.queryLegOrders(today())))
    return legs
end

# export @u
# macro u(i)
#     # do i = 2 ; r = j.ress[i][1] ; xdr(i, r.lms) ; and disp passing in row to highlight
# end

function jorn(exs; kws...)
    # rs = Dict{Int,NamedTuple}()
    global ress = Vector{Vector{NamedTuple}}(undef,maximum(exs))
    global ctxs = Vector{NamedTuple}(undef,maximum(exs))
    # global rs1 = NamedTuple[]
    legsConflict = legsConflicting()
    # isLegAllowed(opt, side) = LegTypes.isConflict(legsConflict)
    isLegAllowed = !LegTypes.isConflict(legsConflict)

    for i in exs
        r, ctx = runJorn(expir(i), isLegAllowed; kws...)
        !isempty(r) || continue
        ress[i] = r
        ctxs[i] = ctx
        # push!(rs1, merge((;i), r[1]))
    end
    disp()
    return
end
function disp()
    res = NamedTuple[]
    for i in eachindex(ress)
        isassigned(ress, i) || continue
        r = ress[i][1]
        ctx = ctxs[i]
        push!(res, (;
            ex=i,
            delete(r, :lms, :met, :metb)...,
            evr="$(rd3(ctx.posMet.evr)):$(rd3(r.met.evr)):$(rd3(r.metb.evr))",
            prob="$(rd3(ctx.posMet.prob)):$(rd3(r.met.prob)):$(rd3(r.metb.prob))"
        ))
    end
    pretyble(res)
end

function runJorn(expr::Date, isLegAllowed; nopos=false, all=false)
    maxSpreadWidth = C(8.0)
    ctx = makeCtx(expr; nopos, all)
    oqss = filtOqss(Chains.getOqss(expr, ctx.curp, xlms(expr))) do oq
        abs(getStrike(oq) / ctx.curp - 1.0) < 0.1
    end
    ress = [Vector{NamedTuple}() for _ in 1:Threads.nthreads()]

    # GenCands.iterSingle(oqss, ctx, res) do lms, c, r
    #     jr = joe(c, lms)
    #     if jr.rate > 0.0
    #         push!(r, jr)
    #     end
    # end

    # cnt = 0
    # empty!(Msgs)
    # GenCands.paraSpreads(oqss, maxSpreadWidth, ctx, ress) do lms, c, rs
    #     cnt += 1
    #     jr = joeSpread(c, lms)
    #     if !isnothing(jr)
    #         push!(rs[Threads.threadid()], jr)
    #     end
    #     return true
    # end
    # res = sort!(reduce(vcat, ress); rev=true, by=x -> x.roi)
    # println("proced $(cnt), results: $(length(res))")
    # isempty(Msgs) || @info Msgs

    cnt = 0
    empty!(Msgs)
    GenCands.iterCondors(oqss, maxSpreadWidth, ctx.curp, isLegAllowed, ctx, ress) do cond, c, rs
        cnt += 1
        jr = joeCond(c, cond)
        if !isnothing(jr)
            push!(rs[Threads.threadid()], jr)
        end
        return true
    end
    isempty(Msgs) || @info Msgs

    # res = sort!(reduce(vcat, ress); rev=true, by=x -> x.roi)
    res = sort!(reduce(vcat, ress); rev=true, by=x -> x.roiEv)
    # res = sort!(reduce(vcat, ress); rev=true, by=x -> x.met.evr)
    println("proced $(cnt), results: $(length(res))")
    return (res, ctx)
end

using Caches, TradierData

#region Local
# const lock = ReentrantLock()
const MaxLossExpr = Ref{Float64}(-3.0)
const MaxLoss = Ref{Float64}(-2.0)

calcRate(to::Date, ret, risk) = (ret / risk) * (1 / Calendars.texToYear(calcTex(now(UTC), to)))

# export lms
# import CmdExplore
# toLms(condor::Condor) = map(x -> x[1], Iterators.flatten(condor))
toLms(condor::Condor) = (condor[1][1][1], condor[1][2][1], condor[2][1][1], condor[2][2][1])
# SH.draw(condor::Condor) = CmdExplore.drlms(toLms(condor))
# SH.draw!(condor::Condor) = CmdExplore.drlms!(toLms(condor))
function drawKelly(r)
    vals = calcRet(toLms(r.cond)).vals
    risk = -minimum(vals)
    draw([(ratio, Kelly.eee(ctx.prob.vals, vals / risk, ratio)) for ratio in .001:.001:.9])
end

# function loadVix()
#     cache!(Float64, :vixLast, Minute(10)) do
#         # TODO: support snapped?
#         tradierQuote("VIX")["last"]
#     end
# end

function makeCtx(expr::Date; nopos, all)::NamedTuple
    start = market().tsMarket
    curp = market().curp
    tex = calcTex(start, expr)
    timult = 1 / Calendars.texToYear(tex)
    # vix = market().vix
    # prob = probKde(F(curp), tex, F(vix))
    # prob, _ = pk.probKdeComp(F(curp), F(vix), start, expr)
    prob = xprob(expr)
    posLms = nopos ? LegMeta[] : xlms(expr)
    posRet = combineTo(Ret, posLms, curp)
    posMet = calcMetrics(prob, posRet)
    threads = [(;
        retBuf1 = Bins.empty(),
        retBuf2 = Bins.empty()
    ) for _ in 1:Threads.nthreads()]
    return (;
        all,
        curp,
        prob,
        timult,
        posLms,
        posRet,
        posMet,
        posMin=minimum(posRet.vals),
        threads
    )
end

import Kelly
condRets(condor) = (condor[1][1][2], condor[1][2][2], condor[2][1][2], condor[2][2][2])

function joeSpread(ctx, lms::NTuple{2,LegMeta})
    tctx = ctx.threads[Threads.threadid()]
    ret = combineTo(Ret, lms, ctx.curp)
    # TODO: use thread buffer, tctx.retBuf1
    adjust!(ret.vals, ret.numLegs) # reduce for slippage and closing short cost
    r = joe(ctx, tctx, ret)
    return isnothing(r) ? nothing : merge(r, (;lms))
end
using Rets
function joeCond(ctx, cond::Condor)
    tctx = ctx.threads[Threads.threadid()]
    combineRetVals!(tctx.retBuf1, condRets(cond))
    ret = Ret(tctx.retBuf1, ctx.curp, 4)
    adjust!(ret.vals, ret.numLegs) # reduce for slippage and closing short cost
    # ret.vals[end] > 0 || return nothing # TODO: remove?
    r = joe(ctx, tctx, ret)
    return isnothing(r) ? nothing : merge(r, (;lms=toLms(cond)))
end
function joe(ctx, tctx, ret)
    MinMx = 0.04
    all = ctx.all
    # roi = -Inf
    # roiEv = -Inf
    # rate = -Inf
    # rateEv = -Inf
    # kelly = -Inf
    met = calcMetrics(ctx.prob, ret)
    # if met.mx > -adjusted
    #     global lmsBad = lms
    #     global metBad = met
    #     return
    #     error("met.mx $(met.mx) <= $(-adjusted)")
    # end
    # TODO: is ev > 0 too restrictive? and why can kelly be > 0 when ev < 0?
    extra = ret.vals[1] > 0.0 && ret.vals[end] > 0.0
    if all || (met.mx >= MinMx && met.mn >= MaxLoss[] && met.prob >= 0.75 && met.ev >= 0.0 && extra)
        kelly = ckel(ctx.prob, ret)
        if kelly > 0.0
            Rets.addRetVals!(tctx.retBuf2, ctx.posRet.vals, ret.vals)  # combineTo(Ret, vcat(ctx.posLms, lms...), ctx.curp)
            valsb = tctx.retBuf2
            minb = minimum(valsb)
            if all || (minb >= ctx.posMin || minb > MaxLossExpr[])
                rateEv = ctx.timult * met.ev / (-met.mn)
                rate = ctx.timult * met.profit / (-met.mn)
                roi = rate * kelly
                roiEv = rateEv * kelly
                # rate = ctx.timult * met.mx / (-met.mn)
                retb = Ret(tctx.retBuf2, ctx.curp, ctx.posRet.numLegs + 4)
                metb = calcMetrics(ctx.prob, retb)
                return (;roi, roiEv, rate, rateEv, kelly, met, metb)
            else
                runSync(lockMsg) do
                    Msgs[:MaxLossExpr] = ["Hit MLE", minb, ctx.posMin, MaxLossExpr[]]
                end
            end
        end
    end
    return nothing
end

const lockMsg = ReentrantLock()
const Msgs = Dict{Symbol,Vector{Any}}()

# function joe(ctx, lms)
#     # targ = getExpiration(lms)
#     rate = 0.0

#     ret = Ret(condorRetVals!(ctx.retBuf1, condRetVals(cond)), ctx.curp, 4)
#     met = calcMetrics(ctx.prob, ret)
#     if met.ev >= 0.0 && met.prob >= 0.9
#         # retb = combineTo(Ret, vcat(lms, ctx.posLms), ctx.curp)
#         retb = combineTo(Ret, vcat(ctx.posLms, lms...), ctx.curp)
#         # metb = calcMetrics(prob, retb)
#         if minimum(getVals(retb)) > MaxLossExpr[]
#             # TODO: consider using ev or evr or ? in rate calc
#             # rate = ctx.timult * met.profit / (-met.mn)
#             rate = ctx.timult * met.mx / (-met.mn)
#             # if met.ev > 0.0
#             #     @info "joe" rate met
#             # end
#         end
#     end
#     return (;rate, lms, met)
# end

#endregion

end