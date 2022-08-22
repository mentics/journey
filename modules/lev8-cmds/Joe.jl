module Joe
using Dates
using SH, BaseTypes, Bins, LegMetaTypes, RetTypes, StratTypes
using OptionUtil, CalcUtil, ThreadUtil, OutputUtil
import GenCands as Cands
using Calendars, Expirations, Chains, ProbKde, Markets
using CmdUtil

export jorn

# rs = Vector(undef,20) ; [(i, (rs[i] = jorn(expir(i)); rs[i][1].roi)) for i in 1:1]

# const lock = ReentrantLock()
const MaxLossExpr = Ref{Float64}(-3.0)
const MaxLoss = Ref{Float64}(-2.0)

calcRate(to::Date, ret, risk) = (ret / risk) * (1 / texToYear(calcTex(now(UTC), to)))

function calcProb(lms::Coll{LegMeta})
    start = market().tsMarket
    expr = minimum(getExpiration.(lms))
    tex = calcTex(start, expr)
    curp = market().curp
    vix = loadVix()
    return probKde(curp, tex, vix)
end
# calcRet(lms::Coll{LegMeta}, curp = market().curp) = combineTo(Ret, lms, curp)
function calcMet(lms::Coll{LegMeta})
    ret = calcRet(lms)
    calcMetrics(calcProb(lms), ret)
end

# export lms
import CmdExplore
# toLms(condor::Condor) = map(x -> x[1], Iterators.flatten(condor))
toLms(condor::Condor) = (condor[1][1], condor[1][2], condor[2][1], condor[2][2])
SH.draw(condor::Condor) = CmdExplore.drlms(toLms(condor))
SH.draw!(condor::Condor) = CmdExplore.drlms!(toLms(condor))
function drawKelly(r)
    vals = calcRet(toLms(r.cond)).vals
    risk = -minimum(vals)
    draw([(ratio, Kelly.eee(ctx.prob.vals, vals / risk, ratio)) for ratio in .001:.001:.9])
end

function jorn(exs; kws...)
    # rs = Dict{Int,NamedTuple}()
    global ress = Vector{Vector{NamedTuple}}(undef,maximum(exs))
    pry = NamedTuple[]
    for i in exs
        r = runJorn(expir(i); kws...)
        ress[i] = r
        push!(pry, merge((;i), r[1]))
    end
    pretyble(pry)
    return ress
end

function runJorn(expr::Date; nopos=false, all=false)
    maxSpreadWidth = C(8.0)
    global ctx = makeCtx(expr; nopos, all)
    global oqss = filtOqss(Chains.getOqss(expr, ctx.curp, xlms(expr))) do oq
        abs(getStrike(oq) / ctx.curp - 1.0) < 0.1
    end
    ress = [Vector{NamedTuple}() for _ in 1:Threads.nthreads()]

    cnt = 0
    # Cands.iterSingle(oqss, ctx, res) do lms, c, r
    #     jr = joe(c, lms)
    #     if jr.rate > 0.0
    #         push!(r, jr)
    #     end
    # end

    Cands.paraSpreads(oqss, maxSpreadWidth, ctx, ress) do lms, c, rs
        cnt += 1
        jr = joe(c, lms)
        if jr.rate > 0.0
            push!(rs[Threads.threadid()], jr)
        end
        return true
    end
    global res = sort!(reduce(vcat, ress); rev=true, by=x -> x.roi)
    println("proced $(cnt), results: $(length(res))")
    isempty(Msgs) || @info Msgs
    return res

    empty!(Msgs)

    Cands.iterCondors(oqss, maxSpreadWidth, ctx.curp, ctx, ress) do cond, c, rs
        # cnt += 1
        jr = joe(c, cond)
        if jr.rate > 0.0
            push!(rs[Threads.threadid()], jr)
        end
        # if (cnt % 1000000) == 0
        #     println("progress $(Threads.threadid()): ", cnt)
        #     flush(stdout)
        # end
        return true
    end

    global res = sort!(reduce(vcat, ress); rev=true, by=x -> x.roi)
    println("proced $(cnt), results: $(length(res))")
    isempty(Msgs) || @info Msgs
    return res
end

using Caches, TradierData
function loadVix()
    cache!(Float64, :vixLast, Minute(10)) do
        # TODO: support snapped?
        tradierQuote("VIX")["last"]
    end
end

function makeCtx(expr::Date; nopos, all)::NamedTuple
    start = market().tsMarket
    curp = market().curp
    tex = calcTex(start, expr)
    timult = 1 / texToYear(tex)
    posLms = nopos ? LegMeta[] : xlms(expr)
    posRet = combineTo(Ret, posLms, curp)
    vix = loadVix()
    threads = [(;
        retBuf1 = Bins.empty(),
        retBuf2 = Bins.empty()
    ) for _ in 1:Threads.nthreads()]
    return (;
        all,
        curp,
        prob=probKde(curp, tex, vix),
        timult,
        posLms,
        posRet,
        posMin=minimum(posRet.vals),
        threads
    )
end
#region Local

condTo(buf::Vector{Float64}, s::Coll{LegRet})::Vector{Float64} = ( combineRetVals!(buf, tos(Ret, s)) ; return buf )

function condorRetVals!(buf, valss)
    # TODO: can remove asserts to speed up later
    # @assert rets[1].center == rets[2].center == rets[3].center == rets[4].center
    for i in eachindex(buf)
        buf[i] = valss[1][i] + valss[2][i] + valss[3][i] + valss[4][i]
    end
    return buf
end

function addRetVals!(bufTo, bufFrom, extraVals)
    for i in eachindex(bufTo)
        bufTo[i] = bufFrom[i] + extraVals[i]
    end
    return bufTo
end

# condRetVals(condor) = (getVals(condor[1][1][2]), getVals(condor[1][2][2]), getVals(condor[2][1][2]), getVals(condor[2][2][2]))

import Kelly
joe(ctx, cond::Condor) = joe(ctx, toLms(cond))
function joe(ctx, lms::Coll{LegMeta})
    tctx = ctx.threads[Threads.threadid()]
    all = ctx.all
    roi = -Inf
    roiEv = -Inf
    rate = -Inf
    rateEv = -Inf
    kelly = -Inf
    # ret = Ret(condorRetVals!(tctx.retBuf1, condRetVals(cond)), ctx.curp, 4)
    ret = combineTo(Ret, lms, ctx.curp)
    met = calcMetrics(ctx.prob, ret)
    @assert met.mx > 0.0 "met.mx > 0.0: $(lms)"
    # TODO: is ev > 0 too restrictive? and why can kelly be > 0 when ev < 0?
    if all || (met.mn > MaxLoss[] && met.prob >= 0.75 && met.ev >= 0.0)
        # TODO: needs to be adjusted like calcMetrics does
        kelly = Kelly.ded(ctx.prob.vals, ret.vals ./ (-minimum(ret.vals)))
        if kelly > 0.0
            # retb = combineTo(Ret, vcat(lms, ctx.posLms), ctx.curp)
            valsb = addRetVals!(tctx.retBuf2, tctx.retBuf1, ctx.posRet.vals)  # combineTo(Ret, vcat(ctx.posLms, lms...), ctx.curp)
            # metb = calcMetrics(prob, retb)
            minb = minimum(valsb)
            if all || (minb >= ctx.posMin || minb > MaxLossExpr[])
                # TODO: consider using ev or evr or ? in rate calc
                rateEv = ctx.timult * met.ev / (-met.mn)
                rate = ctx.timult * met.profit / (-met.mn)
                roi = rate * kelly
                roiEv = rateEv * kelly
                # rate = ctx.timult * met.mx / (-met.mn)
            else
                runSync(lockMsg) do
                    Msgs[:MaxLossExpr] = ["Hit MLE", minb, ctx.posMin, MaxLossExpr[]]
                end
            end
        end
    end
    return (;roi, roiEv, rate, rateEv, kelly, met, lms)
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