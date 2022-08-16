module Joe
using Dates
using SH, BaseTypes, Bins, LegMetaTypes, RetTypes, StratTypes
using OptionUtil, CalcUtil, ThreadUtil
import GenCands as Cands
using Calendars, Expirations, Chains, ProbKde, Markets
using CmdUtil
import GenCands:lms

export jorn

const lock = ReentrantLock()
const MaxLossExpr = Ref{Float64}(-5.0)

function calcProb(lms::Coll{LegMeta})
    start = market().tsMarket
    expr = minimum(getExpiration.(lms))
    tex = calcTex(start, expr)
    curp = market().curp
    vix = loadVix()
    return probKde(curp, tex, vix)
end
function calcRet(lms::Coll{LegMeta})
    curp = market().curp
    return combineTo(Ret, lms, curp)
end
function calcMet(lms::Coll{LegMeta})
    ret = calcRet(lms)
    calcMetrics(calcProb(lms), ret)
end

function run()
    jorn(expir(1))
end

function jorn(expr::Date)
    global ctx = ctxFor(expr)
    global oqss = filtOqss(Chains.getOqss(expr, ctx.curp, xlms(expr))) do oq
        abs(getStrike(oq) / ctx.curp - 1.0) < 0.1
    end
    global res = Vector{NamedTuple}()
    # Cands.iterSingle(oqss, ctx, res) do lms, c, r
    #     jr = joe(c, lms)
    #     if jr.rate > 0.0
    #         push!(r, jr)
    #     end
    # end

    cnt = 0

    # Cands.iterSpreads(oqss, ctx, res) do lms, c, r
    #     cnt += 1
    #     jr = joe(c, lms)
    #     if jr.rate > 0.0
    #         push!(r, jr)
    #     end
    # end

    maxSpreadWidth = C(12.0)

    ress = [Vector{NamedTuple}() for _ in 1:Threads.nthreads()]
    Cands.iterCondors(oqss, maxSpreadWidth, ctx.curp, ctx, ress) do cond, c, rs
        cnt += 1
        jr = joe(c, cond)
        if jr.rate > 0.0
            push!(rs[Threads.threadid()], jr)
            # if length(rs[Threads.threadid()]) >= 1
            #     return false
            # end
            # runSync(lock) do
            #     push!(r, jr)
            # end
        end
        if (cnt % 1000000) == 0
            println("progress $(Threads.threadid()): ", cnt)
            flush(stdout)
        end
        # if cnt > 1000000
        #     return false
        # end
        return true
    end

    res = sort!(reduce(vcat, ress); rev=true, by=x -> x.rate)
    println("proced $(cnt), results: $(length(res))")
    return res
end

using Caches, TradierData
function loadVix()
    cache!(Float64, :vixLast, Minute(10)) do
        # TODO: support snapped?
        tradierQuote("VIX")["last"]
    end
end

function ctxFor(expr::Date)::NamedTuple
    start = market().tsMarket
    curp = market().curp
    tex = calcTex(start, expr)
    timult = 1 / texToYear(tex)
    polms = xlms(expr)
    retPos = combineTo(Ret, polms, curp)
    vix = loadVix()
    threads = [(;
        retBuf1 = Bins.empty(),
        retBuf2 = Bins.empty()
    ) for _ in 1:Threads.nthreads()]
    return (;
        curp,
        prob=probKde(curp, tex, vix),
        timult,
        polms,
        retPos,
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

condRetVals(condor) = (getVals(condor[1][1][2]), getVals(condor[1][2][2]), getVals(condor[2][1][2]), getVals(condor[2][2][2]))

import Kelly
function joe(ctx, cond::Condor)
    tctx = ctx.threads[Threads.threadid()]
    rate = -Inf
    kelly = -Inf
    ret = Ret(condorRetVals!(tctx.retBuf1, condRetVals(cond)), ctx.curp, 4)
    met = calcMetrics(ctx.prob, ret)
    global condor = cond
    @assert met.mx > 0.0 "met.mx > 0.0: $(lms(cond))"
    if met.ev >= 0.0 && met.prob >= 0.75
        # retb = combineTo(Ret, vcat(lms, ctx.polms), ctx.curp)
        valsb = addRetVals!(tctx.retBuf2, tctx.retBuf1, ctx.retPos.vals)  # combineTo(Ret, vcat(ctx.polms, lms...), ctx.curp)
        # metb = calcMetrics(prob, retb)
        if minimum(valsb) > MaxLossExpr[]
            # TODO: consider using ev or evr or ? in rate calc
            rate = ctx.timult * met.ev / (-met.mn)
            kelly = Kelly.simple(met.prob, met.profit, -met.loss)
            # rate = ctx.timult * met.profit / (-met.mn)
            # rate = ctx.timult * met.mx / (-met.mn)
            # if met.ev > 0.0
            #     @info "joe" rate met
            # end
        end
    end
    return (;rate, cond, met, kelly)
end

# function joe(ctx, lms)
#     # targ = getExpiration(lms)
#     rate = 0.0

#     ret = Ret(condorRetVals!(ctx.retBuf1, condRetVals(cond)), ctx.curp, 4)
#     met = calcMetrics(ctx.prob, ret)
#     if met.ev >= 0.0 && met.prob >= 0.9
#         # retb = combineTo(Ret, vcat(lms, ctx.polms), ctx.curp)
#         retb = combineTo(Ret, vcat(ctx.polms, lms...), ctx.curp)
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