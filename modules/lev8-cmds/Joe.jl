module Joe
using Dates
using SH, BaseTypes, LegMetaTypes, RetTypes
using OptionUtil, CalcUtil, ThreadUtil
import GenCands as Cands
using Calendars, Expirations, Chains, ProbKde, Markets
using CmdUtil

const lock = ReentrantLock()
const MaxLossExpr = Ref{Float64}(-5.0)

function run()
    run(expir(2))
end

function run(expr::Date)
    global ctx = ctxFor(expr)
    global oqss = Chains.getOqss(expr, ctx.curp, xlms(expr))
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

    Cands.iterCondors(oqss, ctx, res) do lms, c, r
        cnt += 1
        jr = joe(c, lms)
        if jr.rate > 0.0
            runSync(lock) do
                push!(r, jr)
            end
        end
        if (cnt % 10000) == 0
            println("progress: ", cnt)
            flush(stdout)
        end
    end

    res = sort!(res; rev=true, by=x -> x.rate)
    println("proced $(cnt), results: $(length(res))")
    return res
end

function ctxFor(expr::Date)::NamedTuple
    start = market().tsMarket
    curp = market().curp
    tex = calcTex(start, expr)
    timult = 1 / texToYear(tex)
    # TODO: cache vix
    vix = 19.53 # tradierQuote("VIX")
    return (;
        curp,
        prob=probKde(curp, tex, vix),
        timult,
        polms=xlms(expr)
    )
end
#region Local

function joe(ctx, lms::Coll{LegMeta})
    # targ = getExpiration(lms)
    rate = 0.0

    ret = combineTo(Ret, lms, ctx.curp)
    met = calcMetrics(ctx.prob, ret)
    if met.ev >= 0.0 && met.prob >= 0.9
        # retb = combineTo(Ret, vcat(lms, ctx.polms), ctx.curp)
        retb = combineTo(Ret, vcat(ctx.polms, lms...), ctx.curp)
        # metb = calcMetrics(prob, retb)
        if minimum(getVals(retb)) > MaxLossExpr[]
            # TODO: consider using ev or evr or ? in rate calc
            # rate = ctx.timult * met.profit / (-met.mn)
            rate = ctx.timult * met.mx / (-met.mn)
            # if met.ev > 0.0
            #     @info "joe" rate met
            # end
        end
    end
    return (;rate, lms, met)
end

#endregion

end