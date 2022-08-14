module Joe
using Dates
using SH, LegMetaTypes, RetTypes
using OptionUtil, CalcUtil
import GenCands as Cands
using Calendars, Expirations, Chains, ProbKde, Markets
using CmdUtil

MaxLossExpr = -5.0

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

    Cands.iterSpreads(oqss, ctx, res) do lms, c, r
        cnt += 1
        jr = joe(c, lms)
        if jr.rate > 0.0
            push!(r, jr)
        end
    end

    println("proced $(cnt)")

    return sort!(res; rev=true, by=x -> x.rate)
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

function joe(ctx, lms::Vector{LegMeta})
    # targ = getExpiration(lms)
    rate = 0.0

    ret = combineTo(Ret, lms, ctx.curp)
    met = calcMetrics(ctx.prob, ret)
    if met.ev >= 0.0 && met.prob >= 0.9
        retb = combineTo(Ret, vcat(lms, ctx.polms), ctx.curp)
        # metb = calcMetrics(prob, retb)
        if minimum(getVals(retb)) > MaxLossExpr
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