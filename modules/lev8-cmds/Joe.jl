module Joe
using Dates
using LegMetaTypes
using OptionUtil
using Calendars, Expirations, Chains, ProbKde, Markets
using CmdUtil

MaxLossExpr = -5.0

function run()
    run(expir(1))
end

function run(expr::Date)
    ctx = ctxFor(expr)
    oqss = Chains.getOqss(expr, ctx.curp, xlms(expr))
    # oqss.
    joe(ctx, lms)
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

function joe(ctx, lms::Vector{LegMeta})::Float64
    # targ = getExpiration(lms)

    ret = combineTo(Ret, lms, ctx.curp)
    met = calcMetrics(ctx.prob, ret)
    met.prob >= .9 || return 0.0

    retb = combineTo(Ret, vcat(lms, ctx.polms), ctx.curp)
    # metb = calcMetrics(prob, retb)
    minimum(getVals(retb)) > MaxLossExpr || return 0.0

    # TODO: consider using ev or evr or ? in rate calc
    rate = ctx.timult * met.ev / met.mn
    return rate
end

#endregion

end