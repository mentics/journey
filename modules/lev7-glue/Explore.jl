module Explore
using Dates, StaticArrays, ThreadPools
import Makie
using BaseTypes, LegTypes
using DateUtil
import Keepers:Keeper
import SH, Calendars
import CollUtil:sortuple
import Positions
import VectorCalcUtil
import DataFiles as dat
using DataFrames, StatsBase
using ProbMultiKde
import ProbMultiKde as pmk
import ThreadUtil
import LinesLeg as LL
using ProbMeta

# const CONFIG3 = Ref((;
#     adjustprices = C(-0.0),
#     kelprobadjust = 0.0
# ))

config() = (;
    adjustprices = C(-0.01),
    kelprobadjust = 0.0,
    commit_min = 0.07,
    commit_max = 4.01,
    probprofit_min = 0.72,
    kel_min = 0.1,
    evrate_min = 4.01,
    all_risk_max = 10000.0,
    max_spread = 4.1,
    max_gap = 4.1,
    maxcondormiddle = 4.1,
    annual_min = 4.01,
)

# config() = (;
#     adjustprices = C(-0.0),
#     kelprobadjust = 0.1,
#     commit_min = 0.07,
#     commit_max = 16.01,
#     probprofit_min = 0.94,
#     kel_min = 0.1,
#     evrate_min = 4.01,
#     all_risk_max = 10000.0,
#     max_spread = 8.1,
#     max_gap = 16.1,
#     maxcondormiddle = 16.1,
#     annual_min = 8.01,
# )

#region LookForBestKelly
using SmallTypes, LegQuoteTypes
using Markets, Expirations, Chains
import Kelly, ChainUtil, Between

function prob_test(xpir=expir(1), kde=pmk.get_kde(now(UTC)))
    prob = pmk.makeprob(kde, market().curp, now(UTC), dat.market_close(xpir), Chains.chain(xpir).chain)
    return kde, prob
end

struct Result{T}
    r::T
end

# Base.isless(a::Result{T}, b::Result{T}) where T = a.r.evrate < b.r.evrate
# Base.isless(a::Result, b::Result) = a.r.evrate < b.r.evrate
function Base.isless(a::Result, b::Result)
    av = to_val_kel(a)
    bv = to_val_kel(b)
    return av < bv
    # isnothing(a.r.all) && return a.r.evrate < b.r.evrate
    # if isnan(a.r.all.evrate)
    #     if isnan(b.r.all.evrate)
    #         println("ERROR: results both nans")
    #         @show a b
    #         return a.r.all.evrate < b.r.all.evrate
    #     else
    #         return true # nan is always less
    #     end
    # elseif isnan(b.r.all.evrate)
    #     return false # already know a is not nan and nan is always less
    # else
    #     return
    # end
end
function to_val_evrate(x)
    x1 = isnothing(x.r.forpos) ? x.r.evrate : x.r.forpos.evrate
    return isnan(x1) ? -Inf : x1
end
function to_val_prob(x)
    p = x.r.probprofit
    return isfinite(p) ? p : -Inf
end
function to_val_kel(x)
    p = x.r.kel
    return isfinite(p) ? p : -Inf
end

Base.vec(keeper::Keeper{Result}) = map(x -> x.r, keeper.store)

run(inc::Integer; kws...) = run([inc]; kws...)
function run(incs=1:3; skipto=nothing, kws...)
    # global kcalckel = nothing
    # global kmakeleg = nothing
    # global krun = nothing
    # global kcheck = nothing
    # global kctx = nothing

    # kde = use_kde()
    # xpirs = Expirations.xpirsinrange(1, 4)
    xpirs = Expirations.expirs()[1:2]
    # incs = collect(incsin)
    if isnothing(skipto)
        res = findkel(make_ctx_now(), xpirs, incs; kws...) do xpirts, curp
            return ChainUtil.oqssEntry(chain(xpirts).chain, curp; legsCheck=Positions.positions(), minshort=0.05, minlong=0.05)
        end
        # global krun = res
    else
        res = skipto
    end

    if isempty(res)
        println("No results")
        return
    end

    r1 = res[1]
    lqs = r1.lqs
    target = Pricing.price(Action.open, lqs)
    bdaysout = bdays(today(),SH.getExpir(lqs))
    println("evrate:$(r1.evrate) probprofit:$(r1.probprofit) min:$(r1.neto) target:$(target) risk:$(r1.risk) daysout:$(bdaysout)");
    drawres(lqs)

    # DrawUtil.newfig()
    # evrate_max, i_max = findmax(eachindex(res)) do i
    #     num = incs[i]
    #     rs = res[i]
    #     !isempty(rs) || return 0.0
    #     r1 = rs[1]
    #     lms = r1.lms
    #     target = Pricing.price(Action.open, lms)
    #     bdaysout = DateUtil.bdays(today(),SH.getExpir(lms))
    #     println("$(num): evrate:$(r1.evrate) probprofit:$(r1.probprofit) min:$(r1.neto) target:$(target) risk:$(r1.risk) daysout:$(bdaysout)");
    #     DrawUtil.draw!(:lines, SH.toDraw(lms); label="$(i)-pnl")
    #     DrawUtil.drawprob!(ProbLup[SH.getExpir(lms)]; label="$(i)-prob")
    #     return r1.evrate
    # end

    # if evrate_max > 0.0
    #     r = res[i_max]
    #     r1 = r[1]
    #     lms = r1.lms
    #     println("#$(incs[i_max]) has best")
    #     DrawUtil.updateLegend()
    #     return res, r1, lms
    # else
    #     println("No results")
    #     return
    # end
    return r1
end

function drawres(lms)
    prob = ProbLup4[SH.getExpir(lms)]
    numlegs = length(lms)
    display(DrawUtil.newfig())
    DrawUtil.draw!(:lines, SH.toDraw(lms); label="$(numlegs)-pnl")
    DrawUtil.drawprob!(prob; label="$(numlegs)-prob")
    DrawUtil.updateLegend()
end

import Trading
function opentrade(r; minextra=P(0.01))
    # TODO: warn if curp has changed much
    println("Opening trade: neto:$(r.neto)")
    Trading.open_trade(market(), r.lqs, r.neto; pre=false, minextra);
end

# function make_ctx_now(; keep=2)
#     mkt = market()
#     ts = mkt.tsMarket
#     curp = mkt.curp
#     kde = pmk.get_kde(ts)
#     # ress = [Keeper{Result}(keep) for _ in 1:Threads.nthreads()]
#     # kelly_bufs = [Kelly.make_buf() for _ in 1:Threads.nthreads()]
#     # (;ts, date=Date(ts), curp, kde, keep, ress, kelly_bufs)
#     return make_ctx(ts, kde, curp; keep)
# end
function make_ctx(ts, prob_for_xpirts, curp; keep=2)
    ress = [Keeper{Result}(keep) for _ in 1:Threads.nthreads()]
    kelly_bufs = [Kelly.make_buf() for _ in 1:Threads.nthreads()]
    return (;ts, date=Date(ts), prob_for_xpirts, curp, keep, ress, kelly_bufs)
end

function reset_ctx(ctx)
    empty!.(ctx.ress)
    @assert isempty(ctx.ress[1])
    @assert isempty(ctx.ress[2])
    @assert isempty(ctx.ress[4])
    @assert isempty(ctx.ress[end])
end

function findkel(ctx, xpirts::DateTime, oqss, pos_rs, incs)
    reset_ctx(ctx)
    ress = ctx.ress

    # prob = pmk.makeprob(ctx.kde, ctx.curp, ctx.ts, xpirts, oqs)
    prob = nothing
    try
        prob = ctx.prob_for_xpirts(xpirts)
    catch e
        println("ERROR: exception thrown getting prob for $((;ctx.ts, xpirts)). Skipping.\n  $(e)")
        return []
    end
    riskrat = calcriskrat(ctx.ts, xpirts)

    pos = nothing
    if !isempty(pos_rs)
        lqs = get_pos_lqs(pos_rs)
        segs = LL.toSegments(lqs, P.(SH.getBid.(lqs)))
        risk = riskrat * max(Pricing.calcMarg(ctx.curp, segs))
        kel, evret, ev, probprofit = calckel(ctx.kelly_bufs[Threads.threadid()], ctx.curp, prob, risk, segs)
        pos = kv(;lqs=nc(lqs), segs=nc(segs), risk, kel, evret, ev, probprofit, count=length(pos_rs))
    end

    ctx2 = kv(;ctx.ts, ctx.date, ctx.curp, prob, riskrat, xpirts, ctx.kelly_bufs, pos)
    # 1 in incs && findkel1!(ress, ctx2, oqss) && error("stop")
    2 in incs && findkel2!(ress, ctx2, oqss) && error("stop")
    3 in incs && findkel3!(ress, ctx2, oqss) && error("stop")
    4 in incs && findkel4!(ress, ctx2, oqss) && error("stop")

    # println("Finished searching")
    res = merge(ress, ctx.keep)
    return vec(res)
end

# pos_new() = Dict()
# function pos_add(pos, r)
# end

# struct LegQuoteSized
#     lq::LegQuote
#     quantity::Float64
# end

function get_pos_lqs(pos)
    # sort!(collect(Iterators.flatten(map(r -> r.lms, pos))); by=SH.getStrike)
    v = sort!(collect(Iterators.flatten(map(r -> r.r1.lqs, pos))); by=SH.getStrike)
    # xpir = SH.getExpir(v[1])
    # @assert isnothing(findfirst(x -> SH.getExpir(x) != xpir, v))
    return v
    # len = length(v)
    # MAX_LEN[] = max(MAX_LEN[], len)
    # res = SVector{len}(v)
    # return res
end
# const MAX_LEN = Ref(0)

# findkel(f_xpir_to_oqss, ctx, xpirs::Coll{Date}, incs; keep=100) = findkel(f_xpir_to_oqss, ctx, Calendars.getMarketClose.(xpirs), incs)
# function findkel(f_xpir_to_oqss, ctx, xpirtss::Coll{DateTime}, incs; keep=100, use_problup=true)
#     use_problup && empty!(ProbLup2)
#     # println("Finding kel for curp: $(ctx.curp)")
#     ress = [Keeper{Result}(keep) for _ in 1:Threads.nthreads()]
#     kelly_buffer = [Kelly.make_buf() for _ in 1:Threads.nthreads()]
#     for xpirts in xpirtss
#         xpir = Date(xpirts)
#         # println("Searching xpir: $(xpir)")
#         oqss = f_xpir_to_oqss(xpirts, ctx.curp)
#         # println("num calls $(length(oqss.call.long))")
#         # println("num puts $(length(oqss.put.long))")
#         # TODO: messy concating these after they were just separated
#         oqs = vcat(oqss.call.long, oqss.put.long)
#         prob = pmk.makeprob(ctx.kde, ctx.curp, ctx.ts, xpirts, oqs)
#         riskrat = calcriskrat(ctx.date, xpir)
#         ctx2 = (;ctx.ts, ctx.date, ctx.curp, prob, riskrat, xpir, kelly_buffer)
#         use_problup && ( ProbLup2[xpir] = prob )
#         1 in incs && findkel1!(ress, ctx2, oqss)
#         2 in incs && findkel2!(ress, ctx2, oqss)
#         3 in incs && findkel3!(ress, ctx2, oqss)
#     end
#     # println("Finished searching")
#     res = merge(ress, keep)
#     return vec(res)
# end

calcevrate(evret, xpir::DateLike, date) = evret * DateUtil.timult(date, xpir)

# calcrate(r, date=today()) = DateUtil.calcRate(date, r.xpir, r.kel * r.ev, r.risk)
# calcret(r, date=today()) = r.kel * calcrate(r, date)

function make_leg(oq, side)
    # if SH.getBid(oq) > SH.getAsk(oq)
    #     @show oq side
    #     global kmakeleg=(;adjust=CONFIG10[].adjustprices, oq, side)
    #     error("stop")
    # end
    lm = LegQuoteOpen(oq, side; adjustprices=config().adjustprices)
    if SH.getBid(lm) == 0
        # global kmakeleg=(;adjust=config().adjustprices, oq, side, lm)
        error("bid 0")
    end
    return lm
end

function findkel1!(ress, ctx, oqss)::Bool
    stop = ThreadUtil.loop(oqss.call.long) do call
        check!(ress, ctx, (make_leg(call, Side.long),))
    end
    !stop || return stop
    stop = ThreadUtil.loop(oqss.put.long) do put
        check!(ress, ctx, (make_leg(put, Side.long),))
    end
    return stop
end

import GenCands
function findkel2!(ress, ctx, oqss)::Bool
    stop = GenCands.paraSpreads(oqss.call, config().max_spread, alltrue) do thid, lms
        check!(ress, ctx, lms)
    end
    !stop || return stop
    stop = GenCands.paraSpreads(oqss.put, config().max_spread, alltrue) do thid, lms
        check!(ress, ctx, lms)
    end
    return stop
end

function findkel3!(ress, ctx, oqss)::Bool
    # count = zeros(Int, Threads.nthreads())
    stop = GenCands.paraSpreads(oqss.call, config().max_spread, alltrue) do thid, lms
        strike1 = SH.getStrike(lms[1])
        strike2 = SH.getStrike(lms[end])
        for oq in oqss.call.long
            strike = SH.getStrike(oq)
            if (strike < strike1 && strike1 - strike <= config().max_gap) ||
                    (strike > strike2 && strike - strike2 <= config().max_gap)
                lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
                check!(ress, ctx, lms2)
            end
        end
        yield()
        # count[thid] += length(oqss.call.long)
        for oq in oqss.put.long
            strike = SH.getStrike(oq)
            if (strike < strike1 && strike1 - strike <= config().max_gap) ||
                    (strike > strike2 && strike - strike2 <= config().max_gap)
                lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
                check!(ress, ctx, lms2)
            end
        end
        # count[thid] += length(oqss.put.long)
        yield()
        return true
    end
    !stop || return stop
    stop = GenCands.paraSpreads(oqss.put, config().max_spread, alltrue) do thid, lms
        strike1 = SH.getStrike(lms[1])
        strike2 = SH.getStrike(lms[end])
        for oq in oqss.call.long
            strike = SH.getStrike(oq)
            if (strike < strike1 && strike1 - strike <= config().max_gap) ||
                    (strike > strike2 && strike - strike2 <= config().max_gap)
                lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
                check!(ress, ctx, lms2)
            end
        end
        # count[thid] += length(oqss.call.long)
        yield()
        for oq in oqss.put.long
            strike = SH.getStrike(oq)
            if (strike < strike1 && strike1 - strike <= config().max_gap) ||
                    (strike > strike2 && strike - strike2 <= config().max_gap)
                lms2 = sortuple(SH.getStrike, lms..., make_leg(oq, Side.long))
                check!(ress, ctx, lms2)
            end
        end
        # count[thid] += length(oqss.put.long)
        yield()
        return true
    end
    # println("Searched $(sum(count)) permutations")
    return stop
end

function findkel4!(ress, ctx, oqss)::Bool
    kel4!(ress, ctx, Side.long, oqss.call.long, oqss.call.short, oqss.call.short, oqss.call.long)
    kel4!(ress, ctx, Side.short, oqss.call.short, oqss.call.long, oqss.call.long, oqss.call.short)

    kel4!(ress, ctx, Side.long, oqss.put.long, oqss.put.short, oqss.put.short, oqss.put.long)
    kel4!(ress, ctx, Side.short, oqss.put.short, oqss.put.long, oqss.put.long, oqss.put.short)

    kel4!(ress, ctx, Side.long, oqss.call.long, oqss.call.short, oqss.put.short, oqss.put.long)
    kel4!(ress, ctx, Side.short, oqss.call.short, oqss.call.long, oqss.put.long, oqss.put.short)

    kel4!(ress, ctx, Side.short, oqss.put.long, oqss.put.short, oqss.call.short, oqss.call.long)
    kel4!(ress, ctx, Side.long, oqss.put.short, oqss.put.long, oqss.call.long, oqss.call.short)

    return false
end

function kel4!(ress, ctx, side1, oqsL1, oqsL2, oqsR1, oqsR2)
    side2 = SH.toOther(side1)
    maxspread = config().max_spread
    maxcondormiddle = config().maxcondormiddle
    for left1 in 1:(lastindex(oqsL1)-1)
        oqL1 = oqsL1[left1]
        strikeL1 = SH.getStrike(oqL1)
        for left2 in eachindex(oqsL2)
            oqL2 = oqsL2[left2]
            strikeL2 = SH.getStrike(oqL2)
            strikeL2 > strikeL1 || continue
            (strikeL2 - strikeL1) <= maxspread || break
            for right1 in left2:lastindex(oqsR1)
                oqR1 = oqsR1[right1]
                strikeR1 = SH.getStrike(oqR1)
                strikeR1 >= strikeL2 || continue
                (strikeR1 - strikeL2) <= maxcondormiddle || break
                for right2 in (left1+1):lastindex(oqsR2)
                    oqR2 = oqsR2[right2]
                    strikeR2 = SH.getStrike(oqR2)
                    strikeR2 > strikeR1 || continue
                    (strikeR2 - strikeR1) <= maxspread || break
                    lqs = (make_leg(oqL1, side1), make_leg(oqL2, side2), make_leg(oqR1, side2), make_leg(oqR2, side1))
                    check!(ress, ctx, lqs)
                    yield()
                end
            end
        end
    end
end

# function testcheck(res, ctx, lms2)
#     for i in 1:10000
#         Explore.check!(res, ctx, lms2)
#     end
# end

function check!(ress, ctx, lqs)::Nothing
    cfg = config()
    netos = P.(SH.getBid.(lqs))
    segs = LL.toSegments(lqs, netos)
    LL.canprofit(segs) || return
    commit = max(Pricing.calcMarg(ctx.curp, segs))
    cfg.commit_min < commit < cfg.commit_max || return
    # commit *= ctx.riskrat

    thid = Threads.threadid()
    kel, evret, ev, probprofit = calckel(ctx.kelly_bufs[thid], ctx.curp, ctx.prob, commit, segs)
    (isfinite(kel) && kel > cfg.kel_min) || return
    probprofit >= cfg.probprofit_min || return
    # evrate = calcevrate(evret, ctx.xpirts, ctx.date)
    timult = DateUtil.timult(ctx.date, ctx.xpirts)
    evrate = evret * timult
    evrate > cfg.evrate_min || return

    try
        pos = ctx.pos
        pos_count = 0
        forpos = nothing
        if !isnothing(pos)
            pos_count = pos.count
            all_lqs = sort!(vcat(pos.lqs, lqs...); by=SH.getStrike)
            all_segs = LL.toSegments(all_lqs, P.(SH.getBid.(all_lqs)))
            marg = Pricing.calcMarg(ctx.curp, all_segs)
            all_risk = ctx.riskrat * max(marg)
            if all_risk > cfg.all_risk_max
                # global kriskhigh = (;marg, all_risk, lqs, pos_lqs=pos.lqs, all_lqs, all_segs)
                # @show all_risk
                return
            end
            all_kel, all_evret, all_ev, all_probprofit = calckel(ctx.kelly_bufs[thid], ctx.curp, ctx.prob, all_risk, all_segs)
            if !isnan(all_evret) && (all_evret <= pos.evret) # || all_probprofit <= pos.probprofit)
                return
            end
            all_evrate = calcevrate(evret, ctx.xpirts, ctx.date)
            forpos = kv(;evrate=all_evrate, kel=all_kel, evret=all_evret, ev=all_ev, probprofit=all_probprofit, risk=all_risk)
        end

        # probprofit = Between.calcprobprofit(ctx.prob, segs)
        # cnt = 1 + (pos_count + numtradestoxpir(ctx.ts, ctx.xpirts))
        # balrat = 1 / cnt
        balrat = 1 / (19 * 3) # 19 / day * 3 days
        # TODO: filter out ones that have a lot of outcome too close to zero
        annual = calc_iter_ret(ctx.curp, ctx.prob, segs, commit, timult)
        if annual < config().annual_min
            # println("WARN: annual return $(annual) too low, evrate:$(evrate)")
            return
        end
        push!(ress[thid], Result(kv(;forpos, evrate, balrat, kel, ev, evret, probprofit, commit, annual, ctx.xpirts, lqs=nc(lqs), netos=netos=nc(netos), neto=sum(netos))))
    catch e
        global kcheck = kv(;ctx, lqs=nc(lqs), segs, netos)
        rethrow(e)
    end
    return
end

import Lines
function calc_iter_ret(curp, prob, lqs; balrat = 0.5)
    xpir = SH.getExpir(lqs)
    netos = P.(SH.getBid.(lqs))
    segs = LL.toSegments(lqs, netos)
    timult = DateUtil.timult(prob.ts, xpir)
    commit = max(Pricing.calcMarg(prob.center, segs))
    return calc_iter_ret(curp, prob, segs, commit, timult; balrat)
end
function calc_iter_ret(curp, prob, segs, commit, timult; balrat = 0.5)
    len = Bins.VNUM # length(prob.xs.xs)
    s = 1.0
    for i in 1:len
        x = Bins.x(i) # prob.xs.xs[i]
        y = Lines.atsegs(segs, x * curp)
        o = y / commit
        p = prob[i] # prob.prob_mass[i]
        @assert -commit <= y @str commit x y o p balrat
        # p *= o >= 0.0 ? 1.0 - probadjust : 1.0 + probadjust
        # o > 0 && (probprofit += p)
        #

        change = 1.0 + balrat * o
        if change < 0
            @show change x y o p balrat
        end
        news = change^(p*timult)
        # if o < 0
        #     @show s news change x y o p
        # end
        s *= news
    end
    return s
end

function checkold!(ress, ctx, lms)::Nothing
    # neto = CZ
    # try
    #     neto = Pricing.price(Action.open, lms)
    # catch e
    #     global kpricingerr = (;ctx, lms)
    #     ThreadUtil.show_exc(e, "error pricing")
    #     rethrow(e)
    # end
    # if neto < 0.07
    #     return true
    # end
    # segs, netos = Between.toSegmentsN(lms)
    netos = P.(SH.getBid.(lms))
    segs = LL.toSegments(lms, netos)
    try
        LL.canprofit(segs) || return
        risk = ctx.riskrat * max(Pricing.calcMarg(ctx.prob.center, segs))
        0.14 < risk < 6.0 || return
        thid = Threads.threadid()
        kel, evret, ev, probprofit = calckel(ctx.kelly_buffer[thid], ctx.prob, risk, segs)
        isfinite(kel) || return
        probprofit >= 0.85 || return

        pos = ctx.pos
        pos_count = 0
        all = nothing
        if !isnothing(pos)
            pos_count = pos.count
            # all_lms = sort!(vcat(collect(lms), pos.lms); by=SH.getStrike)
            # all_segs = LL.toSegments(all_lms, P.(SH.getBid.(all_lms)))
            # all_risk = ctx.riskrat * max(Pricing.calcMarg(ctx.prob.center, all_segs))
            # all_kel, all_evret, all_ev = calckel(ctx.kelly_buffer[thid], ctx.prob, all_risk, all_segs)

            # all_lms = sort(vcat(SVector(lms), pos.lms); by=SH.getStrike)
            all_lms = sort!(vcat(pos.lms, lms...); by=SH.getStrike)
            all_segs = LL.toSegments(all_lms, P.(SH.getBid.(all_lms)))
            all_risk = ctx.riskrat * max(Pricing.calcMarg(ctx.prob.center, all_segs))
            all_kel, all_evret, all_ev, all_probprofit = calckel(ctx.kelly_buffer[thid], ctx.prob, all_risk, all_segs)
            if !isnan(all_evret) && (all_evret <= pos.evret) # || all_probprofit <= pos.probprofit)
                return
            end

            all_evrate = calcevrate(evret, ctx.xpirts, ctx.date)
            # ThreadUtil.sync_output(@str (pos.kel, pos.evret) (kel, evret) (all_kel, all_evret))
            all = kv(;evrate=all_evrate, kel=all_kel, evret=all_evret, ev=all_ev, probprofit=all_probprofit)
            # if length(lms) == 1 && (all_kel > pos.kel || all_evret > pos.evret)
            #     ThreadUtil.sync_output(@str all_kel pos.kel all_evret pos.evret)
            #     error("Found improved by one leg")
            # end
        end

        # if isfinite(kel) # && kel >= 0.48
            evrate = calcevrate(evret, ctx.xpirts, ctx.date)
            # probprofit = Between.calcprobprofit(ctx.prob, segs)
            cnt = 1 + (pos_count + numtradestoxpir(ctx.ts, ctx.xpirts))
            # if cnt < 1
            #     global kctx = ctx
            #     error("invalid count")
            # end
            balrat = 1 / cnt
            # TODO: filter out ones that have a lot of outcome too close to zero
            push!(ress[thid], Result(kv(;all, evrate, balrat, kel, ev, evret, probprofit, risk, ctx.xpirts, lms=nc(lms), netos=netos=nc(netos), neto=sum(netos))))
        # end
    catch e
        # global kcheck = kv(;ctx, lms=nc(lms), segs, netos)
        rethrow(e)
    end
    return
end
#endregion LookForBestKelly

#region Kelly
riskfreerate() = 0.04
calcriskrat(from::DateLike, to::DateLike) = 1 + riskfreerate() * DateUtil.riskyears(from, to)

import Kelly, Pricing, LinesLeg as LL
# function calcKel1(ts::DateTime, curp::Real, lms)
#     xpir = getExpir(lms)
#     Kelly.calcKel(makeprob(ts, xpir, curp), calcriskrat(Date(ts), xpir), lms)
# end

# calckel(lms) = calckel(ProbLup[SH.getExpir(lms)], lms)
# calckel(prob, lms) = calckel(prob, calcriskrat(today(), SH.getExpir(lms)), lms)

function calckel(buf, curp, prob, risk::Real, segs)
    probadjust = config().kelprobadjust
    try
        return Kelly.calckel(buf, curp, prob, risk, segs, probadjust)
    catch e
        # global kcalckel = kv(;prob, risk, segs=nc(segs), probadjust)
        rethrow(e)
    end
end

function numtradestoxpir(ts::DateTime, xpir::DateLike)
    xpir = Date(xpir)
    date = Date(ts)
    @assert xpir > date
    num_day1 = floor(Int, (dat.market_close(ts) - ts) / Minute(15))
    bdays = DateUtil.bdays(date, xpir) - 1 # exclude the xpir day itself
    return num_day1 + bdays * 26 # each 15 minutes 6:30 - 1:00
end
#endregion Kelly

#region kde
# import Distributions, KernelDensityEstimate, DrawUtil
# function kdedata()
#     nspy = Distributions.Normal(1.0, 0.1)
#     numsamples = 10000
#     maxdur = 200
#     basevix = 10
#     vixspread = 2
#     return [(dur = 1 + i % maxdur ; spy = log(rand(nspy)) ; [spy*dur, basevix + vixspread*spy + rand() - 0.5, dur]) for i in 0:numsamples]
# end
# function testkde()
#     bws = [0.1, 0.1, 1.01]
#     data = stack(kdedata())
#     kdem1 = KernelDensityEstimate.kde!(data, bws)
#     range1 = -0.49:0.01:0.49
#     vals = map(x -> x[1], [kdem1(reshape([x, basevix, maxdur / 2], 3, 1)) for x in range1])
#     display(DrawUtil.draw(:scatter, range1, vals))
#     for k in 0.5:0.1:1.5
#         sleep(0.5)
#         vals = map(x -> x[1], [kdem1(reshape([x, basevix, k * maxdur / 2], 3, 1)) for x in range1])
#         DrawUtil.draw!(:scatter, range1, vals)
#     end
# end

# using MultiKDE, Distributions, Random
# function multikde()
#     data = kdedata()
#     dims = [ContinuousDim(), ContinuousDim(), ContinuousDim()]
#     bws = [0.1, 0.1, 1.01]
#     kde = KDEMulti(dims, bws, data)

#     xs = -0.49:0.01:0.49
#     display(DrawUtil.draw(:scatter, [0.0], [0.0]))
#     for dur in 1.0:10.0:200.0
#         vals = [MultiKDE.pdf(kde, [x, 10.0, dur]) for x in xs]
#         DrawUtil.draw!(:scatter, xs, vals)
#         println("done: $(dur)")
#         yield()
#     end
# end

# getxmin(prob::KdeProb2) = prob.src.extents[1].mn
# getxmax(prob::KdeProb2) = prob.src.extents[1].mx
#endregion kde

import DrawUtil
DrawUtil.drawprob(prob::KdeProb2; kws...) = DrawUtil.drawdist!(DrawUtil.getAxis(;newFig=true), prob.center, prob.xs.xs, probdispvals(prob); kws...)
DrawUtil.drawprob!(prob::KdeProb2; kws...) = DrawUtil.drawdist!(DrawUtil.getAxis(;newFig=false), prob.center, prob.xs.xs, probdispvals(prob); kws...)
probdispvals(prob::KdeProb2) = ( k = 100 * 0.01 / xswidth(prob) ; prob.prob_mass .* k )
xswidth(prob::KdeProb2) = prob.xs.xs[2] - prob.xs.xs[1]

# function data_from_tsx(tsx, len)
#     len_tot = length(tsx.ts)
#     inds = len > 0 ? (1:len) : ((len_tot+len+1):len_tot)
#     return [[tsx.ret[i], tsx.logvol[i], tsx.tex[i]] for i in inds]
# end

# function kde_from_tsx(tsx, len)
#     return TheKde(make_kde(data_from_tsx(tsx, len)), extrema(first, data)...)
# end

# function make_kde(data)
#     dims = [ContinuousDim(), ContinuousDim(), ContinuousDim()]
#     bws = [0.005, 0.1, 20.0]

#     # TODO: consider normalizing the inputs, then might use same bandwidths
#     # the nature of the variables are different, so need to adjust

#     # bws = nothing

#     # kde = KDEMulti(dims, bws, data)
#     # bws = MultiKDE.default_bandwidth(data) ./ 4
#     return KDEMulti(dims, bws, data)
# end

# function plotkde(; len=-100000, gridcount=10)
#     tsx = ktsx
#     kde = kde_from_tsx(tsx, len).kde

#     len_tot = length(tsx.ts)
#     inds = len > 0 ? (1:len) : ((len_tot+len):len_tot)
#     # data = [[tsx.ret[i], tsx.logvol[i], tsx.tex[i]] for i in inds]
#     rets, logvols, texs = grid3((extrema(tsx.ret[inds]), extrema(tsx.logvol[inds]), extrema(tsx.tex[inds])); num=gridcount)

#     numcoords = length(rets) * length(logvols) * length(texs)
#     xs = Vector{Float64}(undef, numcoords)
#     ys = Vector{Float64}(undef, numcoords)
#     zs = Vector{Float64}(undef, numcoords)
#     ps = Vector{Float64}(undef, numcoords)

#     i = 0
#     coord = [0., 0., 0.]
#     for x in rets, y in logvols, z in texs
#         i += 1
#         coord .= [x, y, z]
#         xs[i] = x; ys[i] = y; zs[i] = z
#         ps[i] = MultiKDE.pdf(kde, coord)
#     end
#     VectorCalcUtil.normalize!(ps)

#     display(Makie.scatter(xs, ys, zs; color=ps, markersize=5, colormap=:viridis, axis=(type=Makie.Axis3,)))
# end

# function over(extent; num=10)
#     left = first(extent)
#     right = last(extent)
#     step = (right - left) / (num - 1)
#     return [left + i * step for i in 0:num-1]
# end
# grid3(extents; num=10) = map(x -> over(x; num), extents)

# function calckde(; len=-100000, gridcount=10, area=gridcount ÷ 10)
#     count = gridcount + 2 * area
#     m = zeros(count, count, count)
#     global km = m
#     data = data_from_tsx(ktsx, len)
#     tfs = VectorCalcUtil.fit_01.(data)

#     width = 1 / (gridcount - 1)
#     gridcs = [togrid(tfs[i].v, width) for i in eachindex(tfs)]

#     for i in eachindex(tfs[1].v)
#         for x in toarea(gridcs[1][i], area), y in toarea(gridcs[2][i], area), z in toarea(gridcs[3][i], area)
#             m[x, y, z] = kernel()
#         end
#     end
# end

#region Explore
function calckel(prob, lms::Coll{N,<:LegLike}) where N
    # segs, _ = Between.toSegmentsN(lms)
    segs = LL.toSegments(lms)
    riskrat = calcriskrat(Date.(pmk.tss(prob))...)
    risk = riskrat * max(Pricing.calcMarg(prob.center, segs))
    calckel(Kelly.make_buf(), prob, risk, segs)
end

function drawfield(m)
    inds = reshape(CartesianIndices(m), prod(size(m)))
    ps = [length(m[ind]) for ind in inds]
    xyzs = [Tuple(i) for i in inds]
    # global kxyzs = xyzs
    plot = Makie.scatter(xyzs; color=ps, markersize=5, colormap=:viridis, axis=(type=Makie.Axis3,))
    #  camera=Makie.cam3d_cad!
    display(plot)
    # return Makie.camera(plot.figure.scene)
    scene = plot.figure.scene
    Makie.cam3d_cad!(scene)
end

function drawrandfield(kde::SimpleKde, num=100; k=0.001)
    # pts = Vector{Vector{Float64}}()
    pts = Vector{NTuple{3,Float64}}()
    denss = Vector{Float64}()
    skipped = 0
    while length(pts) < num
        txin = Tuple(rand(3))
        x = pmk.txout(kde, txin)
        # @show txin x
        dens = pmk.kde_pdf(kde, x, k)
        if dens > 100
            push!(pts, x)
            push!(denss, dens)
        else
            skipped += 1
        end
    end
    @show skipped
    @show extrema(denss)
    VectorCalcUtil.normalize!(denss)
    plot = Makie.scatter(pts; color=denss, markersize=5, colormap=:viridis, axis=(type=Makie.Axis3,))
    #  camera=Makie.cam3d_cad!
    display(plot)
    # return Makie.camera(plot.figure.scene)
    scene = plot.figure.scene
    # Makie.cam3d_cad!(scene)
    return
end
#endregion Explore

alltrue(_...) = true

@nospecialize
kv(;kws...) = return (;kws...)
nc(x) = collect(x)
nc(x::LL.Segments) = return x # ( global kseg = x ; return x )
@specialize

end