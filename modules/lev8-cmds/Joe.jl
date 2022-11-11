module Joe
using Dates, NamedTupleTools
using Globals, SH, BaseTypes, SmallTypes, Bins, LegMetaTypes, RetTypes, StratTypes
using DateUtil, OptionUtil, CalcUtil, ThreadUtil, OutputUtil, LogUtil, DictUtil
import GenCands
using Calendars, Expirations, Chains, ProbKde, Markets
using CmdUtil
using CmdPos

export jorn

# rs = Vector(undef,20) ; [(i, (rs[i] = jorn(expir(i)); rs[i][1].roi)) for i in 1:1]

function defaultStrats()
    cfg = Globals.get(:Strats)
    cfg[:maxStrikeDist] = 28.0
    cfg[:maxPutHeight] = 3.2
    cfg[:maxCallHeight] = 22.0
    return
end

function allStrats()
    cfg = Globals.get(:Strats)
    cfg[:maxStrikeDist] = 99999.0
    cfg[:maxPutHeight] = 99999.0
    cfg[:maxCallHeight] = 99999.0
    return
end

mla() = ( MaxLossAdd[] = -2.5 ;  MaxLoss[] = -0.01 ; MaxLossExpr[] = -0.02 )
mli() = ( MaxLoss[] = -2.0 ; MaxLossExpr[] = -7.9 )
ml0() = ( MaxLoss[] = -0.5 ; MaxLossExpr[] = -0.75 )
ml1() = ( MaxLoss[] = -1.0 ; MaxLossExpr[] = -1.5 )
ml2() = ( MaxLoss[] = -3.0 ; MaxLossExpr[] = -4.0 )
ml3() = ( MaxLoss[] = -4.0 ; MaxLossExpr[] = -5.0 )
mlx() = ( MaxLoss[] = -9999.0 ; MaxLossExpr[] = -9999.0 )

# export @u
# macro u(i)
#     # do i = 2 ; r = j.ress[i][1] ; xdr(i, r.lms) ; and disp passing in row to highlight
# end

# import Profile
function jorn(exs; kws...)
    empty!(Skipped)
    global Sorter = r -> r.roiEv
    global ressOrig = Vector{Vector{NamedTuple}}(undef,maximum(exs))
    global ress = Vector{Vector{NamedTuple}}(undef,maximum(exs))
    global ctxs = Vector{NamedTuple}(undef,maximum(exs))
    isLegAllowed = entryFilterOption()

    for i in exs
        r, ctx = runJorn(expir(i), isLegAllowed; kws...)
        !isempty(r) || continue
        ressOrig[i] = r
        ctxs[i] = ctx
        # push!(rs1, merge((;i), r[1]))
    end
    filt(r -> true)
    disp()
    return
end
function disp()
    res = NamedTuple[]
    for i in eachindex(ress)
        isassigned(ress, i) && !isempty(ress[i]) || continue
        r = ress[i][1]
        ctx = ctxs[i]
        push!(res, (;
            ex=i,
            delete(r, :lms, :met, :metb)...,
            ev=r.met.ev,
            evr="$(rd3(ctx.posMet.evr)):$(rd3(r.met.evr)):$(rd3(r.metb.evr))",
            prob="$(rd3(ctx.posMet.prob)):$(rd3(r.met.prob)):$(rd3(r.metb.prob))",
            r.met.mx,
            thetaDir=getThetaDir(r.lms)
        ))
    end
    pretyble(res)
end

function runJorn(xpr::Date, isLegAllowed; nopos=false, all=false)
    global ctx = makeCtx(xpr; nopos, all)
    oqssAll = Chains.getOqss(xpr, ctx.curp, xlms(xpr))
    oqss = filtOqss(oqssAll) do oq
        abs(getStrike(oq) / ctx.curp - 1.0) < 0.1
    end
    @log debug "jorn processing" xpr length(oqss) length(oqssAll) ctx.curp
    ress = [Vector{NamedTuple}() for _ in 1:Threads.nthreads()]

    # GenCands.iterSingle(oqss, ctx, res) do lms, c, r
    #     jr = joe(c, lms)
    #     if jr.rate > 0.0
    #         push!(r, jr)
    #     end
    # end

    # cnt = 0
    # empty!(Msgs)
    # GenCands.paraSpreads(oqss, ctx.maxStrikeDist, ctx, ress) do lms, c, rs
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

    # println("Iterating over condors...")
    cnt = 0
    empty!(Msgs)
    # Profile.clear()
    GenCands.iterCondors(oqss, ctx.maxWidth / 2.0, ctx.curp, isLegAllowed, ctx, ress) do cond, c, rs
        cnt += 1
        jr = joeCond(c, cond)
        if !isnothing(jr)
            push!(rs[Threads.threadid()], jr)
        end
        return true
    end
    isempty(Msgs) || @info Msgs

    res = reduce(vcat, ress)
    @log debug "jorn proced results" cnt length(res)
    return (res, ctx)
end


#==
 j.filt(r -> getStrike(r.lms[end]) < curp)
 j.filt(r -> getStrike(r.lms[end]) - getStrike(r.lms[1]) < 8.0)
 ==#
function filt(f)
    for i in eachindex(ressOrig)
        isassigned(ressOrig, i) || continue
        ress[i] = filter(f, ressOrig[i])
    end
    sor(Sorter)
end

# function sor(sym::Symbol)
#     for i in eachindex(ress)
#         isassigned(ress, i) || continue
#         sort!(ress[i]; rev=true, by=x-> haskey(x, sym) ? x[sym] : x.met[sym])
#     end
# end

# Sort by total spread width: j.sor(x -> -(getStrike(x.lms[4]) - getStrike(x.lms[1])))
function sor(f)
    global Sorter = f
    for i in eachindex(ress)
        isassigned(ress, i) || continue
        sort!(ress[i]; rev=true, by=f)
    end
end

using Caches, TradierData

#region Local
# const lock = ReentrantLock()
const MaxLossExpr = Ref{Float64}(-3.0)
const MaxLoss = Ref{Float64}(-2.0)
const MaxLossAdd = Ref{Float64}(-2.0)
const MaxWidth = Ref{Float64}(8.1)

# calcRate(to::Date, ret, risk) = (ret / risk) * (1 / Calendars.texToYear(calcTex(now(UTC), to)))

# export lms
# import CmdExplore
# toLms(condor::Condor) = map(x -> x[1], Iterators.flatten(condor))
toLms(condor::Condor) = (condor[1][1][1], condor[1][2][1], condor[2][1][1], condor[2][2][1])
condRets(condor::Condor) = (condor[1][1][2], condor[1][2][2], condor[2][1][2], condor[2][2][2])
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

function makeCtx(xpir::Date; nopos, all)::NamedTuple
    # start = market().tsMarket
    curp = market().curp
    from = market().startDay
    days = (1 + bdays(from, xpir))
    timt = timult(xpir, from)
    prob = xprob(xpir)
    posLms = nopos ? LegMeta[] : xlms(xpir)
    posRet = combineTo(Ret, posLms, curp)
    posMet = calcMetrics(prob, posRet)
    threads = [(;
        kelBuf1 = Bins.empty(),
        kelBuf2 = Bins.empty(),
        retBuf1 = Bins.empty(),
        retBuf2 = Bins.empty()
    ) for _ in 1:Threads.nthreads()]
    return (;
        all,
        curp,
        prob,
        days,
        timt,
        posLms,
        posRet,
        posMet,
        posMin=minimum(posRet.vals),
        threads,
        maxWidth = MaxWidth[]
        # maxCallHeight = Globals.get(:Strats)[:maxCallHeight],
        # maxPutHeight = Globals.get(:Strats)[:maxPutHeight]
    )
end

import Kelly

function joeSpread(ctx, lms::NTuple{2,LegMeta})
    tctx = ctx.threads[Threads.threadid()]
    ret = combineTo(Ret, lms, ctx.curp)
    # TODO: use thread buffer, tctx.retBuf1
    adjust!(ret.vals, ret.numLegs) # reduce for slippage and closing short cost
    r = joe(ctx, tctx, ret, lms)
    return isnothing(r) ? nothing : merge(r, (;lms))
end
using Rets
function joeCond(ctx, cond::Condor)
    tctx = ctx.threads[Threads.threadid()]
    lms = toLms(cond)
    combineRetVals!(tctx.retBuf1, condRets(cond))
    ret = Ret(tctx.retBuf1, ctx.curp, 4)
    adjust!(ret.vals, ret.numLegs) # reduce for slippage and closing short cost
    # ret.vals[end] > 0 || return nothing # TODO: remove?
    r = joe(ctx, tctx, ret, lms)
    return isnothing(r) ? nothing : merge(r, (;lms))
end
function testOne(r)
    empty!(Skipped)
    ret = combineTo(Ret, r.lms, ctx.curp)
    joe(ctx, ctx.threads[1], ret, r.lms; allo=false)
    println(Skipped)
end
function joe(ctx, tctx, ret, lms; allo=nothing)::Union{Nothing,NamedTuple}
    shouldTrackSkipped = false
    MinMx = 0.17
    # getThetaDir(lms) >= 0.0 || ( (shouldTrackSkipped && trackSkipped("thetaDir")) ; return nothing )
    # (getStrike(lms[4]) - getStrike(lms[1])) <= ctx.maxWidth || ( (shouldTrackSkipped && trackSkipped("max strike width")) ; return nothing )
    all = isnothing(allo) ? ctx.all : allo
    met = calcMetrics(ctx.prob, ret)
    rateEv = ctx.timt * met.ev / (-met.mn)
    rateEvr = ctx.timt * met.evr / (-met.mn)
    rate = ctx.timt * met.profit / (-met.mn)
    0.0 < met.prob < 1.0 || ( (shouldTrackSkipped && trackSkipped("prob")) ; return nothing )

    # must = ret.vals[1] > -0.1 && ret.vals[end] > -0.1
    # must || ( (shouldTrackSkipped && trackSkipped("must")) ; return nothing )
    extra = ret.vals[1] > MinMx # && ret.vals[end] > MinMx
    maxLoss = (ctx.days-1) * MaxLoss[] + MaxLossAdd[]
    # maxLossBoth = (ctx.days-1) * MaxLossExpr[] + MaxLossAdd[]
    if all || (met.mx >= MinMx && met.mn >= maxLoss && met.ev >= 0.01 && extra) # rateEv >= 0.5 && met.prob >= 0.85
            # kelly = ckel(ctx.prob, ret)
        kelly = Kelly.ded!(tctx.kelBuf1, tctx.kelBuf2, ctx.prob.vals, ret.vals, -met.mn)
        # if all || kelly > 0.0
            kelly = max(kelly, 0.0)
            Rets.addRetVals!(tctx.retBuf2, ctx.posRet.vals, ret.vals)  # combineTo(Ret, vcat(ctx.posLms, lms...), ctx.curp)
            # valsb = tctx.retBuf2
            # minb = minimum(valsb)
            # if all || (minb >= ctx.posMin || minb > maxLossBoth)
                roi = rate * kelly
                roiEv = rateEv * kelly
                roiEvr = rateEvr * kelly
                # rate = ctx.timult * met.mx / (-met.mn)
                retb = Ret(tctx.retBuf2, ctx.curp, ctx.posRet.numLegs + 4)
                metb = calcMetrics(ctx.prob, retb)
                return (;roi, roiEv, roiEvr, rate, rateEv, kelly, met, metb)
            # else
                # # shouldTrackSkipped && trackSkipped("max loss both: $((minb, ctx.posMin, maxLossBoth))")
                # shouldTrackSkipped && trackSkipped("max loss both")
            # end
        # end
    else
        checks = ["met.mx >= MinMx", "met.mn >= maxLoss", "met.prob >= 0.85", "met.ev >= 0.01", "extra"]
        checkVals = [met.mx >= MinMx, met.mn >= maxLoss, met.prob >= 0.85, met.ev >= 0.01, extra]
        i = findfirst(x -> !x, checkVals)
        shouldTrackSkipped && trackSkipped(checks[i])
        # shouldTrackSkipped && trackSkipped("if") # $(checks[i]) $(eval(Meta.parse(x)))")
    end
    return nothing
end

const Skipped = Dict{String,Int}()
function trackSkipped(s)
    runSync(lockMsg) do
        incKey(Skipped, s)
    end
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