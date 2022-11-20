module Joe
using Dates, NamedTupleTools
using Globals, SH, BaseTypes, SmallTypes, Bins, LegMetaTypes, RetTypes, StratTypes
using DateUtil, OptionUtil, CalcUtil, ThreadUtil, OutputUtil, LogUtil, DictUtil, CollUtil
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
function jorn(exs; nopos=false, kws...)
    empty!(Skipped)
    global Sorter = r -> r.roiEv
    global ressOrig = Vector{Vector{NamedTuple}}(undef,maximum(exs))
    global ress = Vector{Vector{NamedTuple}}(undef,maximum(exs))
    global ctxs = Vector{NamedTuple}(undef,maximum(exs))
    isLegAllowed = nopos ? (_,_)->true : entryFilterOption()

    for i in exs
        r, ctx = runJorn(expir(i), isLegAllowed; nopos, kws...)
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
            r.met.mx
        ))
    end
    pretyble(res)
end

function runJorn(xpr::Date, isLegAllowed; nopos=false, all=false, posLms=nothing, condors=true, spreads=false)
    global ctx = makeCtx(xpr; nopos, all)
    oqssAll = Chains.getOqss(xpr, ctx.curp, nopos ? posLms : xlms(xpr))
    oqss = filtOqss(oqssAll) do oq
        abs(getStrike(oq) / ctx.curp - 1.0) < 0.1
    end
    @log debug "jorn processing" xpr length(oqss) length(oqssAll) ctx.curp

    # GenCands.iterSingle(oqss, ctx, res) do lms, c, r
    #     jr = joe(c, lms)
    #     if jr.rate > 0.0
    #         push!(r, jr)
    #     end
    # end

    resSpreads = [Vector{NamedTuple}() for _ in 1:Threads.nthreads()]
    if spreads
        cnt = 0
        empty!(Msgs)
        GenCands.paraSpreads(oqss, ctx.maxWidth/2.0, isLegAllowed, ctx, resSpreads) do lms, c, rs
            cnt += 1
            jr = joeSpread(c, lms)
            if !isnothing(jr)
                push!(rs[Threads.threadid()], jr)
            end
            return true
        end
        isempty(Msgs) || @info Msgs
    end

    resConds = [Vector{NamedTuple}() for _ in 1:Threads.nthreads()]
    if condors
        cnt = 0
        empty!(Msgs)
        # Profile.clear()
        GenCands.iterCondors(oqss, ctx.maxWidth / 2.0, ctx.curp, isLegAllowed, ctx, resConds) do cond, c, rs
            cnt += 1
            jr = joeCond(c, cond)
            if !isnothing(jr)
                push!(rs[Threads.threadid()], jr)
            end
            return true
        end
        isempty(Msgs) || @info Msgs
    end

    res = reduce(vcat, flat(resSpreads, resConds))
    @log debug "jorn proced results" cnt length(res)
    return (res, ctx)
end

#==
 j.filt(r -> getStrike(r.lms[end]) < curp)
 j.filt(r -> getStrike(r.lms[end]) - getStrike(r.lms[1]) < 8.0)
 j.filt(r -> r.theta > 0.0 && r.delta > 0.0 && r.met.prob >= .9 && r.roiEv > .01 && r.met.mn > -3.0)
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
    combineRetVals!(tctx.retBuf1, condRets(cond))
    ret = Ret(tctx.retBuf1, ctx.curp, 4)
    adjust!(ret.vals, ret.numLegs) # reduce for slippage and closing short cost
    # ret.vals[end] > 0 || return nothing # TODO: remove?
    lms = toLms(cond)
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
    MinMx = 0.7
    # (getStrike(lms[4]) - getStrike(lms[1])) <= ctx.maxWidth || ( (shouldTrackSkipped && trackSkipped("max strike width")) ; return nothing )
    all = isnothing(allo) ? ctx.all : allo
    (all || getTheta(lms) >= 0.0) || ( (shouldTrackSkipped && trackSkipped("theta")) ; return nothing )
    met = calcMetrics(ctx.prob, ret)
    rateEv = ctx.timt * met.ev / (-met.mn)
    rateEvr = ctx.timt * met.evr / (-met.mn)
    rate = ctx.timt * met.profit / (-met.mn)
    # 0.0 < met.prob < 1.0 || ( (shouldTrackSkipped && trackSkipped("prob")) ; return nothing )

    # must = ret.vals[1] > -0.1 && ret.vals[end] > -0.1
    # must || ( (shouldTrackSkipped && trackSkipped("must")) ; return nothing )
    extra = ret.vals[1] >= MinMx && ret.vals[end] >= MinMx
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
                return (;roi, roiEv, roiEvr, rate, rateEv, kelly, met, metb, delta=getDelta(lms), gamma=getGamma(lms), theta=getTheta(lms))
            # else
                # # shouldTrackSkipped && trackSkipped("max loss both: $((minb, ctx.posMin, maxLossBoth))")
                # shouldTrackSkipped && trackSkipped("max loss both")
            # end
        # end
    else
        if shouldTrackSkipped
            checks = ["met.mx >= MinMx", "met.mn >= maxLoss", "met.prob >= 0.85", "met.ev >= 0.01", "extra"]
            checkVals = [met.mx >= MinMx, met.mn >= maxLoss, met.prob >= 0.85, met.ev >= 0.01, extra]
            i = findfirst(x -> !x, checkVals)
            trackSkipped(checks[i])
        end
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

import LegTypes
function runlc(xpr; maxSpreads=1000, kws...)
    jorn(xpr; all=true, condors=false, spreads=true, nopos=true, posLms=LegMeta[])
    # filt(r -> r.met.mx >= (-r.met.mn + .04))
    global lmss = first([x.lms for x in ress[xpr]], maxSpreads)
    rets = map(lmss) do lms
        ret = combineTo(Ret, lms, ctx.curp)
        adjust!(ret.vals, ret.numLegs) # reduce for slippage and closing short cost
        return ret
    end
    println("findLinCombo for ", length(lmss))

    check = function(qtys)
        indqs = g0q(qtys)
        # TODO: if keep, optimize
        # lms = collect(flat(lmss[inds]...))
        lms = lmsForQtys(lmss, qtys)
        getTheta(lms) > 0.0 || return false
        !LegTypes.hasConflict(lms) || return false

        tctx = ctx.threads[Threads.threadid()]
        combineRetVals!(tctx.retBuf1, rets, indqs)
        ret = Ret(tctx.retBuf1, ctx.curp, 2 * length(lms))
        met = calcMetrics(ctx.prob, ret)
        return met.prob > .6
    end
    global res = findLinCombo(lmss, check; kws...)
    inds = g0q(res.qtys)
    println("Best score ", res.score, ' ', inds, " after iterations ", res.iters)

    # drlms(flat(lmss[inds]...));
    return lmsForQtys(lmss, res.qtys)
end

function lmsForQtys(lmss, qtys)
    inds = g0(qtys)
    # println("lmsfq ", length(lmss), ' ', length(qtys), ' ', inds)
    return collect(Iterators.flatten(map(i -> (withQuantity(lmss[i][1], qtys[i]), withQuantity(lmss[i][2], qtys[i])), inds)))
end

function testlc(;maxIter=10)
    num = 4
    buf1 = zeros(UInt8, num)
    buf2 = zeros(UInt8, num)
    findLinCombo(buf1, buf2, rand(num), (x,q)->dot(x,q), x->true; maxIter)
end

findLinCombo(lmss, isConflict; kws...) = findLinCombo(zeros(UInt8, length(lmss)), zeros(UInt8, length(lmss)), lmssToVdg(lmss), scoreVdg, isConflict; kws...)
function findLinCombo(res, qtys, cands, score, check; maxQtyTotal = 12, scoreTarget = 0.00001, maxIter = 1e6)
    # presorted by desirability, so prefer low indices
    # MaxQtyPer = 3
    # MaxSprs = 12
    # qb = zeros(UInt8, len)
    # qtys = zeros(UInt8, len)
    len = length(qtys)
    scor = 10000.0
    scorBest = scor
    i = 0
    qt = 0
    for qtyTot in 2:maxQtyTotal
        qt = qtyTot
        left = 0
        @label start
        fill!(qtys, 0)
        nz2 = left+1
        nz1 = left+2
        qtys[nz2] = qtyTot - 1
        qtys[nz1] = 1

        while true
            # prit(qtys, ' ', nz2, ' ', nz1)
            # Score new case and break if acceptable
            # scor = score(vals(cands, qtys))
            scor = score(cands, qtys)
            if scor < scorBest && check(qtys)
                scorBest = scor
                copy!(res, qtys)
                println("New best score: ", scor)
                # prit(qtys)
                if scor <= scoreTarget
                    println("Stopping, found score: ", scor, " after iterations ", i, ' ', g0q(res))
                    @goto abort
                end
            end
            i += 1
            if i % 1000000 == 0
                println("Iterations: ", i, ' ', g0(qtys))
            end
            i < maxIter || ( println("Stopping due to max iterations") ; @goto abort )

            if nz1 != len
                qtys[nz1] -= 1
                if qtys[nz1] > 0
                    nz2 = nz1
                end
                nz1 += 1
                qtys[nz1] += 1
            else # nz1 == len
                nz2Prev = nz2
                if qtys[nz2] == 1
                    while true
                        nz2 -= 1
                        if nz2 == left
                            left += 1
                            if left == len - 1
                                println("Proced all for qty ", qtyTot, " after iterations ", i)
                                @goto done
                            else
                                # println("Shifting left ", left)
                                @goto start
                            end
                        end
                        if qtys[nz2] > 0
                            break
                        end
                    end
                end
                moving = qtys[len]
                qtys[len] = 0
                qtys[nz2Prev+1] = moving + 1
                qtys[nz2Prev] -= 1
                nz1 = nz2Prev + 1
                # TODO: the qtys at the end will be wrong unless move the below above, or fix it inside this
            end
        end
        @label done
    end
    @label abort
    println("Reached quantity ", qt)
    return (;qtys=res, score=scorBest, iters=i)
end

g0(v) = findall(x -> x > 0, v)
g0q(v) = [(i, v[i]) for i in findall(x -> x > 0, v)]

function lmssToVdg(lmss)
    len = length(lmss)
    vd = Vector{Float64}(undef, len)
    vg = Vector{Float64}(undef, len)
    for i in eachindex(lmss)
        vd[i] = getDelta(lmss[i])
        vg[i] = getGamma(lmss[i])
    end
    return (vd, vg)
end

using LinearAlgebra
scoreVdg(vdg, qtys) = abs(dot(vdg[1], qtys)) + 8 * abs(dot(vdg[2], qtys))

function lmsvals(lmss, qtys)
    val1 = 0.0
    val2 = 0.0
    i = 1
    for lms in lmss
        qty = qtys[i]
        val1 += getDelta(lms) * qty
        val2 += getGamma(lms) * qty
        i += 1
    end
    return (val1, val2)
end
# lmsval1(lms) = getDelta(lms)
# lmsval2(lms) = getGamma(lms)
# val1(lmss, qtys) = sum(getDelta.(lmss) .* qtys)
# val2(lmss, qtys) = sum(getGamma.(lmss) .* qtys)
lmsscore((val1, val2)) = abs(val1) + 8 * abs(val2)

# scoreZero(lms, qtys) = abs(sum(getDelta.(lms) .* qtys)) + 8 * abs(sum(getGamma.(lms) .* qtys))
# score(lms, qtys) = scoreZero(lms, qtys) # + 0.00001 * sum(qtys) + 0.1 * sum(getTheta.(lms) .* qtys)

prit(qtys, args...) = println(collect(Int, qtys), args...)

end