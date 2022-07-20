module CmdFind
using Dates
using SH, Globals, Bins, DateUtil, OutputUtil, DictUtil
using SmallTypes, LegTypes, LegMetaTypes, RetTypes, StatusTypes, OptionTypes
using CalcUtil, Shorthand, Emails
using Snapshots, Markets, Expirations, Chains, ConTime, Calendars, StoreTrade
using CmdStrats # TODO: probsFor somewhere elese
using DrawStrat

export testOne, din, scandin
export ofor, lmsFor, lmssd, dinDraw

using CmdUtil, CmdExplore, DrawStrat

function makeCtx(i::Int)
    exp = expir(i)
    curp = market().curp
    probs = probsFor(exp; curp)
    lmsPos = xlms(exp)
    retPos = combineTo(Ret, lmsPos, curp)
    metPos = calcMetrics(probs[1], retPos, Bins.binds())
    scorePos = score(retPos, metPos)
    return (; exp, probs, curp, lmsPos, retPos, metPos, scorePos)
end
function makeCtx(ctx, lmsRun, retRun, metRun)
    scoreRun = score(retRun, metRun)
    return (; ctx.exp, ctx.probs, ctx.curp, lmsPos=lmsRun, retPos=retRun, metPos=metRun, scorePos=scoreRun)
end

function din(i::Int)
    ctx = makeCtx(i)
    resAll = CmdFind.findBestAll(ctx);
    # score, lmss, retRuns, metRuns, ret1s, met1s = allRes
    if !isempty(resAll.lmss)
        dinDraw(ctx, resAll)
    else
        println("Not found.")
    end
    # return (; score, lmss, retRuns, metRuns, ret1s, met1s)
    return (;ctx, resAll...)
end

ofor(i::Int) = Main.save[:track][expir(i)]
lmsFor(i::Int) = vcat(ofor(i).lmss...)
lmssd(lmss) = drlms(vcat(lmss...))

dinDraw(i::Int) = dinDraw(Main.save[:track][expir(i)])
# dinDraw(all::NamedTuple) = dinDraw(all.ctx, all.lmss, all.rets, all.mets)
addScore(met) = merge(met, (;score=score(nothing, met)))
function dinDraw(ctx, resAll)
    (;ret1s, met1s, retRuns, metRuns) = resAll
    pretyble(map(addScore, vcat(ctx.metPos, met1s, metRuns[end])))
    pretyble(map(addScore,vcat(ctx.metPos, metRuns)))
    drawRet(ctx.retPos; ctx.probs, ctx.curp, label="from")
    for i in 1:length(ret1s)
        drawRet!(ret1s[i]; label=string(i))
    end
    drawRet!(retRuns[end]; label="to")
    return
end

# (default(::Type{T})::T) where T = T()
# default(::Type{Int64})::Int64 = 0
# default(::Type{Float64})::Float64 = 0.0

# TODO: instead of hardcoded minevr, use distance from fromevr because it changes when price changes
# TODO: do a single loop through setting MinEvr initially
function scandin()
    track = useKey(Dict{Date,Any}, Main.save, :track)
    minScores = useKey(Dict{Int,Float64}, Main.save, :MinScores)

    # prepopulate min scores
    if isempty(minScores)
        for i in 1:12
            runCheck(i, track, minScores, false)
        end
    end

    while true
        check = exsToScan()
        for i in check
            runCheck(i, track, minScores)
            sleep(2.0)
        end
        sleep(8.0)
    end
end
function runCheck(i, track, minScores, notify=true)
    ImpMin = 0.01
    ctx = makeCtx(i)
    println("[$(nowz())] ======== Searching for $(i) : $(ctx.exp) ========")
    (; scoreTo, lmss, retRuns, metRuns, ret1s, met1s) = CmdFind.findBestAll(ctx);
    if !isempty(lmss)
        # metsShow = isnothing(ctx.metPos) ? mets : vcat(ctx.metPos, mets)
        scoreFrom = ctx.scorePos
        scoreMin = max(tryKey(minScores, i, -Inf), scoreFrom + ImpMin)
        if (scoreTo > scoreMin)
            track[ctx.exp] = (;ctx, lmss, retRuns, metRuns, met1s, ret1s)
            minScores[i] = scoreTo
            prety = spretyble(vcat(ctx.metPos, met1s, metRuns[end]))
            println("Cumulative:")
            println(prety)
            notify && sendEmail("***REMOVED***", "Found din scan entry for $(i) $(ctx.exp)", prety)
        else
            # haskey(track, ctx.exp) || (track[ctx.exp] = (;ctx, lmss, mets, rets))
            println("Evr too low ", (;scoreFrom, scoreTo, scoreMin))
            minScores[i] = max(scoreTo, scoreMin - 0.02 * (scoreMin - scoreTo))
        end
    else
        # delete!(track, ctx.exp)
    end
end

function exsToScan()
    newActive = queryEntered(today(), Starting)
    exAvoid = map(row -> searchsortedfirst(expirs(), row.targetdate), newActive)
    nextMarketChange() - now(UTC) < Hour(4) && push!(exAvoid, 1)
    return setdiff(1:12, exAvoid)
end

#========== Begin: Test One ===========#

# TODO: if it's beyond left or right, set ret * prob = 0 beacuse prob is = 0
# I used Bins.binds() but need to check if that's doing it
# TODO: also check if selling a position not opened today could increase evr, but that gets tricky...
# function testOne()
#     month = 5
#     day = 20
#     snap(month,day,7,0)
#     exp = expir(8)
#     # filt = ConTime.dateHourFilter(Date(2022, month, day-1), exp, 14)
#     filt = ConTime.dateFilter(Date(2022, month, day-1), exp)
#     lmsPos = Vector{LegMeta}()
#     Main.save[:exp] = exp
#     Main.save[:rets] = rets
#     Main.save[:lmsPos] = lmsPos

#     ConTime.runForSnaps(filt) do name, ts
#         println(name)
#         findBestAll(exp, lmsPos)
#     end
#     prob = probsFor(exp)[1]
#     cret = combineTo(Ret, lmsPos, market().curp)
#     Main.save[:cret] = cret
#     met = calcMetrics(prob, cret, Bins.binds())
#     Main.save[:met] = met
#     drawRet(cret; label="c", probs=[prob])
#     @info "Result" length(rets) met
#     return (lmsPos, rets, cret, met)
# end

function findBestAll(ctx)::NamedTuple
    lmss = Vector{Vector{LegMeta}}()
    retRuns = Vector{Ret}()
    metRuns = Vector{NamedTuple}()
    ret1s = Vector{Ret}()
    met1s = Vector{NamedTuple}()
    lmsRun = copy(ctx.lmsPos)

    oqs = getOqs(ctx.exp, ctx.lmsPos, ctx.curp)
    scoreTo = ctx.scorePos
    while true
        scoreTo, res = findBest(ctx, oqs)
        if !isnothing(res)
            (lms, retRun, metRun, ret1, met1) = res
            # if met.evr < evr
            #     break
            # else
            #     evr = met.evr
            # end
            length(lms) + sum(length, lmss; init=0) <= 4 || break
            report(ctx.curp, lms, metRun, ctx.scorePos, scoreTo)
            push!(lmss, lms)
            push!(retRuns, retRun)
            push!(metRuns, metRun)
            push!(ret1s, ret1)
            push!(met1s, met1)
            append!(lmsRun, lms)
            # @info "met" getStyle.(lms) getSide.(lms) met.mn met.mx met.evr met.prob
            ctx = makeCtx(ctx, lmsRun, retRun, metRun)
        else
            break
        end
    end
    return (; scoreTo, lmss, retRuns, metRuns, ret1s, met1s)
end

function findBest(ctx, oqs)::Tuple
    res = (ctx.scorePos, nothing)
    res = findBestSpread(ctx, oqs, res)
    for oq in oqs.call.long
        lm = to(LegMeta, oq, Side.long)
        res = lyze(ctx, [lm], res)
    end

    for oq in oqs.put.long
        lm = to(LegMeta, oq, Side.long)
        res = lyze(ctx, [lm], res)
    end

    incCallShort, incPutShort = countType(ctx.lmsPos)
    if incCallShort
        for oq in oqs.call.short
            lm = to(LegMeta, oq, Side.short)
            res = lyze(ctx, [lm], res)
        end
    end

    if incPutShort
        for oq in oqs.put.short
            lm = to(LegMeta, oq, Side.short)
            res = lyze(ctx, [lm], res)
        end
    end

    return res
end

function findBestSpread(ctx, oqs, res)
    res = findBestSpreadS(ctx, oqs.call, res)
    res = findBestSpreadS(ctx, oqs.put, res)
    return res # scoreMaxC > scoreMaxP ? (resC, scoreMaxC) : (resP, scoreMaxP)
end

function findBestSpreadS(ctx, oqs, res)
    for oq1 in oqs.long, oq2 in oqs.short
        oq1 != oq2 || continue
        lms1 = [to(LegMeta, oq1, Side.long), to(LegMeta, oq2, Side.short)]
        # lms2 = [to(LegMeta, oqs[i], Side.long), to(LegMeta, oqs[j], Side.short)]
        res = lyze(ctx, lms1, res)
        # res = lyze(ctx, lms2, res)
    end
    return res
end

function lyze(ctx, lms::Vector{LegMeta}, res)::Tuple
    # TODO: we could calc the filter directly without creating ret and met
    ret1 = combineTo(Ret, lms, ctx.curp)
    met1 = calcMetrics(ctx.probs[1], ret1, Bins.binds())
    # Bad data, you generally can't really get a 100% prob position. arbitrage exists, but don't count on getting lucky.
    met1.prob < 1.0 || return res
    met1.loss < 0.0 || return res

    lmsBoth = vcat(lms, ctx.lmsPos)
    ret = combineTo(Ret, lmsBoth, ctx.curp)
    met = calcMetrics(ctx.probs[1], ret, Bins.binds())
    s = score(ctx.retPos, ctx.metPos, ret, met, ret1, met1)
    # println("$(s) > $(res[1])")
    return s > res[1] ? (s, (lms, ret, met, ret1, met1)) : res
end

SH.isLong(v) = getSide(v) == Side.long
SH.isShort(v) = getSide(v) == Side.short
SH.isCall(v) = getStyle(v) == Style.call
SH.isPut(v) = getStyle(v) == Style.put
function countType(lms::Vector)
    res = [0, 0, 0, 0]
    for lm in lms
        if isCall(lm)
            i = isLong(lm) ? 1 : 2
        else
            i = isLong(lm) ? 3 : 4
        end
        res[i] += 1
    end
    return (res[1] > res[2], res[3] > res[4])
end

function report(curp, lms, met, scoreFrom, scoreTo)
    exps = [getExpiration(lms[1])]
    if length(lms) == 1
        ss = str1(lms[1], exps)
    elseif length(lms) == 2
        ss = string(str1(lms[1], exps), " / ", str1(lms[2], exps))
    else
        error("unexpected len ", lms)
    end
    ms = join([string(k, '=', round(met[k]; digits=3)) for k in keys(met)], ' ')
    # println(curp, ": ", scoreFrom, " -> ", scoreTo, '\t', ms, "\t: ", ss)
end

str1(lm, exps) = string(tosh(lm, exps), ' ', bap(lm))

function score(retFrom, metFrom, retTo, metTo, ret1, met1)
    # met1.evr >= 0.0 || return -Inf
    metTo.mn > -3.1 || return -Inf
    scoreTo = score(retTo, metTo)
    if !isnothing(metFrom)
        # metTo.prob >= .95 * metFrom.prob || return -Inf
        # metTo.prob > metFrom.prob || return -Inf
        # scoreFrom = score(retFrom, metFrom)
        # (scoreTo - scoreFrom) / abs(scoreFrom) > .02 || return -Inf
    end
    return scoreTo
end
function score(ret, met)
    # score = exp(met.evr) * met.prob
    score = met.evr
    return score
    # adjust(score, ret) = score # ret[2] > ret[end-1] ? score : (score < 0.0 ? 2 * score : 0.5 * score)
    # adj = adjust(score, ret)
    # if adj != score
    #     println("adjusted ", score, " to ", adj)
    # end
    # return adj
end

end