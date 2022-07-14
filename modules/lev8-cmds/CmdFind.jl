module CmdFind
using Dates
using SH, Globals, Bins, DateUtil, OutputUtil, DictUtil
using SmallTypes, LegTypes, LegMetaTypes, RetTypes, StatusTypes
using CalcUtil, Shorthand, Emails
using Snapshots, Markets, Expirations, Chains, ConTime, Calendars, StoreTrade
using CmdStrats # TODO: probsFor somewhere elese
using DrawStrat

export testOne, din, scandin
export lmsFor, dinDraw

using CmdUtil, CmdExplore, DrawStrat

function makeCtx(i::Int)
    exp = expir(i)
    curp = market().curp
    probs = probsFor(exp; curp)
    lmsPos = findLmsPos(exp)
    retPos = isempty(lmsPos) ? nothing : combineTo(Ret, lmsPos, curp)
    metPos = isnothing(retPos) ? nothing : calcMetrics(probs[1], retPos, Bins.binds())
    scorePos = isnothing(retPos) ? -Inf : score(retPos, metPos)
    return (; exp, probs, curp, lmsPos, retPos, metPos, scorePos)
end
function makeCtx(ctx, lmsRun, retRun, metRun)
    scoreRun = isnothing(retRun) ? -Inf : score(retRun, metRun)
    return (; ctx.exp, ctx.probs, ctx.curp, lmsPos=lmsRun, retPos=retRun, metPos=metRun, scorePos=scoreRun)
end

function din(i::Int)
    ctx = makeCtx(i)
    lmss, rets, mets = CmdFind.findBestAll(ctx);
    if !isempty(lmss)
        dinDraw(ctx, lmss, rets, mets)
    else
        println("Not found.")
    end
    return (; lmss, rets, mets, ctx)
end

lmsFor(i::Int) = vcat(Main.save[:track][expir(i)].lmss...)

dinDraw(i::Int) = dinDraw(Main.save[:track][expir(i)])
dinDraw(all::NamedTuple) = dinDraw(all.ctx, all.lmss, all.rets, all.mets)
function dinDraw(ctx, lmss, rets, mets)
    display(drawRet(rets[end]; ctx.probs, ctx.curp, label="all"))
    if !isnothing(ctx.metPos)
        println("0: ", ctx.metPos)
        drawRet!(ctx.retPos; label="0")
    end
    for i in 1:length(mets)
        # ret = combineTo(Ret, lmss[i], market().curp)
        println("$(i): ", mets[i])
        drawRet!(rets[i]; label=string(i))
    end
end

function scandin()
    track = Dict{Date,Any}()
    while true
        minEvrs = hasproperty(Main, :MinEvrs) ? Main.MinEvrs : Dict{Int,Float64}()
        check = exsToScan()
        Main.save[:track] = track
        for i in check
            ctx = makeCtx(i)
            println("[$(nowz())] ======== Searching for $(i) : $(ctx.exp) ========")
            lmss, rets, mets = CmdFind.findBestAll(ctx);
            if !isempty(lmss)
                # metsShow = isnothing(ctx.metPos) ? mets : vcat(ctx.metPos, mets)
                minEvr = tryKey(minEvrs, i)
                fromEvr = isnothing(ctx.metPos) ? -Inf : ctx.metPos.evr
                toEvr = mets[end].evr
                impEvr = toEvr - fromEvr
                if (isnothing(minEvr) ? impEvr > .02 : toEvr > minEvr)
                    track[ctx.exp] = (;ctx, lmss, mets, rets)
                    prety = spretyble(isnothing(ctx.metPos) ? mets : vcat(ctx.metPos, mets))
                    println("Cumulative:")
                    println(prety)
                    sendEmail("***REMOVED***", "Found din scan entry for $(i) $(ctx.exp)", prety)
                else
                    println("Evr too low ", (;fromEvr, toEvr, minEvr))
                end
            else
                # delete!(track, ctx.exp)
            end

            sleep(2.0)
        end
        sleep(8.0)
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
function testOne()
    month = 5
    day = 20
    snap(month,day,7,0)
    exp = expir(8)
    # filt = ConTime.dateHourFilter(Date(2022, month, day-1), exp, 14)
    filt = ConTime.dateFilter(Date(2022, month, day-1), exp)
    lmsPos = Vector{LegMeta}()
    Main.save[:exp] = exp
    Main.save[:rets] = rets
    Main.save[:lmsPos] = lmsPos

    ConTime.runForSnaps(filt) do name, ts
        println(name)
        findBestAll(exp, lmsPos)
    end
    prob = probsFor(exp)[1]
    cret = combineTo(Ret, lmsPos, market().curp)
    Main.save[:cret] = cret
    met = calcMetrics(prob, cret, Bins.binds())
    Main.save[:met] = met
    drawRet(cret; label="c", probs=[prob])
    @info "Result" length(rets) met
    return (lmsPos, rets, cret, met)
end

function findBestAll(ctx)
    lmss = Vector{Vector{LegMeta}}()
    rets::Vector{Ret} = Ret[]
    mets = Vector{NamedTuple}()
    lmsRun = copy(ctx.lmsPos)
    oqs = filter(isValid(Globals.get(:Strats), ctx.curp), chains()[ctx.exp].chain)
    while true
        scoreTo, res = findBest(ctx, oqs)
        if !isnothing(res)
            (lms, retRun, metRun) = res
            # if met.evr < evr
            #     break
            # else
            #     evr = met.evr
            # end
            report(ctx.curp, lms, metRun, ctx.scorePos, scoreTo)
            push!(lmss, lms)
            push!(rets, retRun)
            push!(mets, metRun)
            append!(lmsRun, lms)
            # @info "met" getStyle.(lms) getSide.(lms) met.mn met.mx met.evr met.prob
            ctx = makeCtx(ctx, lmsRun, retRun, metRun)
        else
            break
        end
    end
    return (lmss, rets, mets)
end

function findBest(ctx, oqs)
    incCallShort, incPutShort = countType(ctx.lmsPos)

    res = (ctx.scorePos, nothing)
    res = findBestSpread(ctx, oqs, res)
    for oq in oqs
        incShort = getStyle(oq) == Style.call ? incCallShort : incPutShort
        lm = to(LegMeta, oq, Side.long)
        res = lyze(ctx, [lm], res)
        if incShort
            lm = to(LegMeta, oq, Side.short)
            res = lyze(ctx, [lm], res)
        end
    end
    return res # (res[2], (scorePos, res[1]))
end

function findBestSpread(ctx, oqs, res)
    calls = filter(isCall, oqs)
    puts = filter(isPut, oqs)
    res = findBestSpreadS(ctx, calls, res)
    res = findBestSpreadS(ctx, puts, res)
    return res # scoreMaxC > scoreMaxP ? (resC, scoreMaxC) : (resP, scoreMaxP)
end

function findBestSpreadS(ctx, oqs, res)
    len = length(oqs)
    for i in 1:len
        for j in (i+1):len
            lms1 = [to(LegMeta, oqs[i], Side.short), to(LegMeta, oqs[j], Side.long)]
            lms2 = [to(LegMeta, oqs[i], Side.long), to(LegMeta, oqs[j], Side.short)]
            res = lyze(ctx, lms1, res)
            res = lyze(ctx, lms2, res)
        end
    end
    return res
end

function lyze(ctx, lms::Vector{LegMeta}, res)
    for lm in lms
        for lmp in ctx.lmsPos
            !isConflict(lm, lmp) || (return res)
        end
    end
    # TODO: we could calc the filter directly without creating ret and met
    retOne = combineTo(Ret, lms, ctx.curp)
    metOne = calcMetrics(ctx.probs[1], retOne, Bins.binds())
    # Bad data, you generally can't really get a 100% prob position. arbitrage exists, but don't count on getting lucky.
    metOne.prob < 1.0 || return -Inf
    metOne.loss < 0.0 || return -Inf

    lmsBoth = vcat(lms, ctx.lmsPos)
    ret = combineTo(Ret, lmsBoth, ctx.curp)
    met = calcMetrics(ctx.probs[1], ret, Bins.binds())
    s = score(ctx.retPos, ctx.metPos, ret, met)
    # println("$(s) > $(res[1])")
    return s > res[1] ? (s, (lms, ret, met)) : res
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

function score(retFrom, metFrom, retTo, metTo)
    metTo.mn > -3.1 || return -Inf
    scoreTo = score(retTo, metTo)
    if !isnothing(metFrom)
        metTo.prob >= .95 * metFrom.prob || return -Inf
        scoreFrom = score(retFrom, metFrom)
        (scoreTo - scoreFrom) / abs(scoreFrom) > .02 || return -Inf
    end
    adj = adjust(scoreTo, retTo)
    if adj != scoreTo
        println("adjusted ", scoreTo, " to ", adj)
    end
    return adj
end
adjust(score, ret) = score # ret[2] > ret[end-1] ? score : (score < 0.0 ? 2 * score : 0.5 * score)
score(ret, met) = adjust(met.evr, ret)

end