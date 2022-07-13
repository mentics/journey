module CmdFind
using Dates
using SH, Globals, Bins
using SmallTypes, LegTypes, LegMetaTypes, RetTypes
using CalcUtil, Shorthand
using Snapshots, Markets, Expirations, Chains, ConTime
using CmdStrats # TODO: probsFor somewhere elese
using DrawStrat

export testOne

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
    rets = Vector{Ret}()
    lmsPos = Vector{LegMeta}()
    Main.save[:exp] = exp
    Main.save[:rets] = rets
    Main.save[:lmsPos] = lmsPos

    ConTime.runForSnaps(filt) do name, ts
        println(name)
        findBestAll(exp, lmsPos, rets)
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

# exp = expir(14) ; lms, rets, mets = CmdFind.findBestAll(exp, findLmsPos(exp));
function findBestAll(exp, lmsPos::Vector{LegMeta}=LegMeta[], rets::Vector{Ret}=Ret[], mets=[])
    prob = probsFor(exp)[1]
    curp = market().curp
    oqs = filter(isValid(Globals.get(:Strats), curp), chains()[exp].chain)
    # TODO: loop through as long as evr strictly increases
    while true
        res, (scoreFrom, scoreTo) = findBest(curp, prob, oqs, lmsPos)
        if !isnothing(res)
            (lms, met, ret) = res
            # if met.evr < evr
            #     break
            # else
            #     evr = met.evr
            # end
            report(curp, lms, met, scoreFrom, scoreTo)
            append!(lmsPos, lms)
            push!(rets, ret)
            push!(mets, met)
            # @info "met" getStyle.(lms) getSide.(lms) met.mn met.mx met.evr met.prob
        else
            break
        end
    end
    return (lmsPos, rets, mets)
end

function findBest(sp, prob, oqs, lmsPos::Vector{LegMeta})
    incCallShort, incPutShort = countType(lmsPos)

    if !isempty(lmsPos)
        retPos = combineTo(Ret, lmsPos, sp)
        metPos = calcMetrics(prob, retPos, Bins.binds())
        scorePos = score(metPos)
    else
        metPos = nothing
        scorePos = 0.0
    end

    res = (scorePos, nothing)
    res = findBestSpread(sp, prob, lmsPos::Vector{LegMeta}, metPos, oqs, res)
    for oq in oqs
        incShort = getStyle(oq) == Style.call ? incCallShort : incPutShort
        lm = to(LegMeta, oq, Side.long)
        res = lyze(sp, prob, lmsPos, metPos, [lm], res)
        if incShort
            lm = to(LegMeta, oq, Side.short)
            res = lyze(sp, prob, lmsPos, metPos, [lm], res)
        end
    end
    return (res[2], (scorePos, res[1]))
end

function findBestSpread(sp, prob, lmsPos::Vector{LegMeta}, metPos, oqs, res)
    calls = filter(isCall, oqs)
    puts = filter(isPut, oqs)
    res = findBestSpreadS(sp, prob, lmsPos::Vector{LegMeta}, metPos, calls, res)
    res = findBestSpreadS(sp, prob, lmsPos::Vector{LegMeta}, metPos, puts, res)
    return res # scoreMaxC > scoreMaxP ? (resC, scoreMaxC) : (resP, scoreMaxP)
end

function findBestSpreadS(sp, prob, lmsPos::Vector{LegMeta}, metPos, oqs, res)
    len = length(oqs)
    for i in 1:len
        for j in (i+1):len
            lms1 = [to(LegMeta, oqs[i], Side.short), to(LegMeta, oqs[j], Side.long)]
            lms2 = [to(LegMeta, oqs[i], Side.long), to(LegMeta, oqs[j], Side.short)]
            res = lyze(sp, prob, lmsPos::Vector{LegMeta}, metPos, lms1, res)
            res = lyze(sp, prob, lmsPos::Vector{LegMeta}, metPos, lms2, res)
        end
    end
    return res
end

function lyze(sp, prob, lmsPos::Vector{LegMeta}, metPos, lms::Vector{LegMeta}, res)
    for lm in lms
        for lmp in lmsPos
            !isConflict(lm, lmp) || (return res)
        end
    end
    # TODO: we could calc the filter directly without creating ret and met
    retOne = combineTo(Ret, lms, sp)
    metOne = calcMetrics(prob, retOne, Bins.binds())
    # Bad data, you generally can't really get a 100% prob position. arbitrage exists, but don't count on getting lucky.
    metOne.prob < 1.0 || return -Inf
    metOne.loss < 0.0 || return -Inf

    lmsBoth = vcat(lms, lmsPos)
    ret = combineTo(Ret, lmsBoth, sp)
    met = calcMetrics(prob, ret, Bins.binds())
    s = score(metPos, met)
    # println("$(s) > $(res[1])")
    return s > res[1] ? (s, (lms, met, ret)) : res
end

function score(metFrom, metTo)
    metTo.mn > -3.1 || return -Inf
    isnothing(metFrom) || metTo.prob >= .95 * metFrom.prob || return -Inf
    return score(metTo)
end
score(met) = met.evr

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
    println(curp, ": ", scoreFrom, " -> ", scoreTo, '\t', ms, "\t: ", ss)
end

str1(lm, exps) = string(tosh(lm, exps), ' ', bap(lm))

end