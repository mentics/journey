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
    snap(5,20,7,0)
    dateTarget = expir(8)
    # filt = ConTime.dateHourFilter(Date(2022, 5, 19), dateTarget, 14)
    filt = ConTime.dateFilter(Date(2022, 5, 19), dateTarget)
    rets = Vector{Ret}()
    lmsPos = Vector{LegMeta}()
    Main.save[:dateTarget] = dateTarget
    Main.save[:rets] = rets
    Main.save[:lmsPos] = lmsPos

    evr = -Inf
    ConTime.runForSnaps(filt) do name, ts
        println(name)
        prob = probsFor(dateTarget)[1]
        sp = market().startPrice
        # oqs = filter(o -> true, chains()[dateTarget].chain)
        # TODO: we could allow longs to have bid 0 but, meh
        oqs = filter(oq -> getBid(oq) > 0.0 && (abs(1.0 - sp / getStrike(oq)) <= .1), chains()[dateTarget].chain)
        # TODO: loop through as long as evr strictly increases
        while true
            res = findBest(sp, prob, oqs, lmsPos)
            if !isnothing(res)
                (lms, met, ret) = res
                if met.evr < evr
                    break
                else
                    evr = met.evr
                end
                report(lms, met)
                push!(rets, ret)
                append!(lmsPos, lms)
                # @info "met" getStyle.(lms) getSide.(lms) met.mn met.mx met.evr met.prob
            else
                break
            end
        end
    end
    cret = combineTo(Ret, lmsPos, market().curp)
    Main.save[:cret] = cret
    drawRet(cret; label="c")
    # drawRet!(ret1; label="1")
    met = calcMetrics(probsFor(dateTarget)[1], cret, Bins.binds())
    Main.save[:met] = met
    @info "Result" length(rets) met
end

function findBest(sp, prob, oqs, lmsPos::Vector{LegMeta})
    incCallShort, incPutShort = countType(lmsPos)
    res = (-Inf, nothing)
    res = findBestSpread(sp, prob, oqs, lmsPos::Vector{LegMeta}, res)
    for oq in oqs
        incShort = getStyle(oq) == Style.call ? incCallShort : incPutShort
        lm = to(LegMeta, oq, Side.long)
        res = lyze(sp, prob, lmsPos, [lm], res)
        if incShort
            lm = to(LegMeta, oq, Side.short)
            res = lyze(sp, prob, lmsPos, [lm], res)
        end
    end
    return res[2]
end

function findBestSpread(sp, prob, oqs, lmsPos::Vector{LegMeta}, res)
    calls = filter(isCall, oqs)
    puts = filter(isPut, oqs)
    res = findBestSpreadS(sp, prob, calls, lmsPos::Vector{LegMeta}, res)
    res = findBestSpreadS(sp, prob, puts, lmsPos::Vector{LegMeta}, res)
    return res # scoreMaxC > scoreMaxP ? (resC, scoreMaxC) : (resP, scoreMaxP)
end

function findBestSpreadS(sp, prob, oqs, lmsPos::Vector{LegMeta}, res)
    len = length(oqs)
    for i in 1:len
        for j in i:len
            lms1 = [to(LegMeta, oqs[i], Side.short), to(LegMeta, oqs[j], Side.long)]
            lms2 = [to(LegMeta, oqs[i], Side.long), to(LegMeta, oqs[j], Side.short)]
            res = lyze(sp, prob, lmsPos::Vector{LegMeta}, lms1, res)
            res = lyze(sp, prob, lmsPos::Vector{LegMeta}, lms2, res)
        end
    end
    return res
end

function lyze(sp, prob, lmsPos::Vector{LegMeta}, lms::Vector{LegMeta}, res)
    for lm in lms
        for lmp in lmsPos
            !isConflict(lm, lmp) || (return res)
        end
    end
    # TODO: we could calc the filter directly without creating ret and met
    retOne = combineTo(Ret, lms, sp)
    metOne = calcMetrics(prob, retOne, Bins.binds())
    metOne.prob < 1.0 || return -Inf # Bad data, you generally can't really get a 100% prob position. arbitrage exists, but don't count on getting lucky.
    metOne.loss < 0.0 || return -Inf

    lmsBoth = vcat(lms, lmsPos)
    ret = combineTo(Ret, lmsBoth, sp)
    met = calcMetrics(prob, ret, Bins.binds())
    s = score2(met, ret)
    return s > res[1] ? (s, (lms, met, ret)) : res
end

function score2(met, ret)
    met.mn > -5.1 || return -Inf
    return met.evr
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

function report(lms, met)
    exps = [getExpiration(lms[1])]
    if length(lms) == 1
        ss = str1(lms[1], exps)
    elseif length(lms) == 2
        ss = string(str1(lms[1], exps), " / ", str1(lms[2], exps))
    else
        error("unexpected len ", lms)
    end
    ms = join([string(k, '=', round(met[k]; digits=3)) for k in keys(met)], ' ')
    println(ms, "\t: ", ss)
end

str1(lm, exps) = string(tosh(lm, exps), ' ', bap(lm))

end