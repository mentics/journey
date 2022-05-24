module RunStrats
using Dates, ThreadPools
using LogUtil, CollUtil, ThreadUtil, CalcUtil
using SH, Bins, Rets, StratTypes, RetTypes, LegTypes
using Strats
using Scoring
using SmallTypes, LegMetaTypes # Just for test filter

export runStrats, makeCtx

const VECF_EMPTY = Vector{Float64}()

function makeCtx(calcScore, probs, numDays::Int; nthreads::Int=12, maxRun::Int=120, keep::Int=10000, posRet::Union{Nothing,Ret}, kws...)
    maxRun < nthreads && error("maxRun should be more than nthreads", maxRun, nthreads)
    keep < nthreads && error("keep should be more than nthreads", keep, nthreads)
    # cntEst = binomial(sum(length, legs4), 4) # TODO: too high
    # maxRun = min(maxRun, cntEst)
    thrMaxRun = div(maxRun, nthreads) + 1
    if isnothing(posRet)
        baseScore = -Inf ; metp = nothing ; metp2 = nothing
        return (;
            calcScore, probs, maxRun, baseScore, numDays, posRet, kws...,
            threads = [makeThreadCtx(div(keep, nthreads), thrMaxRun, baseScore) for _ in 1:nthreads]
        )
    else
        posVals = getVals(posRet)
        baseScore = calcScore((;probs, numDays, kws...), nothing, VECF_EMPTY, posVals, posRet)
        # @info "Setting baseScore" baseScore isnothing(posRet)
        metp = calcMetrics(probs[1], posRet)
        metp2 = calcMetrics(probs[2], posRet)
        return (;
            calcScore, probs, maxRun, baseScore, numDays, posRet, metp, metp2, kws...,
            threads = [makeThreadCtx(div(keep, nthreads), thrMaxRun, baseScore) for _ in 1:nthreads]
        )
    end
end

function runStrats(allSpreads2::Spreads2, ctx)
    resetStats()
    catMaxLen = maximum(length, allSpreads2)
    nthreads = length(ctx.threads)
    @sync for i in 1:nthreads
        # TODO: is this ok to run on 1-x? will the main thread move?
        # threadId = Threads.nthreads() - i + 1
        threadId = i
        range = i:nthreads:catMaxLen
        @tspawnat threadId runThread(ctx, allSpreads2, threadId, range)
    end
    keptCnt = sum(Iterators.map(tc->length(tc.res), ctx.threads))
    procCnt = sum(Iterators.map(tc->tc.procCnt[], ctx.threads))
    if procCnt >= ctx.maxRun
        @log warn "Truncated processing" procCnt ctx.maxRun notFinite.count lowScore.count invalidCombi.count keptCnt
    else
        @log info "Processing completed" procCnt ctx.maxRun notFinite.count lowScore.count invalidCombi.count keptCnt
    end
    # TODO: Remove above after sure @sync waits for all to finish
    res = Vector{Combi}(undef,keptCnt)
    i = 0
    for thrid in 1:nthreads
        t = ctx.threads[thrid]
        # @info "For thread" thrid first(t.scores) last(t.scores)
        for v in ctx.threads[thrid].res
            i += 1
            res[i] = v
        end
    end
    return res
end

#region Local
makeThreadCtx(thrKeep::Int, thrMaxRun::Int, baseScore::Float64) = (;
    thrMaxRun,
    bufRet1 = Bins.empty(),
    bufRet2 = Bins.empty(),
    scores = fill(baseScore, thrKeep),
    res = Vector{Combi}(undef, round(Int, 10*log(thrMaxRun) * thrKeep)), # multiple because ramp up makes it go over TODO: this mult is too big, maybe just for test weirdness?
    procCnt = Ref{Int}()
)

function runThread(ctx, all::Spreads2, threadId::Int, itrInds::StepRange{Int,Int})
    cnt = 0
    resIndex = 0
    tctx = ctx.threads[threadId]
    try
        for i in itrInds
            resIndex, cnt = runAllStrats((ctx, tctx), all, i, (resIndex, cnt))
            cnt >= tctx.thrMaxRun && break
        end
    catch e
        syncOut(e)
    finally
        # @info "Thread processed" threadId cnt resIndex
        tctx.procCnt[] = cnt
        resize!(tctx.res, resIndex)
    end
end

function runAllStrats((ctx, tctx), sprs::Spreads2, index::Int, (resIndex, cnt)::Tuple{Int,Int})::Tuple{Int,Int}
    sprs1 = sprs[1] ; sprs2 = sprs[2]
    # run 1 to all above 1
    sprs = sprs1
    len = length(sprs)
    # for i in 1:len
    index < len && for i in index
        for j in i+1:len
            cnt += 1
            c = (sprs[i][1], sprs[i][2], sprs[j][1], sprs[j][2])
            if procStrat((ctx, tctx), c)
                resIndex += 1
                sc = sortTuple(getStrike, c)
                # if sc in tctx.res[resIndex]

                # end
                tctx.res[resIndex] = sc
            end
            cnt == tctx.thrMaxRun && return (resIndex, cnt)
        end
    end

    # run 2 to all above 2
    sprs = sprs2
    len = length(sprs)
    # for i in 1:len
    index < len && for i in index
        for j in i+1:len
            cnt += 1
            c = (sprs[i][1], sprs[i][2], sprs[j][1], sprs[j][2])
            if procStrat((ctx, tctx), c)
                resIndex += 1
                tctx.res[resIndex] = sortTuple(getStrike, c)
            end
            cnt == tctx.thrMaxRun && return (resIndex, cnt)
        end
    end

    # run 1 to all 2
    index < length(sprs1) && for i in index
        x1 = sprs1[i]
        for x2 in sprs2
            cnt += 1
            c = (x1[1], x1[2], x2[1], x2[2])
            if procStrat((ctx, tctx), c)
                resIndex += 1
                tctx.res[resIndex] = sortTuple(getStrike, c)
            end
            cnt == tctx.thrMaxRun && return (resIndex, cnt)
        end
    end
    return (resIndex, cnt)
end

# # AllLegs4 is structured: (callsLong, putsLong, callsShort, putsShort)
# function runAllStrats(cnt::Int, ctx, all::AllLegs4, index::Int, resIndex::Int)::Tuple{Int,Int}
#     resIndex, cnt = runStratsWith((resIndex, cnt), ctx, index, all[1], index, all[3], all[4])
#     resIndex, cnt = runStratsWith((resIndex, cnt), ctx, index, all[2], index, all[3], all[4])
#     resIndex, cnt = runStratsWith((resIndex, cnt), ctx, index, all[3], index+1, all[1], all[2]) # index+1 to avoid duplicates
#     resIndex, cnt = runStratsWith((resIndex, cnt), ctx, index, all[4], index+1, all[1], all[2])
#     return (resIndex, cnt)
# end

# function runStratsWith((resIndex, cnt), (ctx, tctx), index1::Int, twoFromFirst::Vector{LegRet}, index2::Int, takeTwo::Vector{LegRet}, orTakeTwo::Vector{LegRet})
#     index1 > length(twoFromFirst) && return (resIndex, cnt)
#     cnt >= tctx.thrMaxRun && return (resIndex, cnt)
#     one = twoFromFirst[index1]
#     for i1 in (index1+1):length(twoFromFirst)
#         for i2 in index2:length(takeTwo)
#             for i3 in (i2+1):length(takeTwo)
#                 cnt += 1
#                 c = (one, twoFromFirst[i1], takeTwo[i2], takeTwo[i3])
#                 if procStrat((ctx, tctx), c)
#                     resIndex += 1
#                     tctx.res[resIndex] = sortTuple(getStrike, c)
#                 end
#                 cnt == tctx.thrMaxRun && return (resIndex, cnt)
#             end
#         end
#         for i2 in index2:length(orTakeTwo)
#             for i3 in (i2+1):length(orTakeTwo)
#                 cnt += 1
#                 c = (one, twoFromFirst[i1], orTakeTwo[i2], orTakeTwo[i3])
#                 if procStrat((ctx, tctx), c)
#                     resIndex += 1
#                     tctx.res[resIndex] = sortTuple(getStrike, c)
#                 end
#                 cnt == tctx.thrMaxRun && return (resIndex, cnt)
#             end
#         end
#     end
#     return (resIndex, cnt)
# end

const notFinite = Atomic(0)
const lowScore = Atomic(0)
const invalidCombi = Atomic(0)
function resetStats()
    @atomic notFinite.count = 0
    @atomic lowScore.count = 0
    @atomic invalidCombi.count = 0
end

function procStrat((ctx, tctx), combi::Combi)::Bool
    testFilter(combi) || return false
    isValidCombi(combi) || ( @atomic invalidCombi.count += 1 ; return false )
    buf1, buf2 = (tctx.bufRet1, tctx.bufRet2)
    getVals!(buf1, combi)
    useBuf2 = isnothing(ctx.posRet) ? buf1 : getVals!(buf2, combi, getVals(ctx.posRet))
    score = ctx.calcScore(ctx, tctx, buf1, useBuf2, ctx.posRet)
    if isfinite(score)
        prinsert!(tctx.scores, score) || ( @atomic lowScore.count += 1 ; return false )
    else
        @atomic notFinite.count += 1
        return false
    end
    return true
end

function isValidCombi(combi::Combi)
    res = isConflict(combi[1], combi[3]) ||
        isConflict(combi[1], combi[4]) ||
        isConflict(combi[2], combi[3]) ||
        isConflict(combi[2], combi[4])
    # TODO: consider counting how many there are and maybe fixing earlier
    # if res
    #     @error "invalid combi" getindex.(combi, 1)
    #     error("stop")
    # end
    return !res
end

const LK2 = ReentrantLock()
function syncOut(e)
    runSync(LK2) do
        showerror(stderr, e, catch_backtrace())
        println(stderr)
    end
end

const TestCombis = Vector{Vector{Leg}}()
testCombi(c::Combi) = ( empty!(TestCombis) ; push!(TestCombis, collect(getLeg.(tos(LegMeta, c)))) )
addTestCombi(c::Combi) = ( push!(TestCombis, collect(getLeg.(tos(LegMeta, c)))) )
testClear() = empty!(TestCombis)
function testFilter(c::Combi)::Bool
    return true
    # TODO: inefficient, and this is temp code to test matching up holes
    c = sortTuple(getStrike, c)
    issorted(c; by=getStrike) || error("combis not sorted")
    return getSide.(c) == (Side.short, Side.long, Side.long, Side.short)
    isempty(TestCombis) && return true
    legsCombi = sort!(collect(getLeg.(tos(LegMeta, c))), by=getStrike)
    for legs in TestCombis
        legsCombi == legs && return true
    end
    return false
    # # "s446.0c@1 / l447.0c@1 / l452.0c@2 / s454.0c@1"
    # legsTest = [Leg(Option(Style.call, 2022-04-18, 446.000), 1.0, SmallTypes.Side.short),
    #             Leg(Option(Style.call, 2022-04-18, 447.000), 1.0, SmallTypes.Side.long),
    #             Leg(Option(Style.call, 2022-04-20, 452.000), 1.0, SmallTypes.Side.long),
    #             Leg(Option(Style.call, 2022-04-18, 454.000), 1.0, SmallTypes.Side.short)]
end
#endregion

end
