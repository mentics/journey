module CmdStrats
using Dates
using SH, BaseTypes, QuoteTypes, LegTypes, LegMetaTypes, StratTypes, RetTypes, ProbTypes
using Globals, Bins, BaseUtil, DateUtil, CollUtil, ProbUtil, Between, Scoring
using Strats, Rets, StratGen, RunStrats
using ProbHist, Markets, Expirations, Chains, Positions
using Trading, CmdUtil
using OutputUtil, DrawStrat
using StoreOrder

export ana, an, sortar, sortar2, sa, ar, arv, arl, ret, comp
export dr, drf, adr, adrf, dra, ret, retf
export ctx, curStrat, curRet, probs, pvals, ivs

function comp(i::Int)
    retStart = curRet()
    retAdd = combineTo(Ret, ar(i))
    retBoth = combineTo(Ret, collect(Iterators.flatten((ar(i), curStrat()))))
    ind = 0
    for prob in probs()
        ind += 1
        if isnothing(retStart)
            add = hereMetrics(prob, retAdd)
            data = [(;name="combi", add...)]
            # data = [(;name=n, nt...) for (n, nt) in zip(["start$(ind)", "add$(ind)", "both$(ind)"], [start, add, both])]
        else
            start = hereMetrics(prob, retStart)
            add = hereMetrics(prob, retAdd)
            both = hereMetrics(prob, retBoth)
            data = [(;name=n, nt...) for (n, nt) in zip(["start$(ind)", "add$(ind)", "both$(ind)"], [start, add, both])]
        end
        pretyble(data) # TODO: create Tables impl for tuple of namedtuples
    end
    return
end

hereMetrics(pv, r) = Scoring.calcMetrics(pv, r)

# draw(CmdStrats.lastCtx[].probs.ppos.vals)
# draw(CmdStrats.lastCtx[].probs.pposInv.vals)
# draw(getVals(CmdStrats.lastPosRet[]))
# draw(getVals(CmdStrats.retToProb(CmdStrats.lastPosRet[])))
# draw(getVals(CmdStrats.invert(CmdStrats.retToProb(CmdStrats.lastPosRet[]))))
# TODO: handle small values and if everything is above 0. Maybe normalize to 0 to 1 and used bottom half?
# retToProb(ret::Ret) = Prob(getCenter(ret), normalize!(map(x -> x < 0.0 ? 0.0 : 1.0, getVals(ret))))
# retToInvProb(ret::Ret) = Prob(getCenter(ret), normalize!(map(x -> x < 0.1 ? 1.0 : 0.0, getVals(ret))))
# invert(p::Prob) = Prob(getCenter(p), invert(getVals(p)))
# invert(v::Vector{Float64}) = normalize!(map(x -> x === 0.0 ? 1.0 : 0.0, v))

function makeProbs(numDays::Int, targetDate::Date, sp::Currency)::Tuple
    # phOrig = probHist(sp, round(Int, 1.5 * (3 + numDays)))
    pnd = probsNormDist(sp, calcIvsd(targetDate, 1.1 + .25 * numDays * .05))
    # pnd = probsNormDist(sp, calcIvsd(targetDate))
    return (pnd, pnd)
    # ph = Prob(getCenter(phOrig), smooth(getVals(phOrig)))
    # pflat = probFlat(Float64(sp), pnd[1]/2)
    # pflat = probRoof(Float64(sp), pnd[1]/2)
    # return (pnd, pflat)
    # pflat = probFlat(getCenter(pnd), pnd.vals[1])
    # return (pflat, ph)
    # return (pnd, ph)
    # pshort = probMid(ph, binMin(), 1.0)
    # plong = probMid(ph, 1., binMax())
    # pmid = probMid(ph, .5*(1.0+binMin()), .5*(1.0+binMax()))
    # ppos = isnothing(lastPosRet[]) ? nothing : retToProb(lastPosRet[])
    # pposInv = isnothing(lastPosRet[]) ? nothing : invert(ppos)
    # pposHyb = Prob(getCenter(ph), normalize!(getVals(ph) .+ (getVals(pposInv) .* 2)))
    # return (ph,pnd)
end

export aa
aa(ex) = ana((ex:ex+2)...)
ana(exs::Int...; kws...) = an(exs...; kws..., maxRun=0)
ana(exps::Date...; kws...) = an(exps...; kws..., maxRun=0)
an(exs::Int...; kws...) = an(getindex.(Ref(expirs()), exs)...; kws...)
function an(exps::Date...; maxRun::Int=120, keep::Int=100, nthreads::Int=Threads.nthreads(),
            noPos::Bool=false, lmsAdd::Union{Nothing,Vector{LegMeta}}=nothing, lmsPos::Union{Nothing,Vector{LegMeta}}=nothing,
            getProbs=makeProbs, scorer=nothing, headless=false,
            sprFilt=nothing, addDays::Int=0)::Int
    Globals.set(:anRunLast, now(UTC))
    @assert issorted(exps)
    # exps = getindex.(Ref(expirs()), exs)
    targetDate = first(exps)
    lastExp[] = targetDate
    mkt = market()
    sp = mkt.startPrice
    chs = chains()
    # global lastPosStrat[] = noPos ? Vector{LegRet}() : (isnothing(posStrat) ? calcPosStrat(targetDate, sp, Globals.get(:vtyRatio), addPos) : posStrat)
    global lastPosStrat[] = noPos ? Vector{LegRet}() :
            (isnothing(lmsPos) ? calcPosStrat(targetDate, sp, Globals.get(:vtyRatio), lmsAdd) :
                    tos(LegRet, lmsPos, targetDate, sp, Globals.get(:vtyRatio)))
    global lastPosRet[] = (noPos || isempty(lastPosStrat[])) ? nothing : combineTo(Ret, lastPosStrat[])

    numDays = mktNumDays(targetDate) + addDays
    probs = getProbs(numDays, targetDate, sp)

    legs = vcat(getLeg.(positions()), getLeg.(lastPosStrat[]), getLeg.(queryLegOrders(today())))
    allSpreads2 = allSpreads(chs, isConflict(legs), (sp, mkt.curp), exps)
    !isnothing(sprFilt) && (allSpreads2 = map(v->filter(sprFilt, v), allSpreads2))
    global lastSpreads2[] = allSpreads2
    len1, len2 = length.(allSpreads2)
    if maxRun == 0; maxRun = binomial(len1, 2) + binomial(len2, 2) + len1 * len2 end
    resetCountsScore()
    ctx = makeCtx(coal(scorer, calcScore1), probs, numDays; maxRun, keep, posRet=lastPosRet[], nthreads, sp)
    @info "ctx" keys(ctx)

    @info "RunStrats running" maxRun keep nthreads exps sum(length, allSpreads2) sp numDays
    @time strats = runStrats(allSpreads2, ctx)
    showCountsScore()
    global lastRes[] = strats
    global lastCtx[] = ctx
    sortar(byScore)
    global lastView[] = copy(strats)
    if !headless
        sa()
        isempty(lastView[]) || (isempty(lastPosStrat[]) ? dr(1) : dra(1))
        isempty(lastView[]) || isempty(lastPosStrat[]) || comp(1)
    end
    return length(lastView[])
end

# isConflict(opt::Option, side::Side.T) = !isnothing(conflicter(opt, side))
# conflicter(opt::Option, side::Side.T) = findfirst(pos -> isConflict(opt, side, pos), positions())
# isConflict(opt::Option, side::Side.T, pos::Position) = opt == getOption(pos) && side != getSide(pos)

# Had to create own isLess for floats because the built in isless has NaN greater than everything but we want it less
isl(x1::Float64, x2::Float64)::Bool = isnan(x1) ? true : (isnan(x2) ? false : isless(x1, x2))

# sortar(by::Function)::Nothing = ( sortExp!(by, lastRes[]; rev=true, lt=isl) ; nothing )
# sortar(by::Function)::Nothing = ( sort!(lastRes[]; rev=true, by, lt=isl) ; nothing )
# function sortar1(by::Function)::Nothing
#     useBy = isempty(lastPosStrat[]) ? by : s -> by(withPosStrat(s))
#     sort!(lastRes[]; rev=true, by=useBy, lt=isl)
#     lastView[] = copy(lastRes[])
#     return
# end
# function sortar2(by::Function)::Nothing
#     useBy = isempty(lastPosStrat[]) ? by : s -> by(s, withPosStrat(s))
#     sort!(lastRes[]; rev=true, by=useBy, lt=isl)
#     lastView[] = copy(lastRes[])
#     return
# end
function sortar(by::Function)::Nothing
    useBy = c -> by(lastCtx[], combineTo(Vals, c), combineTo(Vals, withPosStrat(c)), lastPosRet[])
    # useBy = isempty(lastPosStrat[]) ? by : s -> by(s, withPosStrat(s))
    sort!(lastRes[]; rev=true, by=useBy, lt=isl)
    lastView[] = copy(lastRes[])
    return
end
SH.getVals(::Nothing) = nothing
export sortScore
sortScore()::Nothing = sortar(byScore)

# TODO: optimize this (so much new vector)
withPosStrat(s::Strat)::Vector{LegRet} = vcat(collect(s), lastPosStrat[])
function sa(cnt::Int=20)::Nothing
    # insert!(tups, 1, toTuple(nothing, lastPosStrat[]))
    # toTuple.(withPosStrat.(view)
    tups = analysisResults(cnt)
    pretyble(tups; rowcol=true, widths=tupleWidths())
    return
end
function analysisResults(cnt::Int=20)::Vector{NamedTuple}
    lastView[] = copy(lastRes[])
    view = lastView[][1:min(cnt,end)]
    tups = [toTuple(s, withPosStrat(s)) for s in view]
    return tups
end

ar(i::Int) = i == 0 ? lastPosStrat[] : lastView[][i]
arv(i::Int)= combineTo(Vals, i == 0 ? lastPosRet[] : lastView[][i])
arl(i::Int)= tos(LegMeta, ar(i))

ret(i::Int) = i == 0 ? lastPosRet[] : combineTo(Ret, withPosStrat(ar(i))) # combineRets(getRets(withPosStrat(ar(i))))
retf(i::Int) = combineTo(Ret, ar(i))
dr(i::Int) = locDraw(ret(i), i)
drf(i::Int) = locDraw(retf(i), string(i)*'f')
adr(i::Int) = locDraw!(ret(i), i)
adrf(i::Int) = locDraw!(retf(i), string(i)*'f')
dra(i::Int) = ( dr(0) ; adrf(i) ; adr(i) )
locDraw(x, label) = ( drawRet(x, probs(), market().curp, string(label)) ; return )
locDraw!(x, label) = ( drawRet!(x, string(label)) ; return )

ctx() = lastCtx[]
curStrat() = lastPosStrat[]
curRet() = lastPosRet[]
# curVals() = getVals(curRet())
probs() = lastCtx[].probs
pvals() = map(x->getVals(x), probs())

#region Local
const lastExp = Ref{Date}()
const lastRes = Ref{Vector{Strat}}(Vector{Strat}())
const lastView = Ref{Vector{Strat}}(Vector{Strat}())
const lastCtx = Ref{Any}()
const lastSpreads2 = Ref{Spreads2}()
const lastPosStrat = Ref{Vector{LegRet}}(Vector{LegRet}())
const lastPosRet = Ref{Union{Nothing,Ret}}(nothing)

# TODO: move toTuple?
using Shorthand, CalcUtil
tupleWidths() = [0,0,0,0,0,0,0,0,0,48,30,10]
function toTuple(s::Union{Nothing,Strat}, lrs::Vector{LegRet})
    exps = unique!(sort!(collect(getExpiration.(s))))
    strikes = legsTosh(s, exps) # join(map(l -> "$(first(string(side(l))))$(s(strike(l), 1))$(first(string(style(l))))@$(searchsortedfirst(exps, expiration(l)))", legs(ar)), " / ")
    length(lrs) > length(s) && (strikes *= " + cur")
    ret = combineTo(Ret, lrs)
    met = hereMetrics(probs()[1], ret)
    met2 = hereMetrics(probs()[2], ret)
    # TODO: calc breakevens
    kel = NaN # calcKelly(pv, ret)
    score = byScore(lastCtx[], combineTo(Vals, s), getVals(lastPosRet[]), ret)
    pnl = extrema(getVals(ret))
    netOpen=!isnothing(s) ? bap(tos(LegMeta, s)) : 0.0
    return (;prob=met.prob, kel, ev=met.ev, evr=met.evr, ev2=met2.ev, evr2=met2.evr, pnl, netOpen, legs=strikes, expir=exps, score)
end
#endregion

export showScore, scoreFor
function showScore(i::Int)
    s = ar(i)
    lrs = withPosStrat(s)
    valsBoth = combineTo(Vals, lrs)
    # lastCtx[].calcScore(lastCtx[], nothing, combineTo(Vals, s), getVals(lastPosRet[]), vals, true)
    scoreFor(combineTo(Vals, s), getVals(lastPosRet[]), valsBoth, true)
end

function scoreFor(lms::Coll{LegMeta})
    scoreFor(collect(lms), minimum(getExpiration.(lms)), market().startPrice, getvr())
end
function scoreFor(lms::Vector{LegMeta}, expr::Date, args...)
    valsCombi = getVals(combineTo(Ret, lms, expr, args...))
    lastCtx[].calcScore(lastCtx[], nothing, valsCombi, nothing, valsCombi, show)
end
scoreFor(valsCombi, valsPos, valsBoth, show=false) = lastCtx[].calcScore(lastCtx[], nothing, valsCombi, valsPos, valsBoth, show)

end