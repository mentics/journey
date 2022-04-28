module CmdStrats
using Dates
using SH, BaseTypes, QuoteTypes, LegMetaTypes, StratTypes, RetTypes, ProbTypes
using Globals, Bins, BaseUtil, DateUtil, CollUtil, ProbUtil, Between, Scoring
using Strats, Rets, StratGen, RunStrats
using ProbHist, Markets, Expirations, Chains, Positions
using Trading, CmdUtil
using OutputUtil, DrawStrat
using StoreOrder

export ana, an, sortar, sortar2, sa, ar, arv, ret, comp
export dr, drf, adr, adrf, dra, ret, retf
export ctx, curStrat, curRet, probs, pvals, ivs
export atmiv, setvr, getvr

function comp(i::Int)
    valsStart = combineTo(Vals, curStrat())
    valsAdd = combineTo(Vals, ar(i))
    valsBoth = combineTo(Vals, collect(Iterators.flatten((ar(i), curStrat()))))
    ind = 0
    for pvals in map(getVals, probs())
        ind += 1
        # labels = [
        #     [:start, :add, :both],
        #     [:startFlat, :addFlat, :bothFlat]
        # ]
        start = calcMetrics(pvals, valsStart)
        add = calcMetrics(pvals, valsAdd)
        both = calcMetrics(pvals, valsBoth)
        data = [(;name=n, nt...) for (n, nt) in zip(["start$(ind)", "add$(ind)", "both$(ind)"], [start, add, both])]
        pretyble(data) # TODO: create Tables impl for tuple of namedtuples
    end
    return
end

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
    phOrig = probHist(sp, numDays)
    ph = Prob(getCenter(phOrig), smooth(getVals(phOrig)))
    pnd = probsNormDist(sp, calcIvsd(targetDate, 1.05))
    # pshort = probMid(ph, binMin(), 1.0)
    # plong = probMid(ph, 1., binMax())
    # pmid = probMid(ph, .5*(1.0+binMin()), .5*(1.0+binMax()))
    # ppos = isnothing(lastPosRet[]) ? nothing : retToProb(lastPosRet[])
    # pposInv = isnothing(lastPosRet[]) ? nothing : invert(ppos)
    # pflat = probFlat(ph)
    # pposHyb = Prob(getCenter(ph), normalize!(getVals(ph) .+ (getVals(pposInv) .* 2)))
    # return (pnd, ph)
    return (ph,pnd)
end

ana(exs...; kws...) = an(exs...; kws..., maxRun=0)
function an(exs...; maxRun::Int=120, keep::Int=1000, nthreads::Int=Threads.nthreads(),
            noPos::Bool=false, addPos=nothing, posStrat::Union{Nothing,Vector{LegRet}}=nothing,
            getProbs=makeProbs, scorer=nothing, headless=false,
            sprFilt=nothing, addDays::Int=0)::Nothing
    Globals.set(:anRunLast, now(UTC))
    exs == () && (exs = (1,2))
    @assert issorted(exs)
    exps = getindex.(Ref(expirs()), exs)
    targetDate = first(exps)
    lastExp[] = targetDate
    mkt = market()
    sp = mkt.startPrice
    chs = chains()
    global lastPosStrat[] = noPos ? Vector{LegRet}() : (isnothing(posStrat) ? calcPosStrat(targetDate, sp, Globals.get(:vtyRatio), addPos) : posStrat)
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
    ctx = makeCtx(coal(scorer, calcScore1), probs, numDays; maxRun, keep, posRet=lastPosRet[], nthreads)

    @info "RunStrats running" maxRun keep nthreads exps sum(length, allSpreads2) sp numDays
    @time strats = runStrats(allSpreads2, ctx)
    showCountsScore()
    global lastRes[] = strats
    global lastCtx[] = ctx
    sortar(byEvr(probs[1]))
    global lastView[] = copy(strats)
    if !headless
        sa()
        isempty(lastView[]) || (isempty(lastPosStrat[]) ? dr(1) : dra(1))
        isempty(lastView[]) || isempty(lastPosStrat[]) || comp(1)
    end
    return
end

# isConflict(opt::Option, side::Side.T) = !isnothing(conflicter(opt, side))
# conflicter(opt::Option, side::Side.T) = findfirst(pos -> isConflict(opt, side, pos), positions())
# isConflict(opt::Option, side::Side.T, pos::Position) = opt == getOption(pos) && side != getSide(pos)

# Had to create own isLess for floats because the built in isless has NaN greater than everything but we want it less
isl(x1::Float64, x2::Float64)::Bool = isnan(x1) ? true : (isnan(x2) ? false : isless(x1, x2))

# sortar(by::Function)::Nothing = ( sortExp!(by, lastRes[]; rev=true, lt=isl) ; nothing )
# sortar(by::Function)::Nothing = ( sort!(lastRes[]; rev=true, by, lt=isl) ; nothing )
function sortar(by::Function)::Nothing
    useBy = isempty(lastPosStrat[]) ? by : s -> by(withPosStrat(s))
    sort!(lastRes[]; rev=true, by=useBy, lt=isl)
    lastView[] = copy(lastRes[])
    return
end
function sortar2(by::Function)::Nothing
    useBy = isempty(lastPosStrat[]) ? by : s -> by(s, withPosStrat(s))
    sort!(lastRes[]; rev=true, by=useBy, lt=isl)
    lastView[] = copy(lastRes[])
    return
end

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
probs() = lastCtx[].probs
pvals() = map(x->getVals(x), probs())
getvr() = Globals.get(:vtyRatio)
setvr(vr::Float64) = Globals.set(:vtyRatio, vr)

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
tupleWidths() = [0,0,0,0,0,0,0,0,48,30] # [0,0,0,0,30,0,0,0,48,0] # TODO: update this
# function toTuple(s::Strat)
#     probsVals = getVals(first(lastCtx[].probss))
#     lms = getLegMetas(s)
#     exps = unique!(sort!(getExpiration.(lms)))
#     strikes = legsTosh(lms, exps) # join(map(l -> "$(first(string(side(l))))$(s(strike(l), 1))$(first(string(style(l))))@$(searchsortedfirst(exps, expiration(l)))", legs(ar)), " / ")
#     vals = getVals(s)
#     evRat = calcEvPnl(probsVals, vals)
#     # TODO: calc breakevens
#     # TODO: calc prob profit
#     return (evRatio=evRat, bes="", pnl=extrema(vals), netOpen=getNetOpen(s), probProfit="", legs=strikes, expir=exps, kel)
# end
function toTuple(s::Union{Nothing,Strat}, lrs::Vector{LegRet})
    pv = getVals(probs()[1])
    pv2 = getVals(probs()[2])
    exps = unique!(sort!(collect(getExpiration.(s))))
    strikes = legsTosh(s, exps) # join(map(l -> "$(first(string(side(l))))$(s(strike(l), 1))$(first(string(style(l))))@$(searchsortedfirst(exps, expiration(l)))", legs(ar)), " / ")
    length(lrs) > length(s) && (strikes *= " + cur")
    vals = combineTo(Vals, lrs)
    met = calcMetrics(pv, vals)
    met2 = calcMetrics(pv2, vals)
    # TODO: calc breakevens
    kel = calcKelly(pv, vals)
    return (;prob=met.prob, kel, ev=met.ev, evr=met.evr, ev2=met2.ev, evr2=met2.evr, bes="", pnl=extrema(vals), netOpen=!isnothing(s) ? getNetOpen(s) : 0.0, legs=strikes, expir=exps)
end
#endregion

end