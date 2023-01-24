module TestStrat
using SH, BaseTypes, SmallTypes, BackTypes, LegMetaTypes
import LogUtil
import DateUtil:timult,calcRate
import ChainUtil:toSearch, oqsLteCurp
using Pricing

struct ThisStrat{T,C} <: Strat
    params::T
    pricingOpen::Function
    pricingClose::Function
    ctx::C
end

struct Cand{T,N}
    lms::NTuple{N,LegMetaOpen}
    score::Float64
    scoring::T
end

struct Scoring1
    neto::Currency
    risk::Currency
    rate::Float64
end

makeCtx() = (;keep=Vector{Cand{Scoring1,3}}())

makeStrat() = ThisStrat(
    (;balInit=1000),
    pricingOpen,
    pricingClose,
    makeCtx(),
)

function (s::Strat)(ops, tim, chain)
    log("Strat running for ts $(tim.ts)")
    # @show ops tim
    # ops.openTrade()
    ops.checkExit(checkExit, tim, chain)

    keep = s.ctx.keep
    empty!(keep)

    for xpir in chain.xpirs[8:20]
        tmult = timult(tim.date, xpir)
        search = toSearch(chain.under.under, chain.xsoqs[xpir].put)
        findEntry(ops, keep, tim, search, tmult)
        # if !isempty(keep)
        #     println("Found: ", keep[1])
        # end
    end
    if !isempty(keep)
        ops.openTrade(keep[1].lms, keep[1].scoring.neto, 1, "scored")
    end
end

function checkExit(tradeOpen::TradeBTOpen, tim, chain)::Union{Nothing,String}
    println("checkExit")
end

function findEntry(ops, keep, tim, search, tmult)
    # global keepSearch = search
    oqs = oqsLteCurp(search, .9)
    # log("findEntry $(length(oqs))")
    itr1 = Iterators.reverse(eachindex(oqs)[3:end])
    for i1 in itr1
        itr2 = i1-1:-1:2
        for i2 in itr2
            itr3 = i2-1:-1:1
            for i3 in itr3
                # @show i1 i2 i3
                oq1 = oqs[i1]
                oq2 = oqs[i2]
                oq3 = oqs[i3]
                lms = (LegMetaOpen(oq3, Side.long), LegMetaOpen(oq2, Side.long), LegMetaOpen(oq1, Side.short))
                r = score(ops, tim, lms, tmult)
                if !isnothing(r) && (isempty(keep) || r[1] > keep[end].score)
                    # TODO: not optimized
                    push!(keep, Cand(lms, r[1], r[2]))
                    sort!(keep; rev=true, by=x->x.score)
                    length(keep) <= 10 || pop!(keep)
                end
            end
        end
    end
end

function score(ops, tim, lms, tmult)
    # global keepLms = lms
    # log("Scoring")
    neto = Pricing.bapFast(lms, .2)
    neto > 0.02 || return nothing
    risk = Pricing.calcMargin(lms)
    @assert risk > CZ "risk was <= 0"
    # println(tim.date, ' ',getExpir(lms))
    # TODO: the following assumes we want it to expire worthless
    # rate = calcRate(tim.date, getExpir(lms), neto, risk)
    rate = calcRate(tmult, neto, risk)
    # log(lms)
    # error("stop")
    return (rate, Scoring1(neto, risk, rate))
end

pricingOpen(lmso::NTuple{3,LegMetaOpen}) = bap(lmso, .2)
pricingClose(lmsc::NTuple{3,LegMetaClose}) = bap(lmsc, .2)

log(args...) = LogUtil.logit(:backtest, args...)

end
