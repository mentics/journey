module TestStrat
import LogUtil
using BackTypes
import DateUtil:calcRate
import ChainUtil:toSearch, oqsLteCurp

strat() = Strat3(
    (;balInit=1000),
    pricingOpen,
    pricingClose,
)

struct Strat3{T} <: Strat
    params::T
    pricingOpen::Function
    pricingClose::Function
end

function (s::Strat3)(ops, tim, chain)
    log("Strat running for ts $(tim.ts)")
    # @show ops tim
    # ops.openTrade()
    ops.checkExit(checkExit, tim, chain)
    for xpir in chain.xpirs
        search = toSearch(chain.xsoqs[xpir].put)
        res = findEntry(ops, tim, search)
        # if !isnothing(res)
        #     lms
        # end
    end
end

function checkExit(tradeOpen::TradeBTOpen, tim, chain)::Union{Nothing,String}
    println("checkExit")
end

function findEntry(ops, tim, search)
    oqs = oqsLteCurp(search, .9)
    itr1 = Iterators.reverse(eachindex(oqs))
    for i1 in itr1
        itr2 = Iterators.drop(itr1, i1)
        for i2 in itr2
            for i3 in Iterators.drop(itr2, i2)
                oq1 = oqs[i1]
                oq2 = oqs[i2]
                oq3 = oqs[i3]
                lms = (LegMetaOpen(oq3, Side.long), LegMetaOpen(oq2, Side.long), LegMetaOpen(oq1, Side.short))
                score(ops, lms)
            end
        end
    end
end

function score(ops, lms)::Union{Nothing,Float64}
    log("Scoring")
    neto = ops.pricingOpen(lms)
    neto > 0.02 || return nothing
    calcRate()
    log(lms)
    error("stop")
end

pricingOpen(lmso::Coll{LegMetaOpen}) = bap(lmso, .2)
pricingClose(lmsc::Coll{LegMetaClose}) = bap(lmsc, .2)

log(args...) = LogUtil.logit(:backtest, args...)

end
