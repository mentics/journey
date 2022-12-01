module ChainUtil
using Globals, BaseTypes, SmallTypes, OptionTypes, LegTypes, LegMetaTypes, ChainTypes

function getOqss(oqs::Vector{OptionQuote}, curp::Currency, legsCheck=LegMeta[])::Oqss
    # oqs = filter(oq -> distRatio(getStrike(oq), curp) < Bins.SPAN/2, oqsIn)
    fconl = !isConflict(legsCheck, Side.long)
    fcons = !isConflict(legsCheck, Side.short)
    # fcans = noLimit ? ftrue : canShort(Globals.get(:Strats), curp) # noLimit=false
    fcans = canShort(Globals.get(:Strats), curp)
    # oqsValid = filter(isValid(curp), oqs)
    oqsValid = oqs
    oqsLong = filter(fconl, oqsValid)
    oqsCallLong = filter(SmallTypes.isCall, oqsLong)
    oqsPutLong = filter(SmallTypes.isPut, oqsLong)
    oqsShort = filter(x -> fcons(x) && fcans(x), oqsValid)
    # oqsShort = filter(x -> fcons(x), oqsValid)
    oqsCallShort = filter(SmallTypes.isCall, oqsShort)
    oqsPutShort = filter(SmallTypes.isPut, oqsShort)
    return Styles(Sides(oqsCallLong, oqsCallShort), Sides(oqsPutLong, oqsPutShort))
end
# function getOqss(oqsIn::Vector{OptionQuote}, curp::Currency)::Oqss
#     # oqs = filter(oq -> distRatio(getStrike(oq), curp) < Bins.SPAN/2, oqsIn)
#     oqsValid = Iterators.filter(isValid, oqs)

#     oqsLong = oqsValid # Iterators.filter(isLong, oqsValid)
#     oqsShort = oqsValid # Iterators.filter(isShort, oqsValid)

#     oqsCallLong = collect(Iterators.filter(SmallTypes.isCall, oqsLong))
#     oqsPutLong = collect(Iterators.filter(SmallTypes.isPut, oqsLong))
#     oqsCallShort = collect(Iterators.filter(SmallTypes.isCall, oqsShort))
#     oqsPutShort = collect(Iterators.filter(SmallTypes.isPut, oqsShort))
#     return Styles(Sides(oqsCallLong, oqsCallShort), Sides(oqsPutLong, oqsPutShort))
# end
export filtOqss
function filtOqss(f::Function, oqss::Oqss)
    return Styles(Sides(filter(f, oqss.call.long), filter(f, oqss.call.short)), Sides(filter(f, oqss.put.long), filter(f, oqss.put.short)))
end

function findOqs(oqs, curp::Currency, dists; maxDiff=1.0)
    res = Vector{OptionQuote}(undef, length(dists))
    diffs = fill(Inf, length(dists)) # Vector{Currency}(Inf, length(dists))
    for oq in oqs
        dist = getStrike(oq) - curp
        for i in eachindex(diffs)
            diff = abs(dists[i] - dist)
            if diff < diffs[i]
                diffs[i] = diff
                res[i] = oq
            end
        end
    end
    isnothing(findfirst(x -> x > maxDiff, diffs)) || return nothing
    return res
end

end