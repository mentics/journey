module ChainUtil
using Globals, BaseTypes, SmallTypes, OptionTypes, LegTypes, LegMetaTypes, ChainTypes

# function getOqss(oqs::Vector{OptionQuote}, curp::Currency, legsCheck=LEGS_EMPTY)::Oqss
#     # oqs = filter(oq -> distRatio(getStrike(oq), curp) < Bins.SPAN/2, oqsIn)
#     fconl = !isConflict(legsCheck, Side.long)
#     fcons = !isConflict(legsCheck, Side.short)
#     # fcans = noLimit ? ftrue : canShort(Globals.get(:Strats), curp) # noLimit=false
#     fcans = canShort(Globals.get(:Strats), curp)
#     # oqsValid = filter(isValid(curp), oqs)
#     oqsValid = oqs
#     oqsLong = filter(fconl, oqsValid)
#     oqsCallLong = filter(SmallTypes.isCall, oqsLong)
#     oqsPutLong = filter(SmallTypes.isPut, oqsLong)
#     oqsShort = filter(x -> fcons(x) && fcans(x), oqsValid)
#     # oqsShort = filter(x -> fcons(x), oqsValid)
#     oqsCallShort = filter(SmallTypes.isCall, oqsShort)
#     oqsPutShort = filter(SmallTypes.isPut, oqsShort)
#     return Styles(Sides(oqsCallLong, oqsCallShort), Sides(oqsPutLong, oqsPutShort))
# end

# function getOqss(oqs, curp::Currency, legsCheck=LEGS_EMPTY)::Oqss
#     lc = Vector{OptionQuote}()
#     sc = Vector{OptionQuote}()
#     lp = Vector{OptionQuote}()
#     sp = Vector{OptionQuote}()
#     oqss = Styles(Sides(lc, sc), Sides(lp, sp))
#     fConL = !isConflict(legsCheck, Side.long)
#     fConS = !isConflict(legsCheck, Side.short)
#     fCanS = canShort(Globals.get(:Strats), curp)
#     # oqs = Iterators.filter(isValid(curp), oqs)
#     for oq in oqs
#         canL = fConL(oq)
#         canS = fCanS(oq) && fConS(oq)
#         if SmallTypes.isCall(oq)
#             canL && push!(lc, oq)
#             canS && push!(sc, oq)
#         else
#             canL && push!(lp, oq)
#             canS && push!(sp, oq)
#         end
#     end
#     return oqss
# end

function oqssAll(oqs)::Oqss
    c = Vector{OptionQuote}()
    sizehint!(c, 100)
    p = Vector{OptionQuote}()
    sizehint!(p, 100)
    oqss = Styles(Sides(c, c), Sides(p, p))
    for oq in oqs
        if SmallTypes.isCall(oq)
            push!(c, oq)
        else
            push!(p, oq)
        end
    end
    return oqss
end

filtEntry(oqs, curp, legs)::Oqss = filtOqss(filtLong(legs), filtShort(curp, legs), oqssAll(oqs))

filtOqss(fl::Function, fs::Function, oqss::Oqss) =
        Styles(Sides(filter(fl, oqss.call.long), filter(fl, oqss.call.short)), Sides(filter(fs, oqss.put.long), filter(fs, oqss.put.short)))

function filtLong(legsCheck)
    isok = !isConflict(legsCheck, Side.long)
    return oq -> isok(oq)
end

function filtShort(curp, legsCheck)
    cans = canShort(Globals.get(:Strats), curp)
    isok = !isConflict(legsCheck, Side.short)
    return oq -> isok(oq) && cans(oq)
end

# function findOqs(oqs, curp::Currency, dists; maxDiff=1.0)
#     res = Vector{OptionQuote}(undef, length(dists))
#     diffs = fill(Inf, length(dists)) # Vector{Currency}(Inf, length(dists))
#     for oq in oqs
#         dist = getStrike(oq) - curp
#         for i in eachindex(diffs)
#             diff = abs(dists[i] - dist)
#             if diff < diffs[i]
#                 diffs[i] = diff
#                 res[i] = oq
#             end
#         end
#     end
#     isnothing(findfirst(x -> x > maxDiff, diffs)) || return nothing
#     return res
# end

end