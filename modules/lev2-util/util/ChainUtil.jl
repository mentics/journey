module ChainUtil
using Globals, SH, BaseTypes, SmallTypes, OptionTypes, LegTypes, LegMetaTypes, ChainTypes

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

function oqssEntry(oqs, curp::Currency, legsCheck=LEGS_EMPTY)::Oqss
    lc = Vector{OptionQuote}()
    sc = Vector{OptionQuote}()
    lp = Vector{OptionQuote}()
    sp = Vector{OptionQuote}()
    oqss = Styles(Sides(lc, sc), Sides(lp, sp))
    fConL = !isConflict(legsCheck, Side.long)
    fConS = !isConflict(legsCheck, Side.short)
    fCanS = canShort(Globals.get(:Strats), curp)
    # oqs = Iterators.filter(isValid(curp), oqs)
    for oq in oqs
        canL = fConL(oq)
        canS = fCanS(oq) && fConS(oq)
        if SmallTypes.isCall(oq)
            canL && push!(lc, oq)
            canS && push!(sc, oq)
        else
            canL && push!(lp, oq)
            canS && push!(sp, oq)
        end
    end
    return oqss
end

export ChainLookup, ChainSearch3
const ChainLookup = Styles{Dict{Currency,OptionQuote}}
struct ChainSearch3
    oqs::Vector{OptionQuote}
    strikes::Vector{Currency}
end
const ChainSearchS2 = Styles{ChainSearch3}
# ChainSearch() = ChainSearch(Styles(Vector{OptionQuote}(),Vector{OptionQuote}()), Styles(Vector{Currency}(),Vector{Currency}()))
function tolup(oqs)::ChainLookup
    d = Styles(Dict{Currency,OptionQuote}(), Dict{Currency,OptionQuote}())
    for oq in oqs
        if SmallTypes.isCall(oq)
            d.call[getStrike(oq)] = oq
        else
            d.put[getStrike(oq)] = oq
        end
    end
    return d
end

function toSearch(oqs)::ChainSearchS2
    calls = Vector{OptionQuote}()
    puts = Vector{OptionQuote}()
    for oq in oqs
        if SmallTypes.isCall(oq)
            push!(calls, oq)
        else
            push!(puts, oq)
        end
    end
    sort!(calls; by=getStrike)
    sort!(puts; by=getStrike)
    return ChainSearchS2(ChainSearch3(calls, getStrike.(calls)), ChainSearch3(puts, getStrike.(puts)))
end

lup(data::ChainLookup, style::Style.T, strike::Currency)::Union{OptionQuote,Nothing} = get(getfield(data, Symbol(style)), strike, nothing)
lup(data::ChainLookup, opt::Option)::Union{OptionQuote,Nothing} = get(getfield(data, Symbol(getStyle(opt))), getStrike(opt), nothing)

abstract type Dir end
abstract type LTE <: Dir end
abstract type GTE <: Dir end

function findDir(dir, data::ChainSearch3, style::Style.T, val::Currency; maxDist=C(7.0))::Union{Nothing,OptionQuote}
    strikes = data.strikes
    if dir == LTE
        ind = searchsortedlast(data.strikes, val)
    else
        ind = searchsortedfirst(data.strikes, val)
    end
    if ind < 1 || (ind >= length(data.strikes) && data.strikes[end] < val - maxDist)
        bt.log("Could not find $(dir) strike for trade $(getExpiration(oqs[1])) style:$(style) strike:$(val) minStrike:$(data.strikes[1]) maxStrike:$(data.strikes[end])")
        return nothing
    end
    return data.oqs[ind]
end

# function findDir(dir, data::ChainSearch2, style::Style.T, val::Currency; maxDist=C(7.0))::Union{Nothing,OptionQuote}
#     strikes = getfield(data.strikes, Symbol(style))
#     if dir == LTE
#         ind = searchsortedlast(data.strikes, val)
#     else
#         ind = searchsortedfirst(data.strikes, val)
#     end
#     if ind < 1 || (ind >= length(data.strikes) && data.strikes[end] < val - maxDist)
#         bt.log("Could not find $(dir) strike for trade $(getExpiration(oqs[1])) style:$(style) strike:$(val) minStrike:$(data.strikes[1]) maxStrike:$(data.strikes[end])")
#         return nothing
#     end
#     return data.oqs[ind]
# end

# function findGte(data::ChainSearch, style::Style.T, gte::Currency; maxDist=C(7.0))::Union{Nothing,OptionQuote}
#     oqs = getfield(data, Symbol(style))
#     ind = findlast(oq -> getStrike(oq) < gte, oqs)
#     if ind >= length(oqs)
#         bt.log("Could not find a >= strike for trade $(getExpiration(oqs[1])) style:$(style) strike:$(gte) minStrike:$(data.strikes[1]) maxStrike:$(data.strikes[end])")
#         return nothing
#     end
#     return data.oqs[ind]
# end

# function oqssAll(oqs)::Oqss
#     c = Vector{OptionQuote}()
#     sizehint!(c, 100)
#     p = Vector{OptionQuote}()
#     sizehint!(p, 100)
#     oqss = Styles(Sides(c, c), Sides(p, p))
#     for oq in oqs
#         if SmallTypes.isCall(oq)
#             push!(c, oq)
#         else
#             push!(p, oq)
#         end
#     end
#     return oqss
# end

# filtEntry(all, curp, legs)::Oqss = filtOqss(filtLong(legs), filtShort(curp, legs), all)

# filtOqss(fl::Function, fs::Function, oqss::Oqss) =
#         Styles(Sides(filter(fl, oqss.call.long), filter(fl, oqss.call.short)), Sides(filter(fs, oqss.put.long), filter(fs, oqss.put.short)))

# function filtLong(legsCheck)
#     isok = !isConflict(legsCheck, Side.long)
#     return oq -> isok(oq)
# end

# function filtShort(curp, legsCheck)
#     cans = canShort(Globals.get(:Strats), curp)
#     isok = !isConflict(legsCheck, Side.short)
#     return oq -> isok(oq) && cans(oq)
# end

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