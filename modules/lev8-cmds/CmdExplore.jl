module CmdExplore
using Dates
using SH, Globals, BaseTypes, SmallTypes, RetTypes, StratTypes, LegMetaTypes, ConstructUtil
using Shorthand, Between
using Expirations, Markets, Chains
using DrawStrat

export sh, shc, shRet, shVals, drsh, drsh!, shLegs # shLegs is reexported form Shorthand
export drlms, drlms!

drlms(lms; kws...) = drawRet(combineTo(Ret, lms, minimum(getExpiration.(lms)), market().startPrice, getvr()); curp=market().curp, kws...)
drlms!(lms; kws...) = drawRet!(combineTo(Ret, lms, minimum(getExpiration.(lms)), market().startPrice, getvr()); kws...)

export findShortsToClose
# TODO: move this to scheduled
function findShortsToClose()
    filter(positions(;age=Second(0))) do p
        oq = optQuoter(getLeg(p), Action.close)
        return getSide(p) == Side.short && abs(getBid(oq)) < .02
    end
end

shc(args...) = Tuple(lr for lr in sh(args...))
sh(str::AStr, exs::Int...) = sh(str, expir.(exs))
sh(str::AStr, exps::Coll{Date}=expirs())::Vector{LegMeta} = tos(LegMeta, shLegs(str, collect(exps)), optQuoter)
function shlr(str::AStr, exps=expirs(), sp=market().startPrice)
    lms = sh(str, exps)
    exp = minimum(getExpiration, lms)
    vr = Globals.get(:vtyRatio)
    [(lm, to(Ret, lm, exp, sp, vr)) for lm in lms]
end
shRet(str::AStr, exps, sp=market().startPrice) = combineTo(Ret, shlr(str, exps, sp))
shVals(str::AStr, exps, sp=market().startPrice) = getVals(shRet(str, exps, sp))
drsh(str::AStr, ex::Int=1; kws...) = drsh(str, expirs()[ex:ex+2]; kws...) # (sp = market().startPrice ; drawRet(shRet(str, expirs()[ex:ex+2], sp), nothing, sp, "sh") )
drsh(str::AStr, exps; kws...) = (sp = market().startPrice ; drawRet(shRet(str, exps, sp); kws..., curp=sp, label="sh") )
drsh!(str::AStr, ex::Int=1; kws...) = drsh!(str, expirs()[ex:ex+2])
drsh!(str::AStr, exps; kws...) = (sp = market().startPrice ; drawRet!(shRet(str, exps, sp); label="sh+") )

# TODO: need to move all probs calcs to util
# using ProbHist
# function locprobs(sp, exps)
#     numDays = mktNumDays(minimum(exps))
#     phOrig = probHist(sp, numDays)
#     ph = Prob(getCenter(phOrig), smooth(getVals(phOrig)))
# end

using StratTypes
export findSpreads, findSpread
findSpreads(spreads, combi::Combi) = ( findSpread(spreads, (combi[1], combi[2])), findSpread(spreads, (combi[3], combi[4])) )
findSpread(spreads, lr::NTuple{2,LegRet}) = findfirst(s -> s == lr, spreads)

using CmdStrats, StratGen, Positions
function whyNotCombi(combi::Combi)
    inspreads = findSpreads(CmdStrats.lastSpreads2[], combi)
    # @info "Was in spreads?" inspreads
    oqs = optQuoter.(tos(LegMeta, combi))
    vo = StratGen.validOption(Globals.get(:Strats)[:maxStrikeDist], market().curp)
    optInvalid = findfirst(map(!, vo.(oqs)))
    @info "Options valid?" optInvalid
    legsPos = getLeg.(positions())
    conflict = map(lr -> isConflict.(getLeg(lr), legsPos), combi)
    @info "Is position conflict?" conflict # Tuple(c for c in conflict)

    if (inspreads == (nothing,nothing))
        answer = "because: \n"
        if !isnothing(optInvalid) # optsValid != (false, false, false, false)
            answer *= "  first invalid option found "
            answer *= string(oqs[optInvalid])
            answer *= '\n'
        end
        if conflict != (false, false, false, false)
            ind = findfirst(conflict)
            answer *= "  conflicted with position "
            answer *= string(find(p -> isConflict(combi[ind], getLeg(p)), legsPos))
            answer *= '\n'
        end
        println("Was not in spreads $(answer)")
    else
        println("TODO: was in spreads")
    end
end

end