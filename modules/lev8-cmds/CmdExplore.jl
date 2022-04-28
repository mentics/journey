module CmdExplore
using BaseTypes
using SH, Globals, Shorthand, Between, RetTypes, StratTypes, LegMetaTypes
using Expirations
using DrawStrat
using Markets, Chains
# using CmdStrats

export sh, shc, shRet, shVals, drsh, shLegs # shLegs is reexported form Shorthand

shc(args...) = Tuple(lr for lr in sh(args...))
sh(str::AStr, exps=expirs())::Vector{LegMeta} = tos(LegMeta, shLegs(str, exps), optQuoter)
function shlr(str::AStr, exps=expirs(), sp=market().startPrice)
    lms = sh(str, exps)
    exp = minimum(getExpiration, lms)
    vr = Globals.get(:vtyRatio)
    [(lm, to(Ret, lm, exp, sp, vr)) for lm in lms]
end
shRet(str::AStr, exps, sp=market().startPrice) = combineTo(Ret, shlr(str, exps, sp))
shVals(str::AStr, exps, sp=market().startPrice) = getVals(shRet(str, exps, sp))
drsh(str::AStr, exps) = (sp = market().startPrice ; drawRet(shRet(str, exps, sp), nothing, sp, "sh") )

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