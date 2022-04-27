module PayloadUtil
using SH
using LegMetaTypes, LegTradeTypes

export mergeLegs!

function mergeLegs!(legs::AbstractVector)::Nothing
    i = 1
    while i <= length(legs)-1
        leg1 = legs[i]
        curOption = getOption(leg1)
        curSide = getSide(leg1)
        j = i+1
        while j <= length(legs)
            leg2 = legs[j]
            # @info "checking" leg1 leg2
            if getOption(leg2) == curOption && getSide(leg2) == curSide
                # TODO: very messy handling both LegTrade and LegMeta
                legs[i] = addQuantity(leg1, getQuantity(leg2))
                # lgs = deleteat(lgs, j)
                deleteat!(legs, j)
            else
                j += 1
            end
        end
        i += 1
    end
    return
end

end