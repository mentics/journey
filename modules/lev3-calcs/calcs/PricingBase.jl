module PricingBase

function quote_dir(dir, bid::T, ask::T)::T where T<:Real
    if dir == 1
        return quote_long(bid, ask)
    else
        return quote_short(bid, ask)
    end
end

function quote_long(bid::T, ask::T)::T where T<:Real
    return price_raw(-ask, -bid)
end

function quote_short(bid::T, ask::T)::T where T<:Real
    return price_raw(bid, ask)
end

function price_raw(worst::T, best::T)::T where T<:Real
    if worst > best
        # TODO: do something else?
        println("WARN: Tried to price when worst is better than ask ", (;worst, best))
        return min(worst, best)
    end

    spread = best - worst
    if spread <= 0.02
        return worst
    # elseif spread <= 0.4
    #     mult = round(Int, spread / 0.04, RoundUp)
    #     return bid + mult * 0.01
    elseif spread <= 0.5
        return worst + ceil(spread / 4 * 100)/100 - 0.01
    else
        return worst + 0.2
    end
end

end