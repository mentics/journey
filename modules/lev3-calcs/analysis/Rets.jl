module Rets
using Dates
using RetTypes, BaseTypes, SH, SmallTypes, Bins, LegTypes, OptionMetaTypes
using DateUtil, LogUtil
using BlacksPricing
using Globals

export makeRet, estAtPrice, combineRetVals!, combineRets, combineRetVals

# This is where we convert from Currency to Float64
function makeRet(leg::Leg, m::OptionMeta, neto::Currency, targetDate::Date, sp::Currency, vtyRatio::Float64=Globals.get(:vtyRatio))::Ret
    if getExpiration(leg) === targetDate
        return atExp(getStyle(leg), Float64(getStrike(leg)), getSide(leg), getQuantity(leg), Float64(neto), Float64(sp))
    else
        return afterExp(leg, m, Float64(neto), targetDate, Float64(sp), vtyRatio)
    end
end

function combineRetVals!(buf, rets::NTuple{4,Ret})
    # TODO: can remove asserts to speed up later
    @assert rets[1].center == rets[2].center == rets[3].center == rets[4].center
    valss = map(r->r.vals, rets)
    for i in eachindex(valss[1])
        buf[i] = valss[1][i] + valss[2][i] + valss[3][i] + valss[4][i]
    end
end
function combineRetVals!(buf::Vector{Float64}, rets::NTuple{4,Ret}, extra::Vector{Float64})
    # TODO: can remove asserts to speed up later
    @assert rets[1].center == rets[2].center == rets[3].center == rets[4].center
    # foreach(extra) do r; @assert rets[1].center == r.center end
    valss = map(r->r.vals, rets)
    # extraValss = map(r->r.vals, extra)
    for i in eachindex(rets[1].vals)
        # println(typeof(extraValss), ", ", length(extraValss))
        # @info typeof(valss) length(valss) length(extraValss[1])
        # error("")
        # es = 0.0
        # for exv in extraValss
        #     es += exv[i] + exv[i]
        # end

        # es = 0.0
        # for ie in eachindex(extra)
        #     es += extra[ie].vals[i]
        # end

        # es = sum(v[i] for v in extraValss)
        # es = reduce((ret1, ret2) -> ret1[i] + ret2[i], extraValss)
        buf[i] = valss[1][i] + valss[2][i] + valss[3][i] + valss[4][i] + extra[i]
    end
end

function combineRetVals(rets)::Vector{Float64}
    c = first(rets).center ; @assert isnothing(findfirst(x->x.center!=c, rets))
    return mapreduce(r->r.vals, +, rets)
end
function combineRets(rets)::Ret
    c = first(rets).center ; @assert isnothing(findfirst(x->x.center!=c, rets))
    return Ret(c, mapreduce(r->r.vals, +, rets))
end

#region Local
function atExp(style::Style.T, strike::Float64, side::Side.T, qty::Float64, neto::Float64, sp::Float64)::Ret
    @assert qty > 0.0
    # @info "atExp" style strike side qty neto sp
    if side == Side.long && style == Style.call
        @assert neto < 0.0
        vals = lineVals(sp, 0.0, qty * neto, strike, qty, -qty * (strike - neto))
    elseif side == Side.long && style == Style.put
        @assert neto < 0.0
        vals = lineVals(sp, -qty, qty * (strike + neto), strike, 0.0, qty * neto)
    elseif side == Side.short && style == Style.call
        @assert neto > 0.0
        vals = lineVals(sp, 0.0, qty * neto, strike, -qty, qty * (strike + neto))
    else
        @assert neto > 0.0
        vals = lineVals(sp, qty, -qty * (strike - neto), strike, 0.0, qty * neto)
    end
    return Ret(sp, vals)
end

function afterExp(leg::Leg, m::OptionMeta, neto::Float64, targetDate::Date, sp::Float64, vtyRatio::Float64)::Ret
    exp = getExpiration(leg)
    if exp == targetDate
        error("Wrong segment called with same exp", exp, targetDate)
    else
        vty = vtyRatio * getIv(m)
        if !(.05 < vty < .5)
            @log debug "Invalid vty in segPricing, using global average" vty leg
            vty = vtyRatio * Globals.get(:vtyAvg)[exp]
        end
        # TODO: benchmark compare to simpler closure way
        ctx = (getStyle(leg), Float64(getStrike(leg)), timeToExpir(targetDate, exp), vty, getQuantityDir(leg), neto, sp)
        vals = valsFor(ctx) do (style, strike, toExpYear, vty, qtyDir, neto, sp), x
            y = neto + qtyDir * priceOption((style, strike, toExpYear, vty)..., sp * x)
            # (isfinite(y) && (-100.0 < y < 100.0)) || error("afterExp: calced nan ", x, " ", (sp, m1, x01, split, m2, x02))
            return y
        end
        return Ret(sp, vals)
    end
end

# │   leg = LegTypes.Leg(Option(call, 2022-04-11, 466.000), 1.0, SmallTypes.Side.long)
# │   m = OptionMetaTypes.OptionMeta(0.13166179776743192)
# │   neto = -1.15
# │   targetDate = 2022-04-06
# │   sp = 455.22
# └   vtyRatio = 0.8

function lineVals(sp, m1, x01, split, m2, x02)
    # @info "lineVals" m1 x01 split m2 x02
    # TODO: benchmark compare to simpler closure way
    ctx = (m1, x01, split, m2, x02)
    return valsFor(ctx) do (m1, x01, split, m2, x02), x
        x *= sp
        y = x < split ? x * m1 + x01 : x * m2 + x02
        # println(x, ", ", y)
        # (isfinite(y) && (-100.0 < y < 100.0)) || error("lineVals: calced nan ", x, " ", (sp, m1, x01, split, m2, x02))
        return y
    end
end

function valsFor(f, ctx)::Vector{Float64}
    v = Vector{Float64}(undef, numVals())
    v[1] = leftVal(f, ctx)
    i = 1
    for x in binXs()[2:end-1]
        i += 1
        v[i] = f(ctx, x)
    end
    @assert i == numVals() - 1
    v[end] = rightVal(f, ctx)
    return v
end
#endregion

end