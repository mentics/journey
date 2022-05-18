module CalcIvs
using Dates
using DateUtil

export ivToStdDev

ivToStdDev(iv::Float64, timeToExpY::Float64) = iv / sqrt(1.0/timeToExpY)

# TODO: use a μ that is based on historical drift

# makeIvDistri(exp::Date=expir(1)) = (nd = Normal(1.0, calcIvd(exp)); (nd, calcProbsND(nd)))

# TODO: cache
function calcIvd(from::Date, to::Date)::Float64
    nd = bdays(from, to)
    ivd2 = infoIv(to) / (sqrt(252.0/nd))
    ivd = infoIv(to) / sqrt(1.0/timeToExpir(to, msMarketOpen(from))) # TODO: this should use market open time unless useCurp is true
    @info "Calculating iv distri for $(to) days:$(nd) ivd:$(s(ivd,5)) ivd2:$(s(ivd2,5))"
    return ivd
end

function infoIv(exp::Date)
    global ivCall = infoIvs(exp, Style.call)
    global ivPut = infoIvs(exp, Style.put)
    # global ivs = map(x->x.iv, res)
    all::Vector{Union{Missing,Float64}} = vcat(ivCall, ivPut)
    # global iv = fit(Normal, all)
    # return sum(all) / length(all)
    return median(skipmissing(all))
end

function infoIvs(d::Date, styl=Style.call)
    curp = market().curp
    c = chains().lookup[d][styl]
    res = []
    for strike in keys(c)
        if abs(1.0 - strike / curp) > 0.04
            continue
        end
        item = c[strike]
        if item.quot.bid > 0.1
            # push!(res, (;strike, item.quot.bid, iv=item.greeks["mid_iv"]))
            push!(res, OptionTypes.getIv(item))
        end
    end
    return res
end

end