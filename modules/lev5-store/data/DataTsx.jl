module DataTsx
using Dates, DataFrames
using ThetaData, Paths
import DateUtil, PricingBase, OptionUtil
using DataRead, DataXpirts, DataPrices
import Calendars as cal

const NTM_COUNT = 8

#=
Data modules pattern:
  get_*: data both from files and query recent if necessary
  make_*: save historical data to files
  update_*: update files with recent historical data
=#

#region Extra
# Can use from either backtest/training or live.
function calc_xtq(ts, xpirts, ts_price, style, strikes, bids, asks)
    tex = cal.calcTex(ts, xpirts)
    inds = first(sortperm(strikes; by=x -> abs(x - ts_price)), NTM_COUNT)
    strikes = strikes[inds]
    bids = bids[inds]
    asks = asks[inds]
    p = sortperm(strikes)
    strikes = strikes[p]
    bids = bids[p]
    asks = asks[p]
    # xtrins = OptionUtil.calc_extrin(style, ts_price, strikes, bids, asks)
    xtrins_bids = OptionUtil.calc_extrin(style, ts_price, strikes, bids)
    xtrins_asks = OptionUtil.calc_extrin(style, ts_price, strikes, asks)
    if !_all(>, xtrins_asks, -0.0001)
        global kxtrins = (;xtrins_bids, xtrins_asks, args=(;ts, xpirts, ts_price, style, strikes, bids, asks))
    end
    # TODO: I'm allowing negative xtrins because even asks are negative sometimes and... that might be ok to include.
    # @assert _all(>, xtrins_asks, -0.0001)
    # replace!(x -> max(x, zero(x)), xtrins_bids)
    # xtrins = (xtrins_bids .+ xtrins_asks) ./ (2 * ts_price * sqrt(tex))
    divid = ts_price * sqrt(tex)
    xtrins = map(xtrins_bids, xtrins_asks) do bid, ask
        (ask < 0 ? ask : ( (max(zero(bid), bid) + ask) / 2 )) / divid
    end
    xs = strikes ./ ts_price .- 1
    ps = fit(xs, xtrins).param
    # global kpoly = (;xs, xtrins, ps)
    # TODO: include multiple points to get some shape and direction
    xtq = model(0.0, ps)
    return Float32(xtq)
end
#endregion

#region Standard Api
function make_tsx(;sym="SPY")
    price_lookup = DataRead.price_lookup()
    df = mapreduce(vcat, DateUtil.year_months()[5:6]) do (year, month)
        println("processing $year $month")
        proc(remove_at_xpirts(DataRead.get_options(year, month; sym)), price_lookup)
    end
    # TODO: put this check back in when data available
    # @assert DataRead.get_ts() == df.ts
    Paths.save_data(DataRead.file_tsx(;sym), df)
    return df
end

function update_tsx(;sym="SPY")
    df = DataRead.get_tsx()
    tss = DataRead.get_ts()
    if tss[end] > df.ts[end]
        price_lookup = DataRead.price_lookup()
        ind = searchsortedfirst(tss, df.ts[end] + Second(5))
        to_proc = tss[ind:end]
        yms = DateUtil.year_months.(;start_date=Date(to_proc[1]), end_date=Date(to_proc[end]))
        df = mapreduce(vcat, yms) do (year, month)
            df = filter(:ts => (ts -> to_proc[1] <= ts <= ts_proc[end]), DataRead.get_options(year, month; sym))
            proc(df, price_lookup)
        end
        @assert tss == df.ts
        return to_proc
    else
        println("DataTsx already up to date $(df.ts[end]) == tss[end]")
        touch(DataRead.file_tsx(;sym))
    end
end
#endregion Standard Api

#region Local
function proc(df, price_lookup)
    gdf = groupby(df, [:ts, :expir, :style])
    df = combine(gdf, [:ts,:expir,:style,:strike,:bid,:ask] => calc_tsx_in_df(price_lookup) => [:xtq, :ret])
    gdf = groupby(df, [:ts, :expir])
    df = combine(gdf,
            [:style, :xtq] => split_style => [:xtq_call, :xtq_put],
            :ret => first => :ret)
end

function split_style(s, x)
    @assert s[1] == -1
    @assert s[2] == 1
    (;xtq_call=x[1], xtq_put=x[2])
end

calc_tsx_in_df(price_lookup) = function(tss, xpirtss, styles, strikes, bids, asks)
    ts = first(tss)
    @assert all(ts .== tss)
    xpirts = first(xpirtss)
    @assert all(xpirts .== xpirtss)
    style = first(styles)
    @assert all(style .== styles)
    calc_tsx(ts, xpirts, price_lookup[ts], price_lookup[xpirts], style, strikes, bids, asks)
end
function calc_tsx(ts, xpirts, ts_price, xpir_price, style, strikes, bids, asks)
    xtq = calc_xtq(ts, xpirts, ts_price, style, strikes, bids, asks)
    ret = xpir_price / ts_price - 1
    return (;xtq, ret)
end

# function add_xpir_dates!(xdd::XpirDateDicts, xpir; sym)
#     dates = ThetaData.query_dates_for_xpir(xpir, sym)
#     for date in dates
#         push!(get!(Vector, xdd.xpir_to_date, xpir), date)
#         push!(get!(Vector, xdd.date_to_xpir, date), xpir)
#     end
# end
#endregion Local

#region Util
function _all(f, v, args...)
    @inbounds for x in v
        if !f(x, args...)
            return false
        end
    end
    return true
end

remove_at_xpirts(df) = filter([:ts,:expir] => ((ts, xpirts) -> ts != xpirts), df)

import LsqFit
function model(x, p)
    # @show x p
    @. p[1] * exp(-p[2] * x^2) + p[3] / (1 + p[4] * x^2)
end
function fit(xs, ys)
    fit = LsqFit.curve_fit(model, xs, ys, [0.001, 1000., 1., 1.]; lower=[0.0, 0.0, -100.0, 0.0])
    # @show fit.param
    return fit
end

# TODO: fix the model a little. I think it underestimates the apex and the tails
# DataTsx.drawpoly(DataTsx.kpoly...)

import DrawUtil
function drawpoly(xpts, ypts, ps)
    DrawUtil.draw(:vlines, 0.0)
    DrawUtil.draw!(:scatter, xpts, ypts)
    width = xpts[end] - xpts[1]
    left = xpts[1] - width
    right = xpts[end] + width
    xs = left:(width/100):right
    DrawUtil.draw!(:lines, xs, model(xs, ps))
end

# # # TODO: make better model and fitting?
# # powers = -2:-1
# # xs = strikes ./ ts_price .- 1
# # fit = polyfit(xs, xtrins, powers)
# # global kpoly = (;powers, fit, xs, xtrins)
# # # @show ts xpirts xs xtrins fit

# """
# Fit y = f(x) to nth order polynomial.
# Returns list of polynomial coefficients.
# """
# polyfit(x, y, powers) = polyfit(Float64.(x), Float64.(y), powers)
# function polyfit(x::Vector{Float64},y::Vector{Float64},powers)
#     # @show x y powers
#     # global kfit = (;x, y, powers)
# 	# @assert length(x) > n
# 	# A = [k^n for k in x, n in 0:n]
# 	A = [k^n for k in x, n in powers]
# 	A \ y
# end
# import DrawUtil
# function drawpoly(powers, cs, xpts, ypts)
#     DrawUtil.draw(:vlines, 0.0)
#     DrawUtil.draw!(:scatter, xpts, ypts)
#     width = xpts[end] - xpts[1]
#     left = xpts[1] - width
#     right = xpts[end] + width
#     xs = left:(width/100):right
#     # ys = [coeffs[1] + coeffs[2] * x + coeffs[3] * x^2 for x in xs]
#     # ys = [c*k^n for c in coeffs, k in xs, n in powers]
#     ys = [sum([c*x^n for (c,n) in zip(cs, powers)]) for x in xs]
#     @show powers cs xs ys
#     DrawUtil.draw!(:lines, xs, ys)
# end

# import BenchmarkTools
# f1(x) = all(x .> 0)
# f2(x) = _all(>, x, 0)
# function test()
#     v = rand(10000)
#     BenchmarkTools.@btime f2($v)
# end
#endregion

end