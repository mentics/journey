module DataTsx
using Dates, DataFrames
using ThreadPools
import StatsBase:mean
using ThetaData, Paths
import DateUtil, PricingBase, OptionUtil
using DataConst, DataRead, DataXpirts, DataPrices
import Calendars as cal

const NTM_COUNT3 = 8
const NTM_RADIUS = NTM_COUNT3 ÷ 2
const XTQ_RADIUS = 0.004

# TODO: more confirming the curve fitting is doing well. Maybe add asserts for its quality.

#=
Data modules pattern:
  get_*: data both from files and query recent if necessary
  make_*: save historical data to files
  update_*: update files with recent historical data
=#

#region Extra
# Can use from either backtest/training or live.
function calc_xtqs(ts, xpirts, ts_price, style, strikes, bids, asks)
    tex = cal.calcTex(ts, xpirts)
    # inds = first(sortperm(strikes; by=x -> abs(x - ts_price)), NTM_COUNT3)
    @assert issorted(strikes)
    mid = searchsortedlast(strikes, ts_price)
    inds = (mid - NTM_RADIUS + 1):(mid + NTM_RADIUS)
    strikes = strikes[inds]
    # @show ts_price inds strikes
    bids = bids[inds]
    asks = asks[inds]
    p = sortperm(strikes)
    strikes = strikes[p]
    @assert strikes[NTM_COUNT3 ÷ 2] <= ts_price
    @assert strikes[NTM_COUNT3 ÷ 2 + 1] >= ts_price
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
    xs = Float64.(strikes ./ ts_price .- 1) # for fitting precision
    max_y = 2 * maximum(xtrins)
    xtqs = fit_sides(xs, xtrins, style, max_y)

    if style == 1 || !_all(x -> -0.001 < x < max_y, xtqs) || maximum(xtqs) != xtqs[length(xtqs) ÷ 2 + 1]
        global kxtq = (;divid, xtrins, xs, xtqs, max_y, xtrins_bids, xtrins_asks, args=(ts, xpirts, ts_price, style, strikes, bids, asks))
        error("xtq out of range: $(xtqs)")
    end
    global kxtq = (;divid, xtrins, xs, xtqs, max_y, xtrins_bids, xtrins_asks, args=(ts, xpirts, ts_price, style, strikes, bids, asks))
    return Float32.(xtqs)
end
#endregion

#region Standard Api
function make_tsx(;sym="SPY")
    price_lookup = DataRead.price_lookup()

    names = readdir(dirname(DataRead.file_options(2012, 6)))
    # [match(r"quotes-SPY-(\d{4})-(\d{2}).arrow", f).captures[1:2] for f in fs]
    yms = sort!([parse.(Int, match(r"quotes-SPY-(\d{4})-(\d{2}).arrow", name).captures[1:2]) for name in names])
    # TODO: use this opportunity to confirm this matches all months from start to now

    # df = mapreduce(vcat, DateUtil.year_months()[2:4]) do (year, month)
    #     println("processing $year $month")
    #     df = DataRead.get_options(year, month; sym)
    #     sdf = remove_at_xpirts(filter_within_days(df, DataConst.XPIRS_WITHIN))
    #     proc(sdf, price_lookup)
    # end

    # DateUtil.year_months()[2:8]
    stop = false
    yms = [(2017,12)]
    try
        dfs = qbmap(yms) do (year, month)
            !stop || return
            println("processing $year $month")
            proc(filtered_options(year, month; sym), price_lookup)
        end
        df = reduce(vcat, dfs)
        sort!(df, [:ts, :expir])
        @assert issorted(df, [:ts, :expir])
        @assert allunique(df, [:ts, :expir])

        # TODO: put this check back in when data available
        # @assert DataRead.get_ts() == df.ts
        # Paths.save_data(DataRead.file_tsx(;sym), df)
        return df
    finally
        stop = true
    end
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
            df = filter(:ts => (ts -> to_proc[1] <= ts <= ts_proc[end]), filtered_options(year, month))
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
    threads = false
    gdf = groupby(df, [:ts, :expir, :style])
    df = combine(gdf, [:ts,:expir,:style,:strike,:bid,:ask] => calc_tsx_in_df(price_lookup) => [:xtq1, :xtq2, :xtq3, :ret]; threads)
    gdf = groupby(df, [:ts, :expir])
    df = combine(gdf,
            [:style, :xtq1, :xtq2, :xtq3] => split_style => [:xtq1_call, :xtq2_call, :xtq3_call, :xtq1_put, :xtq2_put, :xtq3_put],
            :ret => first => :ret; threads)
end

function split_style(s, x1, x2, x3)
    @assert s[1] == -1
    @assert s[2] == 1
    (;xtq1_call=x1[1], xtq2_call=x2[1], xtq3_call=x3[1], xtq1_put=x1[2], xtq2_put=x2[2], xtq3_put=x3[2])
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
    xtqs = calc_xtqs(ts, xpirts, ts_price, style, strikes, bids, asks)
    ret = xpir_price / ts_price - 1
    return (;xtq1=xtqs[1], xtq2=xtqs[2], xtq3=xtqs[3], ret)
end
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

# df = DataRead.get_options(year, month; sym)
# # sdf = remove_at_xpirts(filter_within_days(df, DataConst.XPIRS_WITHIN))
# sdf = filter_df(df, DataConst.XPIRS_WITHIN)
filtered_options(year, month; sym) = filter_df(DataRead.get_options(year, month; sym), DataConst.XPIRS_WITHIN)

# remove_at_xpirts(df) = filter([:ts,:expir] => ((ts, xpirts) -> ts != xpirts), df; view=true)
# filter_within_days(df, within_days) = filter([:ts, :expir] => is_within_days(within_days), df; view=true)
# is_within_days(within_days) = (ts, xpir) -> xpir - ts <= within_days

filter_df(df, within_days) = filter([:ts,:expir] => filter_all(within_days), df; view=true)

filter_all(within_days) = function(ts, xpirts)
    xpirts - ts < within_days &&
            ts != xpirts &&
            ts <= cal.getMarketClose(Date(ts))
end

function fit_sides(xs::Vector{Float64}, ys::Vector{Float64}, style, max_y)
    mid = (NTM_COUNT3 ÷ 2)
    # mid_inds = mid:(mid+1)
    mid_inds = (mid-1):(mid+2)
    # mid_ps = LsqFit.curve_fit(vertex_model, xs[mid_inds], ys[mid_inds], [0.00001,1000.0]; lower=[0.0,0.0], upper=[max_y, Inf]).param
    # mid_ps = fit(vertex_model, xs[mid_inds], ys[mid_inds], max_y)
    ys_mid = ys[mid_inds]
    ys_mid[1] = mean(ys[1:(mid-2)])
    ys_mid[end] = mean(ys[(mid+2):end])
    # @show ys_mid
    @assert ys_mid[end] < ys_mid[end-1]
    @assert ys[1] < ys[2]
    mid_ps = fit_vm(xs[mid_inds], ys_mid, max_y)
    yatz = vertex_model(0.0, mid_ps)
    if yatz <= maximum(ys)
        global kfit_sides = (;mid, mid_inds, ys_mid, mid_ps, yatz, args=(;xs, ys, style, max_y))
    end
    @show mid_ps yatz
    @assert mid_ps[1] > maximum(ys_mid)
    @assert yatz > maximum(ys)
    xtqs = insert!(copy(ys), mid+1, yatz)
    # @show xtqs
    return xtqs

    sm = side_model(yatz)
    if style == -1
        left_inds = 1:mid
        right_inds = (mid+1):NTM_COUNT3
        left_xs = vcat(xs[left_inds], 0.0)
        left_ys = vcat(ys[left_inds], yatz)
        left_ps = fit(sm, left_xs, left_ys)

        right_xs = vcat(0.0, xs[right_inds])
        right_ys = vcat(yatz, ys[right_inds])
        right_ps = fit(sm, right_xs, right_ys)

        left_1 = sm(-XTQ_RADIUS, left_ps)
        right_1 = sm(XTQ_RADIUS, right_ps)
        return (;xtqs=[left_1, yatz, right_1], left_ps, right_ps)
    else
        right_inds = 1:mid
        left_inds = (mid+1):NTM_COUNT3
        right_xs = vcat(0.0, xs[right_inds])
        right_ys = vcat(yatz, ys[right_inds])
        right_ps = fit(sm, right_xs, right_ys)

        left_xs = vcat(xs[left_inds], 0.0)
        left_ys = vcat(ys[left_inds], yatz)
        left_ps = fit(sm, left_xs, left_ys)

        right_1 = sm(XTQ_RADIUS, right_ps)
        left_1 = sm(-XTQ_RADIUS, left_ps)
        return (;xtqs=[left_1, yatz, right_1], left_ps, right_ps)
    end
end

function check(xs, ys, res)
    (;xtqs, left_ps, right_ps) = res
    yatz = xtqs[2]
    sm = side_model(yatz)
    left_xs = (2*xs[1]):0.0001:0.0
    right_xs = 0.0:0.0001:(2*xs[end])
    draw(:vlines, 0.0)
    draw!(:scatter, xs, ys; label="points")
    draw!(:lines, left_xs, [sm(x, left_ps) for x in left_xs]; label="left")
    draw!(:lines, right_xs, [sm(x, right_ps) for x in right_xs]; label="right")
end

import LsqFit
vertex_model(x, p) = @. p[1] * exp(-p[2] * x^2)
# side_model(x, p) = @. p[1] / (1 + p[2] * x^2)
# side_model(yatz) = (x, p) -> @. yatz / (1 + p[1] * x^2 + p[2] * x^3 + p[3] * x^4)
side_model(yatz) = (x, p) -> @. yatz / (1 + p[1] * x^2)
sminit() = [1.0]
smlower() = [0.0]
# smweights() = [1.0, 0.9, 0.8, 0.7, 0.6]
# smweights() = [1.0, 1.0, 0.0, 0.0, 0.0]
# smweights() = fill(NTM_COUNT3 ÷ 2 + 1)
# fit(model, xs, ys) = LsqFit.curve_fit(model, xs, ys, smweights(), sminit()).param
fit(model, xs, ys) = LsqFit.curve_fit(model, xs, ys, sminit(); lower=smlower()).param

# vm(xs, ps) = @. log(ps[1]) - ps[2] * xs^2
function vm(xs, ps)
    # if ps[1] < 0.0
    #     @show xs ps
    #     error("bad ps")
    # end
    @. log(ps[1]) - ps[2] * xs^2
    # @. ps[1] - ps[2] * xs^2
end
function fit_vm(xs, ys, max_y)
    ys2 = [log(max(1e-8, y)) for y in ys]
    # @show xs ys ys2
    # return LsqFit.curve_fit(vm, xs, ys2, [0.0001,1.0,1.0,0.0001], [log(maximum(ys)), 1000.0]).param#; lower=[0.0, 1.0]).param
    return LsqFit.curve_fit(vm, xs, ys2, [0.0001,1.0,1.0,0.0001], [maximum(ys), 1000.0]).param #; lower=[0.0, 1.0]).param
end
function it(xs, ys)
    mleft = (ys[2] - ys[1]) / (xs[2] - xs[1])
    mright = (ys[4] - ys[3]) / (xs[4] - xs[3])
    @show mleft mright
    b = log((xs[2] * mright) / (xs[3] * mleft)) / (xs[2]^2 - xs[3]^2)

    a = exp(-b * xs[2]^2) / ys[2]
    a2 = exp(-b * xs[3]^2) / ys[3]
    # a = m / (-2 * b * x2 * e^(-b * x2 ^ 2))
    a3 = mleft / (-2 * b * xs[2] * exp(-b * xs[2]^2))

    return (a, a2, a3, b)
end
#endregion Util

#=
y = a * e^(-b * x^2)
y' = -2 * a * b * x * e^(-b * x ^ 2)
y'(x2) = (y2-y1) / (x2-x1) = m = -2 * a * b * x2 * e^(-b * x2 ^ 2)
y'(x4) = (y4-y3) / (x4-x3) = n = -2 * a * b * x4 * e^(-b * x4 ^ 2)
1) a = m / (-2 * b * x2 * e^(-b * x2 ^ 2))
n = -2 * m * b * x4 * e^(-b * x4 ^ 2) / (-2 * b * x2 * e^(-b * x2 ^ 2))

n = -2 * m * b * k * e^(-b * k ^ 2) / (-2 * b * j * e^(-b * j ^ 2))


b = log((x2 * n) / (x4 * m)) / (x2^2 - x4^2)
2) y = a * e^(-b * x^2)
a = e^(-b * x^2) / y
--------------------

-2 * n * b * j * e^(-b * j ^ 2) = -2 * m * b * k * e^(-b * k ^ 2)
log(-2 * n * b * j) + (-b * j^2) = log(-2 * m * b * k) + (-b * k ^ 2)
log(-2 * n * b * j) - log(-2 * m * b * k) = (-b * k ^ 2) - (-b * j^2)
log(nj/mk) = -b(k^2 - j^2)
log(nj/mk) / (j^2 - k^2) = b
----------------
non-derivative sub
------------

a = y / e^(-b * x^2)

y/a = e^(-b * x^2)
log(y/a) = -b * x^2
b = -j^2 / (log(u/a))

v = a * e^(-(-j^2 / (log(u/a))) * k^2)


=#

#region Old Fit
# m(x, ps) = ps[1] * exp(-ps[2] * x^2)
# m2(xs, ps) = @. log(ps[1]) - ps[2] * xs^2
# using DrawUtil
# function ffit(xs, ys)
#     xs2 = xs # [x^2 for x in xs]
#     ys2 = [log(y) for y in ys]
#     ps = LsqFit.curve_fit(m2, xs2, ys2, [exp(maximum(ys)), 1.0]; lower=[0.0, 0.0]).param
#     # ps = [ps1[1], log(ps1[2])] # [exp(ps1[1]), ps1[2]]
#     axs = xs[1]:0.0001:max(0.0, xs[end])
#     draw(:lines, axs, [m(x, ps) for x in axs])
#     draw!(:scatter, xs, ys)
#     return ps
# end

# # mm(x, ps) = @. ps[1] * exp(-ps[2] * x^2) + ps[3] / (1 + x^2)
# mm(x, ps) = @. ps[1] / (1 + ps[2] * x^2)

# function ffit2(xs, ys)
#     res = LsqFit.curve_fit(mm, xs, ys, [maximum(ys), 1000.0, 0.1, 0.1]; lower=[0.0, 0.0, 0.0, 0.0], autodiff=:forwarddiff)
#     # println(LsqFit.margin_error(res))
#     ps = res.param
#     axs = min(0.0, xs[1]):0.0001:max(0.0, xs[end])
#     draw(:lines, axs, [mm(x, ps) for x in axs])
#     draw!(:scatter, xs, ys)
#     return ps
# end

import DrawUtil
function drawpoly(xpts, ypts, ps; n=false)
    n && DrawUtil.draw(:vlines, 0.0)
    DrawUtil.draw!(:scatter, xpts, ypts)
    width = xpts[end] - xpts[1]
    left = xpts[1] - width
    right = xpts[end] + width
    xs = left:(width/100):right
    DrawUtil.draw!(:lines, xs, model(xs, ps))
end

function drawpoly2(xpts, ypts, left_ps, right_ps; n=false)
    n && DrawUtil.draw(:vlines, 0.0)
    DrawUtil.draw!(:scatter, xpts, ypts; label="data")
    width = xpts[end] - xpts[1]
    left = xpts[1] - width
    right = xpts[end] + width
    left_xs = left:(width/50):0.0
    right_xs = 0.0:(width/50):right
    DrawUtil.draw!(:lines, left_xs, model(left_xs, left_ps); label="left")
    DrawUtil.draw!(:lines, right_xs, model(right_xs, right_ps); label="right")
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

# DataTsx.drawpoly(DataTsx.kpoly...)
#endregion Old Fit

# import BenchmarkTools
# f1(x) = all(x .> 0)
# f2(x) = _all(>, x, 0)
# function test()
#     v = rand(10000)
#     BenchmarkTools.@btime f2($v)
# end

end