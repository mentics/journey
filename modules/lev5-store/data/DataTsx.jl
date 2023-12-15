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
    radius = min(NTM_RADIUS, lastindex(strikes) - mid, mid)
    if radius <= 2
        global kargs = (;ts, xpirts, ts_price, style, strikes, bids, asks)
        # TODO investigate
        # too few strikes for 2014-02-03T16:30:00 2014-02-28T21:00:00
        println("WARN: too few strikes for $(ts) $(xpirts)")
        return [-1f0, -1f0, -1f0]
    end
    inds = (mid - radius + 1):(mid + radius)
    strikes = strikes[inds]
    # @show ts_price inds strikes
    bids = bids[inds]
    asks = asks[inds]
    p = sortperm(strikes)
    strikes = strikes[p]
    @assert strikes[radius] <= ts_price
    @assert strikes[radius+1] >= ts_price
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

    # TODO: fix the algo to deal with this
    # if !issorted(xtrins[1:radius])
    #     println("WARN: bad data left for $(ts): $(xtrins)")
    # end
    # if !issorted(xtrins[(radius+1):end]; rev=true)
    #     println("WARN: bad data right for $(ts): $(xtrins)")
    # end

    xtqs = fit_sides(xs, xtrins, style, max_y)

    if !_all(x -> -0.001 < x < max_y, xtqs) || maximum(xtqs) != xtqs[length(xtqs) ÷ 2 + 1]
        global kxtq = (;divid, xtrins, xs, xtqs, max_y, xtrins_bids, xtrins_asks, args=(;ts, xpirts, ts_price, style, strikes, bids, asks))
        # TODO: fix the algo to deal with this or fix data or something
        println("WARN: xtq out of range: $(xtqs)")
    end
    # global kxtq = (;divid, xtrins, xs, xtqs, max_y, xtrins_bids, xtrins_asks, args=(;ts, xpirts, ts_price, style, strikes, bids, asks))
    return Float32.(xtqs)
end
#endregion

#region Standard Api
function make_tsx(;sym="SPY")
    price_lookup = DataRead.price_lookup()

    (;avail) = DataRead.get_options_yms()
    yms = avail

    # df = mapreduce(vcat, DateUtil.year_months()[2:4]) do (year, month)
    #     println("processing $year $month")
    #     df = DataRead.get_options(year, month; sym)
    #     sdf = remove_at_xpirts(filter_within_days(df, DataConst.XPIRS_WITHIN))
    #     proc(sdf, price_lookup)
    # end

    # DateUtil.year_months()[2:8]
    stop = false
    # yms = [(2023,12)]
    # yms = [(2023,i) for i in 1:12]
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

        Paths.save_data(DataRead.file_tsx(;sym), df)
        tss = DataRead.get_ts()
        # TODO: inside thetadata, we're filtering out market open
        tss = filter(ts -> ts != cal.getMarketOpen(Date(ts)), tss)
        diff = symdiff(df.ts, tss)
        if !isempty(diff)
            return diff
        end
        return df
    finally
        stop = true
    end
end

function update_tsx(;sym="SPY")
    df = DataRead.get_tsx()
    tss = DataRead.get_ts()
    if tss[end] > df.ts[end]
        # TODO: reprocess the last days because we might have put placeholder values for :ret
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
    global kdf_orig = df
    gdf = groupby(df, [:ts, :expir, :style])
    df = combine(gdf, [:ts,:expir,:style,:strike,:bid,:ask] => calc_tsx_in_df(price_lookup) => [:xtq1, :xtq2, :xtq3, :ret]; threads)
    # df = filter([:xtq2] => (x -> x != -1f0), df)
    global kdf = df
    # return df
    gdf = groupby(df, [:ts, :expir])
    @assert abs(length(gdf) - size(df, 1) ÷ 2) <= 1 "length(gdf) $(length(gdf)) == $(size(df, 1) ÷ 2) size(df, 1) ÷ 2"
    df = combine(gdf,
            [:style, :xtq1, :xtq2, :xtq3] => split_style => [:xtq1_call, :xtq2_call, :xtq3_call, :xtq1_put, :xtq2_put, :xtq3_put],
            :ret => first => :ret; threads)
    filter!([:xtq2_call, :xtq2_put] => ((xc, xp) -> xc != -1f0 || xp != -1f0), df)
    # filter!([:xtq2_call, :xtq2_put] => ((xc, xp) -> xc != -1f0 || xp != -1f0), df)
end

function split_style(s, x1, x2, x3)
    if size(s, 1) != 2
        # @show s x1 x2 x3
        # error("only one $(size(s, 1))")
        return (;xtq1_call=-1f0, xtq2_call=-1f0, xtq3_call=-1f0, xtq1_put=-1f0, xtq2_put=-1f0, xtq3_put=-1f0)
    end
    if s[1] != -1
        @show s x1 x2 x3
        error("wrong one")
    end
    @assert s[1] == -1
    @assert s[2] == 1
    return (;xtq1_call=x1[1], xtq2_call=x2[1], xtq3_call=x3[1], xtq1_put=x1[2], xtq2_put=x2[2], xtq3_put=x3[2])
end

calc_tsx_in_df(price_lookup) = function(tss, xpirtss, styles, strikes, bids, asks)
    ts = first(tss)
    @assert all(ts .== tss)
    xpirts = first(xpirtss)
    @assert all(xpirts .== xpirtss)
    style = first(styles)
    @assert all(style .== styles)
    price_xpirts = xpirts > now(UTC) ? NaN : price_lookup[xpirts]
    calc_tsx(ts, xpirts, price_lookup[ts], price_xpirts, style, strikes, bids, asks)
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
    mid = length(xs) ÷ 2
    mid_inds = (mid-1):(mid+2)
    xs_mid = xs[mid_inds]
    @assert xs_mid[2] <= 0
    @assert xs_mid[3] >= 0
    ys_mid = ys[mid_inds]

    # mid_inds = (mid-1):(mid+2)

    # mid_ps = LsqFit.curve_fit(vertex_model, xs[mid_inds], ys[mid_inds], [0.00001,1000.0]; lower=[0.0,0.0], upper=[max_y, Inf]).param
    # mid_ps = fit(vertex_model, xs[mid_inds], ys[mid_inds], max_y)

    # ys_mid = ys[mid_inds]
    # ys_mid[1] = mean(ys[1:(mid-2)])
    # ys_mid[end] = mean(ys[(mid+2):end])
    # # @show ys_mid
    # @assert ys_mid[end] < ys_mid[end-1]
    # @assert ys[1] < ys[2]

    # x1 = xs[2]
    # x2 = xs[3]
    # y1 = ys[2]
    # y2 = ys[3]
    # (;yatz, a, b) = findab(xs[mid_inds]..., ys[mid_inds]...)
    yatz = find_yatz(xs_mid, ys_mid)

    # mid_ps = fit_vm(xs[mid_inds], ys_mid, max_y)
    # yatz = vertex_model(0.0, mid_ps)


    # if yatz <= maximum(ys)
    #     global kfit_sides = (;mid, mid_inds, ys_mid, mid_ps, yatz, args=(;xs, ys, style, max_y))
    # end
    # @show mid_ps yatz
    # @assert mid_ps[1] > maximum(ys_mid)

    if yatz < maximum(ys)
        global kfit_sides = (;mid, mid_inds, xs_mid, ys_mid, yatz, args=(;xs, ys, style, max_y))
        # TODO: don't do this workaround and fix algo so unneeded
        # println("WARN: yatz too low $(yatz) $(maximum(ys))")
        yatz = maximum(ys)
    end
    if yatz > 1.4 * maximum(ys)
        global kfit_sides = (;mid, mid_inds, xs_mid, ys_mid, yatz, args=(;xs, ys, style, max_y))
        # TODO: don't do this workaround and fix algo so unneeded
        # println("WARN: yatz too high $(yatz) $(maximum(ys))")
        yatz = 1.4 * maximum(ys)
    end

    @assert yatz >= maximum(ys)
    @assert yatz <= 1.4 * maximum(ys)
    # xtqs = insert!(copy(ys), mid+1, yatz)
    # @show xtqs
    return [ys_mid[2], yatz, ys_mid[3]]

    # sm = side_model(yatz)
    # if style == -1
    #     left_inds = 1:mid
    #     right_inds = (mid+1):NTM_COUNT3
    #     left_xs = vcat(xs[left_inds], 0.0)
    #     left_ys = vcat(ys[left_inds], yatz)
    #     left_ps = fit(sm, left_xs, left_ys)

    #     right_xs = vcat(0.0, xs[right_inds])
    #     right_ys = vcat(yatz, ys[right_inds])
    #     right_ps = fit(sm, right_xs, right_ys)

    #     left_1 = sm(-XTQ_RADIUS, left_ps)
    #     right_1 = sm(XTQ_RADIUS, right_ps)
    #     return (;xtqs=[left_1, yatz, right_1], left_ps, right_ps)
    # else
    #     right_inds = 1:mid
    #     left_inds = (mid+1):NTM_COUNT3
    #     right_xs = vcat(0.0, xs[right_inds])
    #     right_ys = vcat(yatz, ys[right_inds])
    #     right_ps = fit(sm, right_xs, right_ys)

    #     left_xs = vcat(xs[left_inds], 0.0)
    #     left_ys = vcat(ys[left_inds], yatz)
    #     left_ps = fit(sm, left_xs, left_ys)

    #     right_1 = sm(XTQ_RADIUS, right_ps)
    #     left_1 = sm(-XTQ_RADIUS, left_ps)
    #     return (;xtqs=[left_1, yatz, right_1], left_ps, right_ps)
    # end
end

# function check(xs, ys, res)
#     (;xtqs, left_ps, right_ps) = res
#     yatz = xtqs[2]
#     sm = side_model(yatz)
#     left_xs = (2*xs[1]):0.0001:0.0
#     right_xs = 0.0:0.0001:(2*xs[end])
#     draw(:vlines, 0.0)
#     draw!(:scatter, xs, ys; label="points")
#     draw!(:lines, left_xs, [sm(x, left_ps) for x in left_xs]; label="left")
#     draw!(:lines, right_xs, [sm(x, right_ps) for x in right_xs]; label="right")
# end

# import LsqFit
# vertex_model(x, p) = @. p[1] * exp(-p[2] * x^2)
# # side_model(x, p) = @. p[1] / (1 + p[2] * x^2)
# # side_model(yatz) = (x, p) -> @. yatz / (1 + p[1] * x^2 + p[2] * x^3 + p[3] * x^4)
# side_model(yatz) = (x, p) -> @. yatz / (1 + p[1] * x^2)
# sminit() = [1.0]
# smlower() = [0.0]
# # smweights() = [1.0, 0.9, 0.8, 0.7, 0.6]
# # smweights() = [1.0, 1.0, 0.0, 0.0, 0.0]
# # smweights() = fill(NTM_COUNT3 ÷ 2 + 1)
# # fit(model, xs, ys) = LsqFit.curve_fit(model, xs, ys, smweights(), sminit()).param
# fit(model, xs, ys) = LsqFit.curve_fit(model, xs, ys, sminit(); lower=smlower()).param

# # vm(xs, ps) = @. log(ps[1]) - ps[2] * xs^2
# function vm(xs, ps)
#     # if ps[1] < 0.0
#     #     @show xs ps
#     #     error("bad ps")
#     # end
#     @. log(ps[1]) - ps[2] * xs^2
#     # @. ps[1] - ps[2] * xs^2
# end
# function fit_vm(xs, ys, max_y)
#     ys2 = [log(max(1e-8, y)) for y in ys]
#     # @show xs ys ys2
#     # return LsqFit.curve_fit(vm, xs, ys2, [0.0001,1.0,1.0,0.0001], [log(maximum(ys)), 1000.0]).param#; lower=[0.0, 1.0]).param
#     return LsqFit.curve_fit(vm, xs, ys2, [0.0001,1.0,1.0,0.0001], [maximum(ys), 1000.0]).param #; lower=[0.0, 1.0]).param
# end
# function it(xs, ys)
#     mleft = (ys[2] - ys[1]) / (xs[2] - xs[1])
#     mright = (ys[4] - ys[3]) / (xs[4] - xs[3])
#     @show mleft mright
#     b = log((xs[2] * mright) / (xs[3] * mleft)) / (xs[2]^2 - xs[3]^2)

#     a = exp(-b * xs[2]^2) / ys[2]
#     a2 = exp(-b * xs[3]^2) / ys[3]
#     # a = m / (-2 * b * x2 * e^(-b * x2 ^ 2))
#     a3 = mleft / (-2 * b * xs[2] * exp(-b * xs[2]^2))

#     return (a, a2, a3, b)
# end
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

===========================

Find a and b such that
y = a * e^(-b * x^2)
passes through x1, y1 and x2, y2.
Two equations and two unknowns:
y1 = a * e^(-b * x1^2)
y2 = a * e^(-b * x2^2)
1) Solve for a in the first equation
a = y1 / e^(-b * x1^2)
2) Substitute for a in the second equation
y2 = (y1 / e^(-b * x1^2)) * e^(-b * x2^2)
and solve for b
n = (m / e^(-b * j^2)) * e^(-b * k^2)
n = m e^(b (j^2 - k^2))
log(n) - log(m) = b * (j^2 - k^2)
b = log(n/m) / (j^2 - k^2)
??? b = log(m/n) / (k^2 - j^2)
3) enter actuals for b value:
b = log(y2/y1) / (x1^2 - x2^2)
b = log(0.000510909854206082 / 0.0005074596441491433) / ((-0.0010197162628173828)^2 - 0.000868678092956543^2)
b = 23757.044035799776
4) Use that value for b in original equation with both points, solve for a, and make sure they both match
a = y1 / e^(-b * x1^2)
a = y2 / e^(-b * x2^2)

The problem is that the closer the y's are together, it tries to make a flat line which is not what we want.
=#

# function findab(x1, x2, y1, y2)
#     b = log(y2/y1) / (x1^2 - x2^2)
#     @assert b > 0
#     a = y1 / exp(-b * x1^2)
#     a2 = y2 / exp(-b * x2^2)
#     @assert a ≈ a2
#     yatz = a
#     return (;yatz, a, b)
# end

# import DrawUtil:draw,draw!
# function checkab(kf)
#     x1, x2 = kf.xs_mid
#     y1, y2 = kf.ys_mid
#     b = log(y2/y1) / (x1^2 - x2^2)
#     if b < 0 && abs(1 - y2 / y1) < 1e-4
#         b2 = abs(1 / (x1^2 - x2^2))
#         @show b2
#     else
#         b2 = -b
#     end
#     # b2 = log(y2/y1) / (x1^2 - x2^2)
#     # @assert b > 0
#     a = y1 / exp(-b * x1^2)
#     a2 = y2 / exp(-b2 * x2^2)
#     # a3 = y2 / exp(b * x2^2)
#     # @assert a ≈ a2
#     f = x -> a * exp(-b * x^2)
#     f2 = x -> a2 * exp(-b2 * x^2)

#     axs = (2*x1):0.00001:(2*x2)
#     draw(:vlines, 0.0)
#     draw!(:scatter, kf.xs_mid, kf.ys_mid; label="points")
#     draw!(:lines, axs, [f(x) for x in axs]; label="fit")
#     draw!(:lines, axs, [f2(x) for x in axs]; label="-fit")
#     yatz = a
#     return (;yatz, a, b)
# end

#=
slopes
y = a * e^(-b * x^2)
y = a * e^(-b * x^2)
dy/dx = -2 * a * b * x * e^(-b * x^2)
m1 = -2 * a * b * x1 * e^(-b * x1^2)
m2 = -2 * a * b * x2 * e^(-b * x2^2)
m = -2 * a * b * j * e^(-b * j^2)
n = -2 * a * b * k * e^(-b * k^2)
a = -(m e^(b j^2))/(2 b j) and b j!=0
n = -2 * (-(m e^(b j^2))/(2 b j)) * b * k * e^(-b * k^2)
n = (k m e^(b (j^2 - k^2)))/j
b = log((j n)/(k m))/(j^2 - k^2)
a1 = -(m e^(b j^2))/(2 b j)
a2 = -(n e^(b k^2))/(2 b k)
--------

It won't go through the points and the points aren't symmetrical anyway, but the function is.
=#

# function findab(xs, ys)
#     dy1 = (ys[2] - ys[1]) / (xs[2] - xs[1])
#     dy2 = (ys[4] - ys[3]) / (xs[4] - xs[3])
#     x1 = xs[2]
#     x2 = xs[3]
#     b = log((x1 * dy2)/(x2 * dy1))/(x1^2 - x2^2)
#     a1 = -(dy1 * exp(b * x1^2))/(2 * b * x1)
#     a2 = -(dy2 * exp(b * x2^2))/(2 * b * x2)
#     @show b a1 a2
#     @assert a1 ≈ a2
#     return (;a=a1, b)
# end

# import DrawUtil:draw,draw!
# function checkab(kf)
#     mi = 3:6
#     xs = kf.args.xs[mi]
#     ys = kf.args.ys[mi]
#     (;a, b) = findab(xs, ys)
#     f = x -> a * exp(-b * x^2)

#     axs = (2*xs[1]):0.00001:(2*xs[end])
#     draw(:vlines, 0.0)
#     draw!(:scatter, xs, ys; label="points")
#     draw!(:lines, axs, [f(x) for x in axs]; label="fit")
#     yatz = a
#     return (;yatz, a, b)
# end

#=
fit lines to the style side

=#

# function fit_line(xs, ys)
#     X = [xs ones(length(xs))]
#     m, b = X \ ys
#     return (;m, b)
# end

# function fit_lines(xs, ys)
#     # puts use left, calls use right
#     # if style == -1

#     (mleft, bleft) = fit_line(xs[1:4], ys[1:4])
#     (mright, bright) = fit_line(xs[5:8], ys[5:8])
#     return (;mleft, bleft, mright, bright)
# end

# function find_yatz(xs, ys, style)
#     (;mleft, bleft, mright, bright) = fit_lines(xs, ys)
#     # return style == 1 ? bright : bleft
#     return max(bleft, bright)
# end

# import DrawUtil:draw,draw!
# function check_yatz(kf)
#     xs = kf.args.xs
#     ys = kf.args.ys
#     (;mleft, bleft, mright, bright) = fit_lines(xs, ys)
#     axs = (2*xs[1]):0.00001:(2*xs[end])
#     draw(:vlines, 0.0)
#     draw!(:scatter, xs, ys; label="points")
#     draw!(:lines, axs, [mleft*x + bleft for x in axs]; label="left")
#     draw!(:lines, axs, [mright*x + bright for x in axs]; label="right")
#     yatz = a
#     return (;yatz, a, b)
# end

function find_yatz(xs_mid, ys_mid)
    if xs_mid[2] ≈ 0.0
        return ys_mid[2]
    elseif xs_mid[3] ≈ 0.0
        return ys_mid[3]
    end
    mleft = (ys_mid[2] - ys_mid[1]) / (xs_mid[2] - xs_mid[1])
    mright = (ys_mid[4] - ys_mid[3]) / (xs_mid[4] - xs_mid[3])
    mpos = (mleft - mright) / 2
    ind = ys_mid[3] > ys_mid[2] ? 3 : 2
    yatz = ys_mid[ind] + mpos * abs(xs_mid[ind])
    return yatz
end

# import DrawUtil:draw,draw!
# function check_yatz(kf)
#     xs = kf.args.xs
#     ys = kf.args.ys
#     yatz = find_yatz(xs, ys)
#     draw(:vlines, 0.0)
#     draw!(:scatter, xs, ys; label="points")
#     draw!(:scatter, 0.0, yatz; label="yatz")
#     return yatz
# end

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

# import DrawUtil
# function drawpoly(xpts, ypts, ps; n=false)
#     n && DrawUtil.draw(:vlines, 0.0)
#     DrawUtil.draw!(:scatter, xpts, ypts)
#     width = xpts[end] - xpts[1]
#     left = xpts[1] - width
#     right = xpts[end] + width
#     xs = left:(width/100):right
#     DrawUtil.draw!(:lines, xs, model(xs, ps))
# end

# function drawpoly2(xpts, ypts, left_ps, right_ps; n=false)
#     n && DrawUtil.draw(:vlines, 0.0)
#     DrawUtil.draw!(:scatter, xpts, ypts; label="data")
#     width = xpts[end] - xpts[1]
#     left = xpts[1] - width
#     right = xpts[end] + width
#     left_xs = left:(width/50):0.0
#     right_xs = 0.0:(width/50):right
#     DrawUtil.draw!(:lines, left_xs, model(left_xs, left_ps); label="left")
#     DrawUtil.draw!(:lines, right_xs, model(right_xs, right_ps); label="right")
# end

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