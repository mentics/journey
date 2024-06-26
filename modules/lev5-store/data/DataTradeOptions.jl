module DataTradeOptions
using Dates, DataFrames
using ThreadPools
import StatsBase:mean
using ThetaData, Paths
import DateUtil, PricingBase, OptionUtil
using DataConst, DataRead, DataXpirts, DataPrices
import Calendars as cal
import DataCheck

const KEY_COLS = [:ts, :expir, :style, :strike]

function validate_result(df)
    @assert issorted(df, KEY_COLS)
    @assert allunique(df, KEY_COLS)
end

#region Standard Api
function make_trade_options(;sym="SPY", max_dist=16, age=nothing)
    MAX_DIST_FROM_PRICE[] = max_dist # max(8, SPREAD_DIST[] + SPREAD_WIDTH[])
    price_lookup = DataRead.price_lookup(;age)

    (;avail) = DataRead.get_options_yms()
    yms = avail
    # yms = [(2020,1)]
    stop = false
    try
        dfs = qbmap(yms) do (year, month)
            !stop || return
            println("processing $year $month")
            proc(DataRead.get_options(year, month; sym), price_lookup)
        end
        df = reduce(vcat, dfs)
        sort!(df, KEY_COLS)
        validate_result(df)
        Paths.save_data(DataRead.file_trade_options(;sym), df; update=true)
        return df
    finally
        stop = true
    end
end

function update_trade_options(;sym="SPY")
    df1 = DataRead.get_trade_options(;age=DateUtil.FOREVER2)
    tss = tss_for_tsx()
    if tss[end] > df1.ts[end]
        # price_lookup = DataRead.price_lookup()
        # ind = searchsortedfirst(tss, df1.ts[end] + Second(5))
        # to_proc = tss[ind:end]
        # println("To process: $(to_proc)")
        # yms = DateUtil.year_months.(;start_date=Date(to_proc[1]), end_date=Date(to_proc[end]))
        # df1 = filter(:ts => (ts -> !( (year(ts), month(ts)) in yms )), df1)
        # df2 = mapreduce(vcat, yms) do (year, month)
        #     df = filter(:ts => (ts -> to_proc[1] <= ts <= to_proc[end]), filtered_options(year, month; sym))
        #     proc2(df, price_lookup)
        # end
        # df = DataCheck.combine_dfs(df1, df2; keycols=[:ts,:expir])
        # Paths.save_data(DataRead.file_tsx(;sym), df; update=true)
        # diff = symdiff(df.ts, tss)
        # if !isempty(diff)
        #     return diff
        # end
        # return to_proc
    else
        println("DataTradeOptions already up to date $(df1.ts[end]) == $(tss[end])")
        touch(DataRead.file_trade_options(;sym))
    end
end

# function reproc_tsx(yms;sym="SPY")
#     df1 = DataRead.get_tsx(;age=DateUtil.FOREVER2)
#     price_lookup = DataRead.price_lookup()
#     df1 = filter(:ts => (ts -> !( (year(ts), month(ts)) in yms )), df1)
#     df2 = mapreduce(vcat, yms) do (year, month)
#         proc2(filtered_options(year, month; sym), price_lookup)
#     end
#     df = DataCheck.combine_dfs(df1, df2; keycols=[:ts,:expir])
#     validate_result(df)
#     Paths.save_data(DataRead.file_tsx(;sym), df; update=true)

#     # tss = filter(ts -> (year(ts), month(ts)) in yms, tss_for_tsx())
#     diff = symdiff(df.ts, tss_for_tsx())
#     if !isempty(diff)
#         return diff
#     end
#     return to_proc
# end
#endregion Standard Api

#region Local
function proc(df, price_lookup)
    threads = false
    df = filter([:ts,:expir] => filter_trade_options, df)
    gdf = groupby(df, [:ts, :expir, :style])
    df = combine(gdf, [:ts,:expir,:style,:strike,:bid,:ask] => filter_ntm(price_lookup) => [:strike,:bid,:ask,:xpir_value,:price_ts,:price_xpir]; threads)
    return df
end

function filter_trade_options(ts, xpirts)
    date = Date(ts)
    xpir = Date(xpirts)
    return ts != cal.getMarketOpen(Date(ts)) &&
        ts != cal.getMarketClose(Date(ts)) &&
        DateUtil.bdays(date, xpir) in [1,2,4,8,16,32]
end

filter_ntm(price_lookup) = function(tss, xpirtss, styles, strikes_in, bids, asks)
    ts = first(tss)
    xpirts = first(xpirtss)
    style = first(styles)
    ts_price = Float32(haskey(price_lookup, ts) ? price_lookup[ts] : NaN32);
    left_of_mid = searchsortedlast(strikes_in, ts_price)
    left, right, _ = get_radius(length(strikes_in), left_of_mid)
    strikes = strikes_in[left:right]
    @assert iseven(length(strikes))
    xpirts_price = Float32((xpirts < now(UTC) && haskey(price_lookup, xpirts)) ? price_lookup[xpirts] : NaN32)
    xpir_values = OptionUtil.value_at_xpir.(style, strikes, xpirts_price)
    out_count = length(strikes)

    return (;strikes, bids=bids[left:right], asks=asks[left:right], xpir_values, price_ts=fill(ts_price, out_count), price_xpir=fill(xpirts_price, out_count))
end
function get_radius(len, left_of_mid)
    radius = min(left_of_mid, len - left_of_mid, MAX_DIST_FROM_PRICE[])
    left = left_of_mid - radius + 1
    right = left_of_mid + radius
    @assert left >= 1 && right <= len
    return left, right, radius
end
#endregion Local

#region Explore
using SmallTypes, QuoteTypes, OptionTypes, OptionMetaTypes, OptionQuoteTypes, LegQuoteTypes
import SH

# dist + width must be <= MAX_DIST_FROM_PRICE
const MAX_DIST_FROM_PRICE = Ref(8)
const PRICE_RATIO = Ref(1f0)
const SPREAD_DIST = Ref(1)
const SPREAD_WIDTH = Ref(1)
const MAX_BID_ASK_SPREAD = Ref(0.1f0)

function explore(fproc)
    global kcount_spread = 1
    global kcount_odd = 1
    # global track_skip = []
    df = DataRead.get_trade_options(;age=DateUtil.FOREVER2)
    gdf = groupby(df, [:ts,:expir,:style])
    dfc = combine(gdf, [:ts,:expir,:style,:strike,:bid,:ask,:xpir_value] => fproc => [:pnl_bid, :pnl_mid, :pnl_ask])
    global kdfc = dfc
    println("calls:")
    sho(dfc[dfc.style .== 1,:])
    println("puts:")
    sho(dfc[dfc.style .== -1,:])
    return dfc
end

function sho(df)
    pnl_bid = sum(df.pnl_bid)
    pnl_mid = sum(df.pnl_mid)
    pnl_ask = sum(df.pnl_ask)
    @show pnl_bid pnl_mid pnl_ask
end

function single(tss, xpirtss, styles, strikes, bids, asks, xpir_values)
    EMPTY = (;pnl_bid=0f0, pnl_mid=0f0, pnl_ask=0f0)
    all(asks .> bids) || return EMPTY
    # all( (asks .- bids) .< MAX_BID_ASK_SPREAD[] ) || ( global kcount_spread += 1 ; return EMPTY )
    global kargs = (;tss, xpirtss, styles, strikes, bids, asks, xpir_values)

    left_of_mid = length(strikes) ÷ 2
    left_of_mid > 1 || return EMPTY
    style = first(styles)
    style == -1 || return EMPTY
    xpir = DateUtil.market_date(first(xpirtss))
    off = [-1]
    inds = [left_of_mid + x for x in off]
    sides = [Side.short]

    lqs = map(inds, sides) do ind, side
        oq = toq(style, xpir, strikes[ind], bids[ind], asks[ind])
        return LegQuoteOpen(oq, side)
    end
    global klqs = lqs
    # lqs = lqs[1:4]
    neto_bid = sum(SH.getBid.(lqs))
    # neto_bid > 0.05 || return EMPTY
    neto_ask = sum(SH.getAsk.(lqs))
    netc = sum(Int.(sides) .* xpir_values[inds])
    neto_mid = sum(SH.getAsk.(lqs) .- 0.01)
    res = (;pnl_bid=(neto_bid + netc), pnl_mid=(neto_mid + netc), pnl_ask=(neto_ask + netc))
    # if res.pnl_bid < 0
    # end
    # @show res
    # error("stop")
    return res
end

function two(tss, xpirtss, styles, strikes, bids, asks, xpir_values)
    EMPTY = (;pnl_bid=0f0, pnl_mid=0f0, pnl_ask=0f0)
    # all(asks .> bids) || return EMPTY
    # all( (asks .- bids) .< MAX_BID_ASK_SPREAD[] ) || ( global kcount_spread += 1 ; return EMPTY )
    global kargs = (;tss, xpirtss, styles, strikes, bids, asks, xpir_values)

    left_of_mid = length(strikes) ÷ 2
    left_of_mid > 8 || return EMPTY
    style = first(styles)
    style == -1 || return EMPTY
    xpir = DateUtil.market_date(first(xpirtss))
    # off = [-1]
    inds = [max(1, left_of_mid - 7), left_of_mid - 6]
    sides = [Side.long, Side.short]

    lqs = map(inds, sides) do ind, side
        oq = toq(style, xpir, strikes[ind], bids[ind], asks[ind])
        return LegQuoteOpen(oq, side)
    end
    global klqs = lqs
    neto_bid = sum(SH.getBid.(lqs))
    neto_bid > 0.05 || return EMPTY
    neto_ask = sum(SH.getAsk.(lqs))
    netc = sum(Int.(sides) .* xpir_values[inds])
    neto_mid = sum(SH.getAsk.(lqs) .- 0.01)
    res = (;pnl_bid=(neto_bid + netc), pnl_mid=(neto_mid + netc), pnl_ask=(neto_ask + netc))
    # if res.pnl_bid < 0
    # end
    # @show res
    # error("stop")
    return res
end

function three(tss, xpirtss, styles, strikes, bids, asks, xpir_values)
    # len = length(strikes)
    left_of_mid = length(strikes) ÷ 2
    left_of_mid > 10 || return (;pnl_bid=0f0, pnl_ask=0f0)
    style = first(styles)
    xpir = DateUtil.market_date(first(xpirtss))
    off = (2,1,0)
    inds = [left_of_mid + off[1], left_of_mid + off[2], left_of_mid + off[3]]
    sides = (Side.long, Side.long, Side.short)
    lqs = map(inds, sides) do ind, side
        oq = toq(style, xpir, strikes[ind], bids[ind], asks[ind])
        return LegQuoteOpen(oq, side)
    end
    global klqs = lqs
    neto_bid = sum(SH.getBid.(lqs))
    neto_bid > 0 || return (;pnl_bid=0f0, pnl_ask=0f0)
    neto_ask = sum(SH.getAsk.(lqs))
    netc = sum(Int.(sides) .* xpir_values[inds])
    res = (;pnl_bid=(neto_bid + netc), pnl_ask=(neto_ask + netc))
    # @show res
    # error("stop")
    return res
end

function four(tss, xpirtss, styles, strikes, bids, asks, xpir_values)
    EMPTY = (;pnl_bid=0f0, pnl_mid=0f0, pnl_ask=0f0)
    all(asks .> bids) || return EMPTY
    all( (asks .- bids) .< MAX_BID_ASK_SPREAD[] ) || ( global kcount_spread += 1 ; return EMPTY )

    global kargs = (;tss, xpirtss, styles, strikes, bids, asks, xpir_values)
    left_of_mid = length(strikes) ÷ 2
    left_of_mid > 4 || return EMPTY
    style = first(styles)
    xpir = DateUtil.market_date(first(xpirtss))
    off = (-2,-1,-1,0)
    # off = (-3,-2,-2,-1)
    # off = (-4,-3,-3,-2)
    # off = (-4,-2,-2,0)
    inds = [left_of_mid + off[1], left_of_mid + off[2], left_of_mid + off[3], left_of_mid + off[4]]

    strikes_use = strikes[inds]
    min_strike, min_ind = findmin(strikes_use)
    mults = off .- minimum(off)
    check = (strikes_use .- min_strike) ./ mults
    global kcheck = check
    if !(isnan(check[min_ind]) && allequal(check[Not(min_ind)]))
        # println("found odd ball")
        # @show strikes_use
        global kcount_odd += 1
        return EMPTY
    end

    sides = (Side.short, Side.long, Side.long, Side.short)
    lqs = map(inds, sides) do ind, side
        oq = toq(style, xpir, strikes[ind], bids[ind], asks[ind])
        return LegQuoteOpen(oq, side)
    end
    neto_bid = sum(SH.getBid.(lqs))
    # neto_bid > 0.05 || return EMPTY
    neto_ask = sum(SH.getAsk.(lqs))
    netc = sum(Int.(sides) .* xpir_values[inds])
    neto_mid = sum(SH.getAsk.(lqs) .- 0.01)
    res = (;pnl_bid=(neto_bid + netc), pnl_mid=(neto_mid + netc), pnl_ask=(neto_ask + netc))
    if res.pnl_bid < 0
        global klqs = lqs
    end
    # @show res
    # error("stop")
    return res
end

function double(tss, xpirtss, styles, strikes, bids, asks, xpir_values)
    EMPTY = (;pnl_bid=0f0, pnl_mid=0f0, pnl_ask=0f0)
    all(asks .> bids) || return EMPTY
    # all( (asks .- bids) .< MAX_BID_ASK_SPREAD[] ) || ( global kcount_spread += 1 ; return EMPTY )
    global kargs = (;tss, xpirtss, styles, strikes, bids, asks, xpir_values)

    left_of_mid = length(strikes) ÷ 2
    left_of_mid > 4 || return EMPTY
    style = first(styles)
    xpir = DateUtil.market_date(first(xpirtss))
    off = [-3,-2,-2,-1,2,3,3,4]
    inds = [left_of_mid + x for x in off]
    sides = [Side.short, Side.long, Side.long, Side.short, Side.short, Side.long, Side.long, Side.short]

    strikes_use = strikes[inds]
    min_strike, min_ind = findmin(strikes_use)
    mults = off .- minimum(off)
    check = (strikes_use .- min_strike) ./ mults
    global kcheck = check
    if !(isnan(check[min_ind]) && allequal(check[Not(min_ind)]))
        # println("found odd ball")
        # @show strikes_use
        global kcount_odd += 1
        return EMPTY
    end

    lqs = map(inds, sides) do ind, side
        oq = toq(style, xpir, strikes[ind], bids[ind], asks[ind])
        return LegQuoteOpen(oq, side)
    end
    global klqs = lqs
    # lqs = lqs[1:4]
    neto_bid = sum(SH.getBid.(lqs))
    neto_bid > 0.05 || return EMPTY
    neto_ask = sum(SH.getAsk.(lqs))
    netc = sum(Int.(sides) .* xpir_values[inds])
    neto_mid = sum(SH.getAsk.(lqs) .- 0.01)
    res = (;pnl_bid=(neto_bid + netc), pnl_mid=(neto_mid + netc), pnl_ask=(neto_ask + netc))
    # if res.pnl_bid < 0
    # end
    # @show res
    # error("stop")
    return res
end

toq(style, xpir, strike, bid, ask) = OptionQuote(Option(Style.T(style), xpir, strike), Quote(bid, ask), OptionMetaTypes.MetaZero)

function explore()
    df = DataRead.get_trade_options(;age=DateUtil.FOREVER2)
    gdf = groupby(df, [:ts,:expir,:style])
    dfc = combine(gdf, [:ts,:expir,:style,:strike,:bid,:ask,:xpir_value] => proc_spread => [:bid_ask, :pnl_long_under, :pnl_short_under, :pnl_long_over, :pnl_short_over])

    for bid_ask in [:bid,:ask], dir in [:long, :short], side in [:under, :over], style in [1, -1]
        values = to(dfc, bid_ask, dir, side, style)
        print("Result for (:$(bid_ask), :$(dir), :$(side), $(style)): ")
        println(sum(values))
    end

    global kdfc = dfc
    return dfc
end

# function show_result(df)
#     println((;LU=sum(df.pnl_long_under), SU=sum(df.pnl_short_under), LO=sum(df.pnl_long_over), SO=sum(df.pnl_short_over)))
# end

function proc_spread(tss, xpirtss, styles, strikes, bids, asks, xpir_values)
    EMPTY = [0f0,0f0]
    # if length(strikes) < 2*SPREAD_DIST[] + 2*SPREAD_WIDTH[]
    #     error("Skipping $(first(tss)) $(first(xpirtss)) with strike length $(length(strikes)) < $(2*SPREAD_DIST[] + 2*SPREAD_WIDTH[])")
    #     return (;bid_ask=[:bid,:ask],pnl_long_under=EMPTY, pnl_short_under=EMPTY, pnl_long_over=EMPTY, pnl_short_over=EMPTY)
    # end
    return calc_row(bids, asks, xpir_values)
end

function calc_row(bids, asks, xpir_values)
    EMPTY = [0f0,0f0]
    ok = [asks[i] > bids[i] && (asks[i] - bids[i]) <= MAX_BID_ASK_SPREAD[] for i in eachindex(bids)]
    len = length(bids)
    left1 = len ÷ 2 - SPREAD_DIST[] - SPREAD_WIDTH[] + 1
    right1 = left1 + SPREAD_WIDTH[]
    left2 = len ÷ 2 + SPREAD_DIST[]
    right2 = left2 + SPREAD_WIDTH[]
    if left1 < 1 || right2 > len
        @show len left1 right1 left2 right2
        # error("Skipping $(first(tss)) $(first(xpirtss)) with strike length $(length(strikes)) < $(2*SPREAD_DIST[] + 2*SPREAD_WIDTH[])")
        println("Skipping strike length $(len) < $(2*SPREAD_DIST[] + 2*SPREAD_WIDTH[])")
        return (;bid_ask=[:bid,:ask], pnl_long_under=EMPTY, pnl_short_under=EMPTY, pnl_long_over=EMPTY, pnl_short_over=EMPTY)
    end
    # @show len left1 right1 left2 right2
    # error("stop")
    ok_under = ok[left1] && ok[right1]
    ok_over = ok[left2] && ok[right2]
    pnl_long_under = ok_under ? calc_spread_pnl(bids, asks, xpir_values, left1, right1) : EMPTY
    pnl_short_under = ok_under ? calc_spread_pnl(bids, asks, xpir_values, right1, left1) : EMPTY
    pnl_long_over = ok_over ? calc_spread_pnl(bids, asks, xpir_values, left2, right2) : EMPTY
    pnl_short_over = ok_over ? calc_spread_pnl(bids, asks, xpir_values, right2, left2) : EMPTY
    # @show typeof.([pnl_long_under, pnl_short_under, pnl_long_over, pnl_short_over])
    # @assert all(x -> x == Vector{Float32}, typeof.([pnl_long_under, pnl_short_under, pnl_long_over, pnl_short_over]))
    return (;bid_ask=[:bid,:ask], pnl_long_under, pnl_short_under, pnl_long_over, pnl_short_over)
end

function calc_spread_pnl(bids, asks, xpir_values, long, short)
    EMPTY = [0f0,0f0]

    # long_spread = asks[long] - bids[long]
    # short_spread = asks[short] - bids[short]
    neto_bid = price_short(bids[short], asks[short]) + price_long(bids[long], asks[long])

    neto_ask = asks[short] - bids[long]
    (neto_bid > 0 && neto_ask > 0) || return EMPTY
    netc = xpir_values[long] - xpir_values[short]
    pnl_bid = neto_bid + netc
    pnl_ask = neto_ask + netc
    return [pnl_bid, pnl_ask]
end

price_long(bid, ask) = -ask + PRICE_RATIO[] * (ask - bid)
price_short(bid, ask) = bid + PRICE_RATIO[] * (ask - bid)

function to(df, bid_ask, dir, side, style=nothing)
    df = df[df.bid_ask .== bid_ask,:]
    isnothing(style) || ( df = df[df.style .== style,:] )
    sym = Symbol("pnl_$(dir)_$(side)")
    return df[:,sym]
end
#endregion Explore

#region Half
import Explore as ore
function explore_half(;hist_len=100)
    dfto = DataRead.get_trade_options(;age=nothing)
    df = filter(:style => ==(-1), dfto)
    df = filter!([:price_ts, :strike, :bid, :ask] =>
        (price_ts, strike, bid, ask) ->
            ask - bid < 0.12 && abs(strike - price_ts) < 8.0,
        df)
    df.long = ore.price_open.(Side.long, df.bid, df.ask)
    df.short = ore.price_open.(Side.short, df.bid, df.ask)

    dfhist = unique(df, [:ts])
    dfhist.hist = rolling_extrema(dfhist.price_ts, hist_len)
    select!(dfhist, :ts, :hist)
    leftjoin!(df, dfhist; on=:ts)

    global kdf = df
    res = []
    dfs = []
    for days_to_xpir in [1,2,4,8,16,32]
        dfin = filter([:ts, :expir] =>
            (ts, xpirts) ->
                DateUtil.bdays(Date(ts), Date(xpirts)) == days_to_xpir,
            df)
        gdf = groupby(dfin, [:ts, :expir])
        for spread in [1,2,3,4,5,6,7]
            for neto_target in [0.1f0, Float32(spread / 4), Float32(spread / 2), Float32(3 * spread / 4), Float32(spread - 0.1f0)]
                f = proc_half(spread, neto_target)
                dfc = combine(gdf, [:ts,:expir,:style,:strike,:long,:short,:xpir_value,:price_ts,:price_xpir,:hist] => f => [:strike_long,:strike_short,:neto,:netc,:pnl,:price_ts,:price_xpir,:risk])
                filter!([:pnl, :price_xpir, :risk] => ((pnl, price_xpir, risk) -> isfinite(pnl) && isfinite(price_xpir) && risk > 0f0), dfc)
                if !isempty(dfc)
                    push!(res, (;days_to_xpir, spread, neto_target, balance_end=calc_balances(dfc.pnl, dfc.risk, days_to_xpir)[end]))
                    push!(dfs, dfc)
                end
            end
        end
    end
    s = sortperm(res; rev=true, by=x -> x.balance_end)
    res = res[s]
    dfs = dfs[s]
    println(res[1])
    draw(:scatter, dfs[1].ts, calc_balances(dfs[1].pnl, dfs[1].risk, res[1].days_to_xpir))
    println(res[2])
    draw!(:scatter, dfs[2].ts, calc_balances(dfs[2].pnl, dfs[2].risk, res[2].days_to_xpir))
    return res, dfs
end
import DrawUtil:draw,draw!

calc_rets(pnls, risks, days_to_xpir) = clamp.(pnls ./ risks ./ days_to_xpir, -1.0, 100.0)
calc_balances(pnls, risks, days_to_xpir) = calc_balances(calc_rets(pnls, risks, days_to_xpir))
calc_balances(rets) = accumulate(*, 1.0 .+ (0.01 .* rets); init=1.0)

# all_match(v) = !all(x -> x==hists[end] || ( all(isnan.(x)) && all(isnan.(hists[end])) ), hists)

proc_half(spread, neto_target) = function(tss, xpirtss, styles, strikes, longs, shorts, xpir_values, price_tss, price_xpirs, hists)
    EMPTY = (;strike_long=0f0, strike_short=0f0, neto=0f0, netc=0f0, pnl=0f0, price_ts=0f0, price_xpir=0f0, risk=0f0)
    !isempty(tss) || return EMPTY
    # @assert price_tss[end] == hists[end]
    @assert all(==(tss[end]), tss)
    @assert all(==(xpirtss[end]), xpirtss)
    if !all(isequal(price_xpirs[end]), price_xpirs)
        global kargs = (;tss, xpirtss, styles, strikes, longs, shorts, xpir_values, price_tss, price_xpirs, hists)
        error("!all(==(price_xpirs[end]), price_xpirs)")
    end
    @assert all(==(price_tss[end]), price_tss)
    # if !all(x -> x==hists[end] || ( all(isnan.(x)) && all(isnan.(hists[end])) ), hists)
    if !all(isequal(hists[end]), hists)
        global kargs = (;tss, xpirtss, styles, strikes, longs, shorts, xpir_values, price_tss, price_xpirs, hists)
        error("all(==(hists[end]), hists)")
    end
    # @assert all(==(hists[end]), hists)
    hist = hists[end]
    range = first(hist) .+ (0.0, 0.4) .* width(hist)
    range[1] < first(price_tss) < range[2] || return EMPTY
    res = []
    for i in eachindex(strikes)[1:end-spread]
        j = i + spread
        # for j in (i+1):lastindex(strikes)
            # neto = shorts[i] + longs[j]
            # # if 0.4 <= neto <= 0.6
            #     push!(res, (;short=i, long=j, neto))
            # # end
            neto = longs[i] + shorts[j]
            0.04 < neto < (spread - 0.04) || continue
            risk = Float32(spread - neto)
            # if -0.6 <= neto <= -0.4
                push!(res, (;long=i, short=j, neto, risk))
            # end
        # end
    end
    filter!(x -> x.neto >= 0.01, res)
    if isempty(res)
        return EMPTY
    end
    # top = res[findmin(x -> abs(abs(x.neto) - 0.5), res)[2]]
    top = res[findmin(x -> abs(x.neto - neto_target), res)[2]]
    netc = xpir_values[top.long] - xpir_values[top.short]
    pnl = top.neto + netc
    # return (;strike_long=[strikes[top.long]], strike_short=[strikes[top.short]], neto=[top.neto], netc=[netc], pnl=[pnl])
    res = (;strike_long=strikes[top.long], strike_short=strikes[top.short], top.neto, netc, pnl, price_ts=first(price_tss), price_xpir=first(price_xpirs), risk=top.risk)
    # println("returning: ", res)
    return res
end

function rolling_extrema(v, window)
    res = Vector{NTuple{2,Float32}}(undef, length(v))
    for i in eachindex(v)
        i >= window || ( res[i] = (NaN32,NaN32); continue )
        left = i - window + 1
        right = i
        res[i] = extrema(v[left:right])
    end
    return res
end

width(t::Tuple) = last(t) - first(t)
#endregion Half

end