module SpyBtal
using Dates, DataFrames, Makie
import HistData as hd
using DrawUtil

function setup()
    global btal = DataFrame(reverse(hd.dataDaily("BTAL")))
    # spy = DataFrame(reverse(hd.dataDaily(btal.date[1], btal.date[end], "SPY")))
    # global spy = filter(spy -> spy.date in btal.date, spy)
    @assert btal.date == spy.date
    global dates_for_axis = makedax(btal.date)
    global spy_all = DataFrame(reverse(hd.dataDaily("SPY")))
end
makedax(dates) = map(dates) do date; Dates.value(date - dates[1]) end

make_df(sym) = filter(p -> p.date in btal.date, DataFrame(reverse(hd.dataDaily(string(sym)))))
function draw_sym(sym; kws...)
    df = make_df(sym)
    draw_df(df; kws...)
end
function draw_df(df; col=:close, use_log=false, kws...)
    c = df[!,col]
    f = use_log ? log : identity
    lines!(dates_for_axis, f.(c ./ c[1]); kws...)
end

function draw()
    fig = Figure()

    ax_left = Axis(fig[1, 1], xlabel = "date", ylabel = "price", title = "SPY", titlealign=:left, yaxisposition=:left)
    ax_right = Axis(fig[1, 1], xlabel = "date", ylabel = "price", title = "BTAL", titlealign=:right, yaxisposition=:right)
    linkxaxes!(ax_left, ax_right)

    lines!(ax_left, dates_for_axis, spy.close, color = :blue)
    lines!(ax_right, dates_for_axis, btal.close, color = :green)

    # fit_price, spy_price, slope, offset, xs = fit_line(btal.date[1])
    # fit_xs = [xs[1], xs[end]]
    # lines!(ax_left, fit_xs, linear_model(fit_xs, [slope, offset]), color = :green)

    fig
end

function fit_line(date)
    global spy_ind = searchsortedfirst(spy_all.date, date)
    global spy_dates = @view spy_all.date[(spy_ind - fit_len):(spy_ind - 1)] # spy hist for up to prev day close
    global spy_hist = @view spy_all.close[(spy_ind - fit_len):(spy_ind - 1)] # spy hist for up to prev day close

    xs = map(spy_dates) do date; Dates.value(date - spy_dates[1]) end
    slope, offset = LsqFit.curve_fit(linear_model, xs, spy_hist, INIT_PARAM).param

    fit_price = linear_model(spy_ind, [slope, offset])
    spy_price = spy_all.close[spy_ind]

    return fit_price, spy_price, slope, offset, xs, spy_hist
end

import IncTA
function fit_line_ema(date)
    fit_len = 20
    global spy_ind = searchsortedfirst(spy_all.date, date)
    global spy_dates = @view spy_all.date[(spy_ind - fit_len):(spy_ind - 1)] # spy hist for up to prev day close
    global spy_hist = @view spy_all.close[(spy_ind - fit_len):(spy_ind - 1)] # spy hist for up to prev day close
    ema = IncTA.EMA{Float64}(;period=fit_len)
    IncTA.fit!(ema, spy_hist)

    fit_price = IncTA.value(ema)
    spy_price = spy_all.close[spy_ind]

    return fit_price, spy_price
end

import LsqFit
linear_model(x, p) = p[1] .* x .+ p[2]
INIT_PARAM = [1.0, 1.0]

function proc_date!(quantities, ind, sym, sym_price; target_value)
    date = btal.date[ind]
    # spy_ind = searchsortedfirst(spy_all.date, date)
    # spy_hist = @view spy_all.close[(spy_ind - fit_len):(spy_ind - 1)] # spy hist for up to prev day close
    # slope, offset = LsqFit.curve_fit(linear_model, eachindex(spy_hist), spy_hist, INIT_PARAM).param
    # fit_price = linear_model(spy_ind, [slope, offset])
    # spy_price = spy_all.close[spy_ind]

    # # fit_price, spy_price, slope, offset = fit_line(date)
    # fit_price, spy_price = fit_line_ema(date)
    # target_spy_qty = target_value / fit_price
    # spy_adjust_qty = target_spy_qty - quantities[:SPY]
    # value_adjust = spy_adjust_qty * spy_price
    # btal_adjust_qty = -value_adjust / btal.close[ind] # negative for opposite action from spy

    # fit_price, spy_price, slope, offset = fit_line(date)
    # fit_price, spy_price = fit_line_ema(date)
    # spy_adjust_qty, btal_adjust_qty = calc_adjust(target_value, spy_price, btal.close[ind], fit_price)
    sym_adjust_qty, btal_adjust_qty = calc_adjust_btal(quantities, target_value, sym_price, btal.close[ind])
    btal_price = btal.close[ind]

    quantities[sym] += sym_adjust_qty
    quantities[:BTAL] += btal_adjust_qty
    # println("ind: $ind, date: $date, fit_price: $fit_price, spy_price: $spy_price, target_spy_qty: $target_spy_qty, spy_adjust_qty: $spy_adjust_qty, value_adjust: $value_adjust, btal_adjust: $btal_adjust_qty")
    # println("after quantities: ", quantities, " value: ", quantities[:SPY] * spy_price + quantities[:BTAL] * btal.close[ind])
    # @assert quantities[:SPY] * fit_price ≈ target_value "SPY value: $(quantities[:SPY] * spy_price), target: $target_value"
    sym_val = quantities[sym] * sym_price
    btal_val = quantities[:BTAL] * btal_price
    total = sym_val + btal_val
    return (;date, total, sym_val, btal_val, sym_qty=quantities[sym], btal_qty=quantities[:BTAL], sym_adjust_qty, btal_adjust_qty, sym_price, btal_price)
end

# function calc_adjust(quantities, target_value, sym_price, btal_price, fit_price)
#     target_spy_qty = target_value / fit_price
#     spy_adjust_qty = target_spy_qty - quantities[:SPY]
#     value_adjust = spy_adjust_qty * sym_price
#     btal_adjust_qty = -value_adjust / btal_price # negative for opposite action from spy
#     return spy_adjust_qty, btal_adjust_qty
# end

function calc_adjust_btal(quantities, target_value, sym_price, btal_price)
    target_btal_qty = target_value / btal_price
    btal_adjust_qty = target_btal_qty - quantities[:BTAL]
    value_adjust = btal_adjust_qty * btal_price
    sym_adjust_qty = -value_adjust / sym_price # negative for opposite action from spy
    return sym_adjust_qty, btal_adjust_qty
end

config() = (;
    init_balance = 100000.0,
    control_to_invest_ratio = 0.2,
    # control_target_value = 10000.0,
    # max_hedge_to_control_ratio = 10.0,
    max_hedge_to_invest_ratio = 0.2,
    # hedge_to_invest_ratio = 0.2,
)

function run()
    global kcheated = 0.0
    global kcheats = []
    fig = Figure()
    ax_left = Axis(fig[1, 1], xlabel = "days", ylabel = "price", title = "", titlealign=:center, yaxisposition=:left)
    display(fig)

    sym_control = :BTAL
    sym_invest = :SPY
    sym_hedge = :SPXU
    sym_benchmark = :SPY

    draw_sym(sym_control; color=:red)
    draw_sym(sym_benchmark; color=:white, use_log=true)
    draw_sym(sym_invest; color=:green, use_log=true)

    # sym_types = (;control=:control, invest=:invest, hedge=:hedge)
    syms = (;control=sym_control, invest=sym_invest, hedge=sym_hedge, benchmark=sym_benchmark)
    dfs = map(syms) do sym; make_df(sym) end
    global kdfs = dfs

    global quantities = Dict{Symbol, Float64}()
    init_balance = config().init_balance
    # x + y = 1
    # x + ratio * x = 1
    # x = 1 / (1 + ratio)
    invest = (1 / (1 + config().control_to_invest_ratio)) * init_balance
    control = init_balance - invest
    quantities[:invest] = invest / dfs.invest.close[1]
    quantities[:control] = control / dfs.control.close[1]
    quantities[:hedge] = 0.0
    global cash = config().init_balance / 2

    # invest = init_balance - config().control_target_value
    # control = config().control_target_value
    # quantities[:invest] = invest / dfs.invest.close[1]
    # quantities[:control] = control / dfs.control.close[1]
    # quantities[:hedge] = 0.0

    records = []
    for ind in eachindex(btal.date)
        prices = map(dfs) do df; df.close[ind] end

        bals = make_balances(quantities, prices)
        begin_bal = bals.total

        moved_amounts = maintain_ratios!(quantities, prices)

        bals = make_balances(quantities, prices)
        end_bal = bals.total

        rec = merge(moved_amounts, bals)
        global krec = rec
        @assert end_bal ≈ begin_bal "begin: $begin_bal, end: $end_bal"

        if any(p -> round(last(p); digits=10) < 0, bals)
            error("Negative balance found at index $ind: $rec")
        end
        push!(records, rec)
    end
    df = DataFrame(records)
    df.check = df.total .+ df.hedge_bal
    draw_df(df; col=:total, color=:blue, use_log=true)
    return df
end

function maintain_ratios!(quantities, prices)
    # If we need to increase control, we first pull from hedge, then from invest
    # If we have surplus control, we first hedge up to max ratio, then invest
    # If hedge is over max ratio to invest, move to invest
    into_control = update_to_ratio!(quantities, prices, config().control_to_invest_ratio, :control => :invest, :hedge, :invest)
    # invest_val = quantities[:invest] * prices.invest
    k = 1.4
    if into_control < 0
        target = -k * into_control
        amt = min(target, cash / 2)
        if amt < target
            println("Ran out of cash: $cash")
        end
        global cash -= amt
        inv_amt = 0.2 * target
        quantities[:invest] -= inv_amt / prices.invest
        quantities[:hedge] += (amt + inv_amt) / prices.hedge
        # quantities[:hedge] += amt / prices.hedge
        # push!(kcheats, target / invest_val)
    else
        target = k * into_control
        amt = min(target, quantities[:hedge] * prices.hedge)
        quantities[:hedge] -= amt / prices.hedge
        global cash += amt
        # quantities[:invest] -= target / prices.invest
        # global cash += amt + target
        # push!(kcheats, -amt / invest_val)
    end
    into_hedge = update_to_max_ratio!(quantities, prices, config().max_hedge_to_invest_ratio, :hedge, :invest)
    return (;into_control, into_hedge)
end

#region archive
# function maintain_ratios_old!(quantities, prices, values)
#     hedge_to_control = 0.0
#     invest_to_control = 0.0
#     control_to_hedge = 0.0
#     hedge_to_invest = 0.0

#     # If we need to increase control, we first pull from hedge, then from invest
#     # If we have surplus control, we first hedge up to max ratio, then invest
#     # If hedge is over max ratio to invest, move to invest
#     control_adjust_value = config().control_target_value - values.control
#     if control_adjust_value > 0.0
#         if values.hedge > 0.0
#             value_from_hedge = min(control_adjust_value, values.hedge)
#             quantities[:hedge] -= value_from_hedge / prices.hedge
#             quantities[:control] += value_from_hedge / prices.control
#             hedge_to_control = value_from_hedge
#             control_adjust_value -= value_from_hedge
#         end
#         if control_adjust_value > 0.0
#             quantities[:invest] -= control_adjust_value / prices.invest
#             quantities[:control] += control_adjust_value / prices.control
#             invest_to_control = control_adjust_value
#         end
#     elseif control_adjust_value < 0.0
#         # Move all the extra to hedge, then move it from hedge to invest if hedge over ratio
#         quantities[:control] -= -control_adjust_value / prices.control
#         quantities[:hedge] += -control_adjust_value / prices.hedge
#         control_to_hedge = control_adjust_value
#     end
#     # Check hedge and move to invest if over max ratio
#     # Can't use values named tuple because it may have changed above
#     hedge_value = quantities[:hedge] * prices.hedge
#     # control_value = quantities[:control] * prices.control
#     invest_value = quantities[:invest] * prices.invest
#     hedge_ratio = hedge_value / invest_value

#     # extra_ratio = hedge_ratio - config().max_hedge_to_invest_ratio
#     # if extra_ratio > 0.0
#     #     extra_value = extra_ratio * invest_value
#     #     quantities[:hedge] -= extra_value / prices.hedge
#     #     quantities[:invest] += extra_value / prices.invest
#     #     hedge_to_invest = extra_value
#     #     println("Had extra hedge: $extra_value")
#     # end

#     # target ratio instead of max ratio
#     hedge_adjust_ratio = config().hedge_to_invest_ratio - hedge_ratio
#     if hedge_adjust_ratio > 0.0
#         need_value = hedge_adjust_ratio * invest_value
#         quantities[:invest] -= need_value / prices.invest
#         quantities[:hedge] += need_value / prices.hedge
#         hedge_to_invest = -need_value
#         println("Moved to hedge: $need_value")
#     elseif hedge_adjust_ratio < 0.0
#         extra_value = -hedge_adjust_ratio * invest_value
#         quantities[:hedge] -= extra_value / prices.hedge
#         quantities[:invest] += extra_value / prices.invest
#         hedge_to_invest = extra_value
#         println("Moved from hedge: $extra_value")
#     end

#     return (;hedge_to_control, invest_to_control, control_to_hedge, hedge_to_invest)
# end

# function maintain_ratios_control_to_invest_ratio!(quantities, prices, values)
#     hedge_to_control = 0.0
#     invest_to_control = 0.0
#     control_to_hedge = 0.0
#     hedge_to_invest = 0.0

#     # If we need to increase btal, we first pull from hedge, then from invest
#     # If we have surplus btal, we first hedge up to max ratio, then invest
#     cur_ratio = values.control / values.invest
#     ratio_adjust = config().control_to_invest_ratio - cur_ratio
#     if ratio_adjust > 0.0
#         needed_value = ratio_adjust * values.invest
#         if values.hedge > 0.0
#             value_from_hedge = min(needed_value, values.hedge)
#             quantities[:hedge] -= value_from_hedge / prices.hedge
#             quantities[:control] += value_from_hedge / prices.control
#             hedge_to_control = value_from_hedge
#             ratio_adjust -= value_from_hedge / values.invest
#         end
#         if ratio_adjust > 0.0
#             needed_value = ratio_adjust * values.invest
#             quantities[:invest] -= needed_value / prices.invest
#             quantities[:control] += needed_value / prices.control
#             invest_to_control = needed_value
#         end
#     elseif ratio_adjust < 0.0
#         # Move all the extra to hedge, then move it from hedge to invest if hedge over ratio
#         extra_value = -ratio_adjust * values.invest
#         quantities[:control] -= extra_value / prices.control
#         quantities[:hedge] += extra_value / prices.hedge
#         control_to_hedge = extra_value
#     end
#     # Check hedge and move to invest if over max ratio
#     # Can't use values named tuple because it may have changed above
#     hedge_value = quantities[:hedge] * prices.hedge
#     control_value = quantities[:control] * prices.control
#     hedge_ratio = hedge_value / control_value
#     extra_ratio = hedge_ratio - config().max_hedge_to_control_ratio
#     if extra_ratio > 0.0
#         extra_value = extra_ratio * control_value
#         quantities[:hedge] -= extra_value / prices.hedge
#         quantities[:invest] += extra_value / prices.invest
#         hedge_to_invest = extra_value
#         println("Had extra hedge: $extra_value")
#     end
#     return (;hedge_to_control, invest_to_control, control_to_hedge, hedge_to_invest)
# end

# function run2()
#     # try maintaining a lagged ratio between hedge and invest
#     fig = Figure()
#     ax_left = Axis(fig[1, 1], xlabel = "days", ylabel = "price", title = "", titlealign=:center, yaxisposition=:left)
#     display(fig)

#     sym_control = :BTAL
#     sym_invest = :SPY
#     sym_hedge = :SPXU
#     sym_benchmark = :SPY

#     draw_sym(sym_control; color=:red)
#     draw_sym(sym_benchmark; color=:white, use_log=true)
#     draw_sym(sym_invest; color=:green, use_log=true)

#     sym_types = (;control=:control, invest=:invest, hedge=:hedge)
#     syms = (;control=sym_control, invest=sym_invest, hedge=sym_hedge, benchmark=sym_benchmark)
#     # dfs = Dict(sym => make_df(values(sym)) for sym in values(syms))
#     dfs = map(syms) do sym; make_df(sym) end

#     global quantities = Dict{Symbol, Float64}()

#     quantities[:invest] = config().init_balance / dfs.invest.close[1]
#     quantities[:hedge] = 0.0
#     prices = map(dfs) do df; df.close[1] end
#     update_to_ratio!(quantities, prices, config().control_target_value, :hedge, :invest)

#     records = []
#     for ind in eachindex(btal.date)
#         prices = map(dfs) do df; df.close[ind] end
#         rec_ratios = update_to_ratio!(quantities, prices, config().hedge_to_invest_ratio, :hedge, :invest; min_ratio=0.1)
#         bals = make_balances(quantities, prices)
#         rec = merge(rec_ratios, bals)
#         if any(p -> round(last(p); digits=10) < 0, bals)
#             error("Negative balance found at index $ind: $rec")
#         end
#         push!(records, rec)
#     end
#     df = DataFrame(records)
#     draw_df(df; col=:total, color=:blue, use_log=true)
#     return df
# end
#endregion archive

#region Util
function make_balances(quantities, prices)
    sbs = [Symbol(string(key), "_bal") => quantities[key] * prices[key] for key in keys(quantities)]
    total = sum(last.(sbs)) + cash
    return merge((;total, cash), sbs)
end

function transfer!(quantities, prices, target_amount, to_sym, from_syms...)
    if target_amount < 0.0
        @assert length(from_syms) == 1
        from_sym = from_syms[1]
        to_bal = quantities[to_sym] * prices[to_sym]
        @assert to_bal >= target_amount
        quantities[to_sym] += target_amount / prices[to_sym]
        quantities[from_sym] -= target_amount / prices[from_sym]
        println("Moved $target_amount from $from_sym to $to_sym")
        return target_amount
    end

    remaining_amount = target_amount
    for from_sym in from_syms
        from_bal = quantities[from_sym] * prices[from_sym]
        amount = min(target_amount, from_bal)
        quantities[to_sym] += amount / prices[to_sym]
        quantities[from_sym] -= amount / prices[from_sym]
        println("Moved $amount from $from_sym to $to_sym")
        remaining_amount = round(remaining_amount - amount; digits=10)
        remaining_amount != 0.0 || break
    end
    return target_amount - remaining_amount
end

function update_to_ratio!(quantities, prices, target_ratio, (sym_to, sym_denom), from_syms...)
    println("update to ratio: $target_ratio, $sym_to, $sym_denom, $from_syms")
    adj = 0.0
    for from_sym in from_syms
        value_to = quantities[sym_to] * prices[sym_to]
        value_denom = quantities[sym_denom] * prices[sym_denom]
        if from_sym == sym_denom
            to_move = need_to_move(value_to, value_denom, target_ratio)
            adj += transfer!(quantities, prices, to_move, sym_to, from_sym)
            break
        else
            to_move = (target_ratio - (value_to / value_denom)) * value_denom
            println("Adjusting $to_move from $from_sym to $sym_to")
            moved = transfer!(quantities, prices, to_move, sym_to, from_sym)
            adj += moved
            if moved ≈ to_move
                break
            end
        end
    end
    value_to = quantities[sym_to] * prices[sym_to]
    value_denom = quantities[sym_denom] * prices[sym_denom]
    @assert (value_to / value_denom) ≈ target_ratio
    return adj
end

function update_to_max_ratio!(quantities, prices, max_ratio, sym_numer, sym_denom)
    println("update to max ratio: $max_ratio, $sym_numer, $sym_denom")
    adj = 0.0
    value_numer = quantities[sym_numer] * prices[sym_numer]
    value_denom = quantities[sym_denom] * prices[sym_denom]
    amount = need_to_move(value_numer, value_denom, max_ratio)
    if amount < 0 # cur_ratio > max_ratio
        # @assert amount < 0
        # amount is neg so it will move from numer to denom to shrink ratio
        adj += transfer!(quantities, prices, amount, sym_numer, sym_denom)
    end
    value_numer = quantities[sym_numer] * prices[sym_numer]
    value_denom = quantities[sym_denom] * prices[sym_denom]
    new_ratio = value_numer / value_denom
    @assert (new_ratio - max_ratio) < 1e-10 "new ratio $new_ratio not below max ratio $max_ratio"
    return adj
end

function update_to_value!(quantities, prices, target_value, sym, from_syms...; min_adj=0.0)
    adj = target_value - quantities[sym] * prices[sym]
    if abs(adj) > min_adj
        amount_transferred = transfer!(quantities, prices, adj, sym, from_syms)
        if amount_transferred != adj
            println("Insufficient balance to transfer from $from_sym to $sym: $adj, transferred: $amount_transferred")
        end
        return adj
    else
        return 0.0
    end
end

# (bal1 + adj) / (bal2 - adj) = target_ratio
# (bal1 + adj) = target_ratio * (bal2 - adj)
# bal1 + adj = target_ratio * bal2 - target_ratio * adj
# bal1 + adj + target_ratio * adj = target_ratio * bal2
# adj + target_ratio * adj = target_ratio * bal2 - bal1
# (1 + target_ratio) * adj = target_ratio * bal2 - bal1
# adj = (target_ratio * bal2 - bal1) / (1 + target_ratio)
function need_to_move(bal1, bal2, target_ratio)
    adj = (target_ratio * bal2 - bal1) / (1 + target_ratio)
    res = (bal1 + adj) / (bal2 - adj)
    @assert res ≈ target_ratio
    # println("Result: $(bal1 + adj) / $(bal2 - adj) ≈ $(res)")
    return adj
end
#endregion Util

end