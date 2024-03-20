module ProtPuts
using Dates, DataFrames, Makie
using SearchSortedNearest
using DateUtil, SmallTypes, OptionTypes, DataRead
import HistData as hd
using DrawUtil
import Calendars as cal

#region Main
function run()
    fig = Figure()
    ax_left = Axis(fig[1, 1], xlabel = "days", ylabel = "price", title = "", titlealign=:center, yaxisposition=:left)
    display(fig)

    sym_control = :BTAL
    sym_invest = get_invest_sym()
    sym_hedge = :SPXU
    sym_benchmark = :SPY

    draw_sym(sym_control; color=:red)
    draw_sym(sym_benchmark; color=:white, use_log=invest_use_log())
    draw_sym(sym_invest; color=:green, use_log=invest_use_log())

    syms = (;control=sym_control, invest=sym_invest, hedge=sym_hedge, benchmark=sym_benchmark)
    dfs = map(syms) do sym; make_df(sym) end
    global kdfs = dfs
    global options_neto = 0.0
    global options_netc = 0.0
    global options = []
    global quantities = Dict{Symbol, Float64}()

    init_invest = 100 * config().num_contracts
    init_bal = init_invest * dfs[:invest].close[1]
    quantities[:invest] = init_invest
    cinit = (config().control_to_invest_ratio * init_bal)
    quantities[:control] = cinit / dfs[:control].close[1]
    quantities[:hedge] = 0.0
    quantities[:cash] = config().control_to_cash_ratio * cinit

    global records = []
    for ind in eachindex(btal.date)
        date = btal.date[ind]
        get_price = sym -> sym == :cash ? 1.0 : dfs[sym].close[ind]

        bal_begin = make_balances(date, quantities, get_price)

        # println("proc $date")
        if isempty(options) || date >= options[1].put.expiration
            @assert length(options) <= 1
            if !isempty(options)
                opts = options[1]
                empty!(options)
                netc = calc_netc(date, opts)
                println("Closing opts for $(netc) on $(date)")
                before = quantities[:control]
                quantities[:control] += netc / get_price(:control)
                if quantities[:control] < 0.0
                    transfer!(quantities, get_price, -quantities[:control] * get_price(:control), :control, :cash)
                end
                quantities[:control] >= 0.0 || error("Negative control after close $(before) -> $(quantities[:control]) on $(date): netc=$(netc)")
                # netc >= 0.0 || error("Negative put sell price $(netc) on $(date)")
                if !iszero(netc)
                    println("Non-zero opts sale for $(netc) on $(date)")
                    global options_netc += netc
                end
            end

            opts = find_next_options(date, dfs.benchmark.open[ind])
            if !isnothing(opts)
                println("Open opts for $(opts.call_neto) - $(opts.put_neto) on $(date)")
                push!(options, opts)
                neto = calc_neto(opts)
                @assert abs(neto + calc_netc(date, opts)) < 0.001 "Mismatched neto $(neto) and sell price $(calc_netc(date, opts)) on $(date)\n  from: $(opts)"
                before = quantities[:control]
                quantities[:control] += neto / get_price(:control)
                if quantities[:control] < 0.0
                    transfer!(quantities, get_price, -quantities[:control] * get_price(:control), :control, :cash)
                end
                quantities[:control] >= 0.0 || error("Negative control after open $(before) -> $(quantities[:control]) on $(date): neto=$(neto)")
                global options_neto += neto
            else
                println("WARN: skipped repurchase of options")
            end
        end

        update_to_ratio!(quantities, get_price, config().control_to_cash_ratio, (:control, :cash), :cash)
        update_to_ratio!(quantities, get_price, config().control_to_invest_ratio, (:control, :invest), :invest)

        bal_end = make_balances(date, quantities, get_price)
        @assert bal_end.total ≈ bal_begin.total "Mismatched balances $(date):\n  $(bal_begin)\n  $(bal_end)"
        rec = merge((;date, price_invest=get_price(:invest)), bal_end)
        push!(records, rec)
        rec.cash_bal > 0.0 || error("Negative cash $(rec.cash_bal) on $(date)")
    end
    df = DataFrame(records)
    if all(x -> isfinite(x) && x >= 0.0, df.total)
        draw_df(df; col=:total, color=:blue, use_log=invest_use_log())
        draw_df(df; col=:invest_bal, color=:purple, use_log=invest_use_log())
    else
        println("Non-finite or negative values in df.total")
    end
    println("Incease mult: ", df.total[end] / df.total[1])
    return df
end
#endregion Main

#region Options
const OPTIONS = Dict{Tuple{Int,Int}, DataFrame}()
# function get_subdf_ts_exp(date, expir; eod=false)
#     ym = get_ym(date)
#     # months = round(Int, config().target_expir_days / 30, RoundUp) + 1
#     # get_ym.(date:Month(1):(date + Month(months)))
#     df = get!(OPTIONS, ym) do
#         DataRead.get_options(ym...; age=DateUtil.FOREVER2)
#     end
# end

function find_next_options(date, under)
    ym = get_ym(date)
    df = get!(OPTIONS, ym) do
        DataRead.get_options(ym...; age=DateUtil.FOREVER2)
    end

    expir_search = date + Day(config().target_expir_days)
    df_call, df_put = get_subdf_stx(df, date, expir_search; eod=true)
    !isempty(df_call) || error("Could not find call for $(date), $(expir_search), $(under)")
    !isempty(df_put) || error("Could not find put for $(date), $(expir_search), $(under)")

    call_ind = searchsortedfirst(df_call.strike, config().target_call_strike_rat * under)
    0 < call_ind <= lastindex(df_call.strike) || ( println("WARN: Could not find call strike for $(date), $(under)") ; return nothing )
    call_row = @view df_call[call_ind,:]

    put_ind = searchsortedlast(df_put.strike, config().target_put_strike_rat * under)
    0 < put_ind <= lastindex(df_put.strike) || ( println("WARN: Could not find put strike for $(date), $(under)") ; return nothing )
    put_row = @view df_put[put_ind,:]

    !isempty(call_row) || error("Could not get put for $(date)")
    !isempty(put_row) || error("Could not get put for $(date)")
    # global kcr_buy = (;call_row, put_row)

    mult = 100 * option_ratio()
    return (;
        put = Option(Style.put, Date(put_row.expir), put_row.strike),
        put_neto = mult * opt_price(put_row),
        call = Option(Style.call, Date(call_row.expir), call_row.strike),
        call_neto = mult * opt_price(call_row),
    )
end

function calc_netc(date, opts)
    ym = get_ym(date)
    df = get!(OPTIONS, ym) do
        DataRead.get_options(ym...; age=DateUtil.FOREVER2)
    end

    df_call, df_put = get_subdf_stx(df, date, opts.put.expiration; eod=true)
    call_date = Date(df_call.ts[1])
    call_date <= date || error("Attempt to price call after expiration $(call) on $(date)")
    put_date = Date(df_put.ts[1])
    put_date <= date || error("Attempt to price put after expiration $(put) on $(date)")
    if call_date != date
        println("WARN: Pricing at $(date) before expiration $(opts.call.expiration) for call $(opts.call)")
    end
    if put_date != date
        println("WARN: Pricing at $(date) before expiration $(opts.put.expiration) for put $(opts.put)")
    end
    call_row = @view df_call[searchsorted(df_call.strike, opts.call.strike),:]
    put_row = @view df_put[searchsorted(df_put.strike, opts.put.strike),:]
    !isempty(call_row) || ( println("WARN: Could not find strike for call $(opts.call) on $(date)") ; return 0.0 )
    !isempty(put_row) || ( println("WARN: Could not find strike for put $(opts.put) on $(date)") ; return 0.0 )

    # global kcr_sell = (;call_row, put_row)
    mult = 100 * option_ratio()
    # (call=mult * opt_price(call_row), put=mult * opt_price(put_row))
    return mult * opt_price(put_row) - ( config().sell_call ? mult * opt_price(call_row) : 0.0 )
end

opt_price(opt_row) = (opt_row.bid[1] + opt_row.ask[1]) / 2
#endregion Options

#region Util
get_ym(date) = (year(date), month(date))

function get_subdf(df, col, value)
    left = searchsortedfirst(col, value)
    right = searchsortedlast(col, value)
    return @view df[left:right,:]
end

function get_subdf_stx(df, date::Date, exp::Date; eod=false)
    call_ind = searchsortedfirst(df.style, 1)
    df_put = @view df[1:(call_ind - 1),:]
    df_call = @view df[call_ind:end,:]
    return (
        get_subdf_ts_exp(df_call, date, exp; eod=eod),
        get_subdf_ts_exp(df_put, date, exp; eod=eod),
    )
end

function get_subdf_ts_exp(df, date::Date, exp::Date; eod=false)
    ts_find = eod ? cal.getMarketClose(date) : cal.getMarketOpen(date) + Hour(2)
    ts = df.ts[searchsortedlast(df.ts, ts_find)]
    df = get_subdf(df, df.ts, ts)

    exp_find = DateTime(exp) + Hour(10)
    expir = df.expir[searchsortednearest(df.expir, exp_find)]
    df = get_subdf(df, df.expir, expir)

    return df
end

function make_balances(date, quantities, get_price)
    sbs = [Symbol(string(key), "_bal") => quantities[key] * get_price(key) for key in keys(quantities)]
    opts_value = sum(map(opts -> calc_netc(date, opts), options); init=0.0)
    total = sum(last.(sbs)) + opts_value
    return merge((;total, opts=opts_value), sbs)
end

function transfer!(quantities, get_price, target_amount, to_sym, from_syms...)
    if target_amount < 0.0
        @assert length(from_syms) == 1
        from_sym = from_syms[1]
        to_bal = quantities[to_sym] * get_price(to_sym)
        @assert to_bal >= target_amount
        quantities[to_sym] += target_amount / get_price(to_sym)
        quantities[from_sym] -= target_amount / get_price(from_sym)
        # println("Moved $target_amount from $from_sym to $to_sym")
        return target_amount
    end

    remaining_amount = target_amount
    for from_sym in from_syms
        from_bal = quantities[from_sym] * get_price(from_sym)
        amount = min(target_amount, from_bal)
        quantities[to_sym] += amount / get_price(to_sym)
        quantities[from_sym] -= amount / get_price(from_sym)
        # println("Moved $amount from $from_sym to $to_sym")
        remaining_amount = round(remaining_amount - amount; digits=10)
        remaining_amount != 0.0 || break
    end
    return target_amount - remaining_amount
end

function update_to_ratio!(quantities, get_price, target_ratio, (sym_to, sym_denom), from_syms...)
    # global kargs = (;quantities, get_price, target_ratio, ratio=(sym_to, sym_denom), from_syms)
    # println("update to ratio: $target_ratio, $sym_to, $sym_denom, $from_syms")
    adj = 0.0
    for from_sym in from_syms
        value_to = quantities[sym_to] * get_price(sym_to)
        value_denom = quantities[sym_denom] * get_price(sym_denom)
        if from_sym == sym_denom
            to_move = need_to_move(value_to, value_denom, target_ratio)
            !iszero(to_move) || continue
            adj += transfer!(quantities, get_price, to_move, sym_to, from_sym)
            break
        else
            to_move = (target_ratio - (value_to / value_denom)) * value_denom
            !iszero(to_move) || continue
            println("Adjusting $to_move from $from_sym to $sym_to")
            moved = transfer!(quantities, get_price, to_move, sym_to, from_sym)
            adj += moved
            if moved ≈ to_move
                break
            end
        end
    end
    value_to = quantities[sym_to] * get_price(sym_to)
    value_denom = quantities[sym_denom] * get_price(sym_denom)
    @assert (value_to / value_denom) ≈ target_ratio "new ratio $(value_to / value_denom) not equal to target ratio $target_ratio"
    return adj
end

function update_to_max_ratio!(quantities, get_price, max_ratio, sym_numer, sym_denom)
    println("update to max ratio: $max_ratio, $sym_numer, $sym_denom")
    adj = 0.0
    value_numer = quantities[sym_numer] * get_price(sym_numer)
    value_denom = quantities[sym_denom] * get_price(sym_denom)
    amount = need_to_move(value_numer, value_denom, max_ratio)
    if amount < 0 # cur_ratio > max_ratio
        # @assert amount < 0
        # amount is neg so it will move from numer to denom to shrink ratio
        adj += transfer!(quantities, get_price, amount, sym_numer, sym_denom)
    end
    value_numer = quantities[sym_numer] * get_price(sym_numer)
    value_denom = quantities[sym_denom] * get_price(sym_denom)
    new_ratio = value_numer / value_denom
    @assert (new_ratio - max_ratio) < 1e-10 "new ratio $new_ratio not below max ratio $max_ratio"
    return adj
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

calc_neto(opts) = config().sell_call ? opts.call_neto - opts.put_neto : -opts.put_neto
#endregion Util

#region Extra
function setup()
    global btal = DataFrame(reverse(hd.dataDaily(Date(2012,6,1), Date(2023,11,30), "BTAL")))
    # spy = DataFrame(reverse(hd.dataDaily(btal.date[1], btal.date[end], "SPY")))
    # global spy = filter(spy -> spy.date in btal.date, spy)
    # @assert btal.date == spy.date
    global dates_for_axis = makedax(btal.date)
    # global spy_all = DataFrame(reverse(hd.dataDaily("SPY")))
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
#endregion Extra

#region Params
# is_target_strike(under, strike) = .97 * under <= strike <= 0.98 * under
# is_target_expir(date, expir) = date + Day(20) <= Date(expir) <= date + Day(60)
# is_target_price(under, ask) = ask <= config().target_price_ratio * under / option_ratio()
# use_target_price() = false
invest_use_log() = true
get_invest_sym() = :SPY

# option_ratio() = config().num_contracts * 3.0 # 3 for the tripling of UPRO
option_ratio() = config().num_contracts * 1

config() = (;
    init_balance = 100000.0,
    num_contracts = 1,
    control_to_invest_ratio = 0.2,
    control_to_cash_ratio = 1.0,
    target_expir_days = 45,
    target_put_strike_rat = 0.95,
    target_call_strike_rat = 1.2,
    sell_call = true,
)
#endregion Params

end