module Portfolio
using Dates, DataFrames
import StatsBase
using DateUtil, Paths, FilesArrow

# TODO: buy at open or close? market on close might be risky

#region Types and Constants
const DATES = DateUtil.all_bdays(Date(2022,2,15), Date(2024,2,15))
dates_list() = DATES

struct Transaction
    date::Date
    sym::Symbol
    qty::Float64
    price::Float64
end

mutable struct Account
    cash::Float64
    quantities::Dict{Symbol, Float64}
    transactions::Vector{Transaction}
end
Account() = Account(1000000, Dict{Symbol, Float64}(), Transaction[])

struct DataLookup
    price::DataFrame
    constituency::DataFrame
end

const DF_LOOKUP = Dict{Symbol, DataLookup}()
const SECTORS2 = Dict{Symbol, DataFrameRow}()
const SECTORS3 = Ref{DataFrame}()
const DISALLOW = Vector{Symbol}()

sparrow_path() = Paths.db_incoming("norgate", "SP500.arrow")
always_symbols() = [:SPY, :UPRO, :TMF]
always_count() = length(always_symbols())
#endregion Types and Constants

function run(;corr_ndays=120, ret_ndays=20, count_per_sector=4, min_price=10.2, refresh=false)
    empty!(DISALLOW)
    global kbal = []
    acct = Account()
    global kacct = acct
    if isempty(DF_LOOKUP) || refresh
        empty!.((DF_LOOKUP, SECTORS2))
        make_sectors()
        make_lookup()
    end

    for ind_curday in 121:lastindex(DATES)
        date = dates_list()[ind_curday]
        df_corr = make_df_corr(DF_LOOKUP, date, ind_curday, corr_ndays, min_price)
        global kdf_corr = df_corr

        get_cur_price = function(sym)
            lup = DF_LOOKUP[sym]
            price = lup.price[ind_curday, :open]
            if isnan(price)
                println("NaN price for $(sym) at $(date)")
                # error("stop")
            end
            # lup.constituency.include[ind_curday] || return 0.0, 0.0

            # prices = @view lup.price[(ind_curday - corr_ndays):(ind_curday-1), :open]
            prices = @view lup.price[(ind_curday - corr_ndays):ind_curday, :open]
            i = findlast(!isnan, prices)
            value = prices[i]

            @assert isnan(price) || price == value "price: $(price) | value: $(value) for $(sym) at $(date)"
            # if price != value
            #     println("** price: $(price) | value: $(value) for $(sym) at $(date)")
            # end
            return price, value
            # df = DF_LOOKUP[sym].price
            # return df[ind_curday, :open]
        end
        global kgetprice = get_cur_price
        # get_cur_price = function(sym)
        #     prices = @view DF_LOOKUP[sym].price[(ind_curday - corr_ndays):(ind_curday-1), :open]
        #     i = findlast(!isnan, prices)
        #     return prices[i]
        # end

        # targets = make_targets(df_corr, keys(acct.quantities), get_cur_price, num_syms, ret_ndays)
        targets = sector_targets(df_corr, keys(acct.quantities), get_cur_price, ret_ndays, count_per_sector)
        global ktargets = targets

        update!(date, get_cur_price, acct, targets)

        total_value = calc_account_value(get_cur_price, acct)
        push!(kbal, (;date, bal=total_value))
        println((;ind_curday, date, total_value, acct.cash))
    end
    return acct
end

function make_df_corr(lookup, date, ind, len, min_price)
    corr_inds = (ind - len):(ind - 1)
    to_include = filter(lookup) do (sym, lup)
        !(sym in DISALLOW) || return false
        occursin('-', string(sym)) && return false

        if lup.price[ind, :close_unadjusted] < min_price
            push!(DISALLOW, sym)
            println("Disallowed: $(sym) at $(date)")
            return false
        end

        # @assert issorted(lup.constituency.date)
        i = searchsortedfirst(lup.constituency.date, date)
        return i <= size(lup.constituency, 1) ? lup.constituency[i, :include] : false
    end
    dfs = DataFrame(map(p for p in to_include) do (sym, lup)
        sym => lup.price[corr_inds, :close]
    end)
    @assert size(dfs, 2) - 500 < 10
    return dfs
end

function update!(date, get_cur_price, acct, targets)
    to_remove = setdiff(keys(acct.quantities), keys(targets))
    if length(to_remove) > 8
        global kup = (;date, get_cur_price, acct, targets)
        error("too many to remove: $(length(to_remove))")
    end
    sell_all!(date, get_cur_price, acct, to_remove)
    @assert isempty(intersect(DISALLOW, keys(acct.quantities)))
    buysell_to_target!(date, get_cur_price, acct, targets)
end

function buysell_to_target!(date, get_cur_price, acct, targets)
    adding = setdiff(keys(targets), keys(acct.quantities))
    isempty(adding) || println("Adding $(adding)")
    cash_start = acct.cash
    total_value = calc_account_value(get_cur_price, acct)
    for sell in [true, false]
        # println("Cash for sell=$(sell): $(acct.cash)")
        for (sym, target_ratio) in targets
            cur_price, cur_value = get_cur_price(sym)
            if isnan(cur_price)
                println("cur_price is nan: $(cur_price) for $(sym) at $(date)")
                continue
            end
            # !isnan(cur_price) || continue
            cur_qty = get(acct.quantities, sym, 0.0)
            cur_value = cur_price * cur_qty
            cur_ratio = cur_value / total_value
            diff_ratio = target_ratio - cur_ratio
            diff_ratio != 0.0 || continue
            (diff_ratio < 0.0) == sell || continue

            # diff_value = diff_ratio * total_value
            # (diff_value < 0.0 || diff_value > 2000.0) || continue
            # @show sell diff_ratio < 0.0

            diff_qty = round(Int, (diff_ratio * total_value) / cur_price, RoundDown)
            !iszero(diff_qty) || continue # ( println("diff_qty < 1.0 so skipping purchase for $(sym)"); continue )

            if acct.cash < diff_qty * cur_price
                x = acct.cash / cur_price
                if x >= 1.0
                    new_diff_qty = round(Int, x, RoundDown)
                    println("cash limited, purchasing reduced $(diff_qty) to $(new_diff_qty) of $(sym)")
                    diff_qty = new_diff_qty
                else
                    println("Insufficient cash $(acct.cash) for $(sym) qty:$(diff_qty) at:$(cur_price) could buy: $(x)")
                    continue
                end
            end

            tx = Transaction(date, sym, diff_qty, cur_price)
            # println(tx)
            push!(acct.transactions, tx)
            acct.quantities[sym] = get!(acct.quantities, sym, 0.0) + diff_qty
            acct.cash -= diff_qty * cur_price
        end
    end
    @assert (length(targets) - length(acct.quantities)) <= 12 "length(targets)=$(length(targets)) | length(acct.quantities)=$(length(acct.quantities))"
    cash_end = acct.cash
    if cash_end < 0.0
        error("Cash went negative: $(cash_start) -> $(cash_end)")
    end
end

calc_account_value(get_cur_price, acct) = acct.cash + sum([qty * get_cur_price(sym)[2] for (sym, qty) in acct.quantities]; init=0.0)

function sell_all!(date, get_cur_price, acct, syms)
    if !isempty(syms)
        @assert not_in(always_symbols(), syms)
        println("Removing: $(syms)")
    end
    for sym in syms
        cur_price = get_cur_price(sym)[1]
        if !isfinite(cur_price)
            println("cur_price is not finite: $(cur_price) for $(sym) at $(date)")
            continue
        end
        value = acct.quantities[sym] * cur_price
        tx = Transaction(date, sym, -acct.quantities[sym], cur_price)
        push!(acct.transactions, tx)
        delete!(acct.quantities, sym)
        # acct.quantities[sym] = 0.0
        acct.cash += value
    end
end

function sector_targets(df_corr, cur_positions, get_cur_price, ret_ndays, count_per_sector)
    targets = [(;sym, score=1.0) for sym in always_symbols()]
    gdfs = groupby(SECTORS3[], :Class3)
    for gdf in gdfs
        syms = vcat(always_symbols(), intersect(gdf.sym, propertynames(df_corr)))
        to_add = score(df_corr[!,syms], get_cur_price, ret_ndays, count_per_sector, cur_positions)
        append!(targets, to_add)
    end
    # target_length = length(gdfs) * count_per_sector + 1 # + 1 for SPY
    # num_to_add = target_length - length(cur_positions)
    # if num_to_add > 0
    #     to_add = first(filter(s -> !(s in cur_positions), new_best_per_sector), num_to_add)
    #     append!(cur_positions, [s.sym for s in to_add])
    # end
    target_ratio = 1 / length(targets)
    return Dict([s.sym => target_ratio for s in targets])
end

import LsqFit
linear_model(x, p) = p[1] .* x .+ p[2]
const INIT_PARAM = [1.0, 1.0]
function score(df_corr, get_cur_price, ret_ndays, count_per_sector, bonuses)
    @assert propertynames(df_corr)[1:always_count()] == always_symbols()

    # Ignore non-finite values and don't score, but we need to keep them in syms/targets in case their already owned
    syms_all = propertynames(df_corr)
    corr_cols = all.(isfinite, eachcol(df_corr))
    # score_cols = copy(corr_cols)
    # score_cols[1] = false

    m = Matrix(df_corr[!,corr_cols])
    global km = m

    # skip = unique!(getindex.(findall(!isfinite, m), 2))
    # for i in skip
    #     println("Non-finite value in $(syms[i])")
    #     fill!((@view m[:,i]), 0.0)
    # end

    slopes_corr = detrend!(m)

    corr = StatsBase.cor(m)
    global kcorr = corr

    # scor_corr = vec(mapslices(v -> StatsBase.median(abs.(v)), corr; dims=1))[length(always_symbols()):end] # skip :SPY
    scor_ltd = vec(mapslices(v -> maximum(abs.(v)), corr; dims=1))[(always_count() + 1):end] # skip :SPY
    slopes_ltd = slopes_corr[(always_count() + 1):end] # skip :SPY
    global kscorr = scor_ltd
    global kslopes_corr = slopes_ltd
    scor = map(eachindex(scor_ltd)) do i
        # 1 / (1 + scor_corr[i])
        # m[end,i] / m[end - ret_ndays,i] / (1 + scor_corr[i])
        slopes_ltd[i] / (1 + scor_ltd[i])
    end
    @assert all(isfinite, scor)

    syms_scored = syms_all[corr_cols][(always_count() + 1):end] # skip :SPY
    global ksyms_scored = syms_scored
    @assert not_in(always_symbols(), syms_scored)

    # bonus = abs(StatsBase.quantile(slopes, 0.9))
    bonus = abs(maximum(scor)) / 1
    for i in eachindex(syms_scored)
        sym = syms_scored[i]
        if sym in bonuses
            scor[i] += bonus
        end
    end
    global kscor = scor

    scor_sort = sortperm(scor; rev=true)
    permute!(syms_scored, scor_sort)
    permute!(scor, scor_sort)
    res1 = first(syms_scored, count_per_sector)
    res2 = first(scor, count_per_sector)

    # keep_inds = filter(eachindex(res1)) do i
    #     scor[i] > 0.0 # || syms_scored[i] in bonuses
    # end
    # res1 = res1[keep_inds]
    # res2 = res2[keep_inds]

    # for i in eachindex(res1)
    #     if scor[i] < 0.0 && !(syms_scored[i] in bonuses)
    #         deleteat!(res1, i)
    #         deleteat!(res2, i)
    #     end
    # end
    return [(;sym=s, score=sc) for (s, sc) in zip(res1, res2)]
end

function make_targets(df_corr, cur_positions, get_cur_price, num_syms, ret_ndays)
    @assert ret_ndays < size(df_corr, 1)
    # return Dict([
    #     :AAPL => 0.1,
    #     :MSFT => 0.1,
    #     :GOOGL => 0.1,
    #     :AMZN => 0.1,
    #     :TSLA => 0.1,
    #     :NVDA => 0.1,
    #     :INTC => 0.1,
    #     :CSCO => 0.1,
    #     :ADBE => 0.1,
    #     :META => 0.1,
    #     # :LLY => 0.1,
    # ])
    # return Dict([
    #     # Symbol("BRK.B") => 0.1,
    #     :CL => 0.1,
    #     :JPM => 0.1,
    #     :PG => 0.1,
    #     :BAC => 0.1,
    #     :WFC => 0.1,
    #     :AXP => 0.1,
    #     :PFE => 0.1,
    #     :C => 0.1,
    #     :CI => 0.1,
    #     :LLY => 0.1,
    # ])

    target_ratio = 1 / num_syms

    syms_all = propertynames(df_corr)
    @assert !not_in(always_symbols(), syms_all)

    rets = [(;sym, ret = get_cur_price(sym)[2] / df_corr[1,sym]) for sym in syms_all]
    sort!(rets; rev=true, by=(r -> r.ret))
    syms = unique!(vcat([r.sym for r in first(rets, 199)], always_symbols()))
    df_corr = df_corr[!, syms]

    print("Correlating $(length(syms)) symbols... ")
    m = Matrix(df_corr)
    corr = StatsBase.cor(m)
    global kcorr = corr
    @assert all(!ismissing, corr)

    # draw_mat(reverse(corr; dims=1))
    # TODO: make this algo better
    scor_corr = vec(mapslices(v -> StatsBase.median(abs.(v)), corr; dims=1))
    global kscorr = scor_corr
    # scor = map(eachindex(scor_corr)) do i
    #     m[end,i] / m[end - ret_ndays,i] / (1 + scor_corr[i])
    # end
    scor = map(eachindex(scor_corr)) do i
        1 / (1 + scor_corr[i])
    end
    global kscor = scor
    @assert all(!ismissing, scor)
    sorted = sortperm(scor; rev=true)
    permute!(syms, sorted)
    permute!(scor, sorted)

    if !isempty(cur_positions)
        syms_dropped = setdiff(cur_positions, syms_all)
        @assert false # !(:SPY in syms_dropped)
        if !isempty(syms_dropped)
            new_positions = collect(filter(cur_positions) do sym
                !(sym in syms_dropped)
            end)
            syms_new = filter(s -> !(s in new_positions), syms)
            to_add = first(syms_new, length(syms_dropped))
            println("Syms dropped: $(syms_dropped), replacing with $(to_add)")
            println("length(new_positions) = $(length(new_positions)) | length(to_add) = $(length(to_add))")
            targets = push!(new_positions, to_add...)
        else
            # Find worst in cur_positions and best not in cur_positions
            worst = findlast(sym -> sym != :SPY && sym in cur_positions, syms)
            best = findfirst(sym -> !(sym in cur_positions), syms)

            if scor[best] > 1.4 * scor[worst]
                worst_sym = syms[worst]
                best_sym = syms[best]
                println("Replacing worst $(worst_sym):$(scor[worst]) with best $(best_sym):$(scor[best])")
                targets = collect(replace(cur_positions, worst_sym => best_sym))
            else
                targets = collect(cur_positions)
            end
            if length(targets) < num_syms
                println("Adding to targets $(typeof(targets)): $(length(targets)) < $(num_syms)")
                push!(targets, first(filter(s -> !(s in targets), syms), num_syms - length(cur_positions))...)
            end
        end
    else
        targets = vcat(first(syms, num_syms-1), :SPY)
    end
    global kcur_positions = cur_positions
    global ktargets = targets
    @assert length(targets) == num_syms string("length(targets) == $(num_syms) | ", length(targets))
    return Dict([s => target_ratio for s in targets])
end

#region Util
not_in(needles, haystack) = isempty(intersect(needles, haystack))

function detrend!(m::AbstractMatrix{T}) where T<:Real
    # calc slopes and detrend
    # return mapslices(m; dims=1) do v
    slopes = Vector{T}(undef, size(m, 2))
    for i in axes(m, 2)
        v = @view m[:,i]
        slope, offset = LsqFit.curve_fit(linear_model, eachindex(v), v, INIT_PARAM).param
        for i in eachindex(v)
            v[i] -= slope * i + offset
        end
        # println("$(eachindex(v)), slope: $(slope) | offset: $(offset)")
        # return slope
        slopes[i] = slope
    end
    return slopes
end
#endregion Util

#region Norgate Data
base_path() = Paths.db_incoming("norgate", "sp500-daily")
sectors_path() = joinpath(base_path(), "sectors.arrow")
sym_path(sym) = joinpath(base_path(), string(sym, ".arrow"))

function make_sectors()
    df_sectors = disallowmissing!(Paths.load_data(sectors_path(), DataFrame))
    SECTORS3[] = select(df_sectors, :Symbol => (s -> Symbol.(s)) => :sym, Not(:Symbol))
    for i in axes(df_sectors, 1)
        sym = Symbol(df_sectors[i, :Symbol])
        SECTORS2[sym] = df_sectors[i, :]
    end
end

function make_lookup()
    df_sectors = disallowmissing!(Paths.load_data(sectors_path(), DataFrame))
    syms = vcat(Symbol.(df_sectors.Symbol), always_symbols())
    df_dates = DataFrame(:date => dates_list())
    @assert issorted(df_dates.date)

    for sym in syms
        df_price_raw = select!(Paths.load_data(sym_path(sym), DataFrame), :Date => (x -> Date.(x)) => :date, Not(:Date))
        df_price = leftjoin(df_dates, df_price_raw; on=:date)
        # global kdf_price_raw = df_price_raw
        # global kdf_price = df_price
        select!(df_price, :date,
                :Open => missing_to_nan => :open,
                :High => missing_to_nan => :high,
                :Low => missing_to_nan => :low,
                :Close => missing_to_nan => :close,
                :Volume => missing_to_nan => :volume,
                Symbol("Unadjusted Close") => missing_to_nan =>:close_unadjusted
        )
        sort!(df_price, :date)

        df_constituency_raw = select!(Paths.load_data(joinpath(base_path(), string(sym, "_constituency.arrow")), DataFrame), :Date => (x -> Date.(x)) => :date, Not(:Date))
        # df_constituency = leftjoin(df_dates, Paths.load_data(joinpath(base_path(), sym * "_constituency.arrow"), DataFrame); on=:date => :Date)
        df_constituency = leftjoin(df_dates, df_constituency_raw; on=:date)
        # global kdf_constituency_raw = df_constituency_raw
        # global kdf_constituency = df_constituency
        select!(df_constituency, :date,
                Symbol("Index Constituent") => (v -> missing_to_false.(v)) => :include
        )
        if sym in always_symbols()
            df_constituency.include .= true
        end
        sort!(df_constituency, :date)

        @assert issorted(df_price.date)
        @assert issorted(df_constituency.date)

        DF_LOOKUP[sym] = DataLookup(df_price, df_constituency)
    end
end

missing_to_nan(v) = replace(v, missing => NaN32)
missing_to_false(x) = ismissing(x) ? false : Bool(x)
#endregion Norgate Data

#region Explore
using Makie
function draw_mat(data)
    m, n = size(data)
    fig = Figure(size = (1200, 600), fontsize = 20)
    ax = Axis(fig[1, 1]) # , xticks = (1:m, xticks), yticks = (1:n, yticks))
    hmap = heatmap!(ax, data, colormap = :plasma)
    for i in 1:m, j in 1:n
        txtcolor = data[i, j] < 0.15 ? :white : :black
        text!(ax, "$(round(data[i,j], digits = 2))", position = (i, j),
            color = txtcolor, align = (:center, :center))
    end
    display(fig)
end
#endregion Explore


end