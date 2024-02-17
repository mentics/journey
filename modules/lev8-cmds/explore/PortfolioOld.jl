module PortfolioOld
using Dates, HTTP, DataFrames
import StatsBase
using DateUtil, DictUtil, Paths, FilesArrow
using HistData

#region Types and Constants
const DATES = DateUtil.all_bdays(Date(2000,1,1), Date(2023,12,31))
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
Account() = Account(100000, Dict{Symbol, Float64}(), Transaction[])

const DF_ALL = Ref{DataFrame}()
#endregion Types and Constants

function run(;corr_ndays=30, num_syms=100, ret_ndays=4)
    global kbal = []
    acct = Account()
    global kacct = acct
    if !isassigned(DF_ALL)
        dfall = impute!(Paths.load_data(Paths.db_incoming("general", "SP500.arrow"), DataFrame))
        check_inside_missings(dfall)
        # remove_low_prices!(dfall, 1:corr_ndays)
        @assert all(issorted, dfall.tickers)
        @assert dfall.date == dates_list() string(symdiff(dfall.date, dates_list()))
        DF_ALL[] = dfall
        println("DF_ALL[] size: ", size(DF_ALL[]))
    end
    dfall = copy(DF_ALL[])
    global kdfall = dfall
    @assert size(dfall, 2) == 726

    for ind_curday in 3000:lastindex(dfall.date)
        # remove_low_prices!(dfall, ind_curday:ind_curday)

        corr_inds = (ind_curday - corr_ndays):(ind_curday - 1)
        not_missing = findall(!ismissing, dfall[!,i][ind_curday] for i in 1:size(dfall, 2))
        df = dfall[corr_inds, not_missing]
        global kdf = df

        get_cur_price = function(sym)
            x = last_not_missing(dfall[(ind_curday-10):ind_curday, sym])
            if isnothing(x)
                error("No price for $(sym)[$(ind_curday)] on $(dfall.date[ind_curday])")
            end
            x.open
        end

        targets = make_targets(df, keys(acct.quantities), num_syms, ret_ndays)
        # syms = vcat(keys(acct.quantities), keys(targets))
        # sort!(syms)
        # unique!(syms)

        # df_curday = DataFrame(map(syms) do sym
        #     sym => last_not_missing((@view dfall[(ind_curday - 10):ind_curday,sym]))
        # end)
        # df_curday.date = dfall.date[ind_curday:ind_curday]

        date = dfall[ind_curday,:date]
        update!(date, get_cur_price, acct, targets)
        push!(kbal, (;date, bal=calc_account_value(get_cur_price, acct)))
    end
    return acct
end

function update!(date, get_cur_price, acct, targets)
    to_remove = setdiff(keys(acct.quantities), keys(targets))
    if length(to_remove) > 8
        global kup = (;date, get_cur_price, acct, targets)
        error("too many to remove: $(length(to_remove))")
    end
    sell_all!(date, get_cur_price, acct, to_remove)
    buysell_to_target!(date, get_cur_price, acct, targets)
end

function buysell_to_target!(date, get_cur_price, acct, targets)
    cash_start = acct.cash
    total_value = calc_account_value(get_cur_price, acct)
    @show total_value, cash_start
    for (sym, target_ratio) in targets
        cur_price = get_cur_price(sym)
        cur_qty = get(acct.quantities, sym, 0.0)
        cur_value = cur_price * cur_qty
        cur_ratio = cur_value / total_value
        diff_ratio = target_ratio - cur_ratio
        diff_qty = round(Int, (diff_ratio * total_value) / cur_price, RoundDown)
        if diff_qty != 0
            tx = Transaction(date, sym, diff_qty, cur_price)
            push!(acct.transactions, tx)
            acct.quantities[sym] = get!(acct.quantities, sym, 0.0) + diff_qty
            acct.cash -= diff_qty * cur_price
        end
    end
    @assert (length(targets) - length(acct.quantities)) <= 1
    cash_end = acct.cash
    if cash_end < 0.0
        error("Cash went negative: $(cash_start) -> $(cash_end)")
    end
end

calc_account_value(get_cur_price, acct) = acct.cash + sum([qty * get_cur_price(sym) for (sym, qty) in acct.quantities]; init=0.0)

function sell_all!(date, get_cur_price, acct, syms)
    if !isempty(syms)
        println("Removing: $(syms)")
    end
    for sym in syms
        cur_price = get_cur_price(sym)
        value = acct.quantities[sym] * cur_price
        tx = Transaction(date, sym, -acct.quantities[sym], cur_price)
        push!(acct.transactions, tx)
        delete!(acct.quantities, sym)
        # acct.quantities[sym] = 0.0
        acct.cash += value
    end
end

function make_targets(df_corr, cur_positions, num_syms, ret_ndays)
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
    global kdf_corr = df_corr
    syms = intersect(df_corr.tickers[end], Symbol.(names(df_corr)))
    global ksyms = syms
    syms_cands = filter(syms) do sym
        !(sym in bad_syms()) && all(!ismissing, df_corr[!,sym])
    end
    # println("Filtered out $(length(df_corr.tickers[end]) - length(syms_cands))")

    # rets = [(;sym, ret=last_not_missing(df_corr[!,sym]).close - first_not_missing(df_corr[!,sym]).open) for sym in syms_cands]
    # sort!(rets; rev=true, by=(r -> r.ret))
    # syms = [r.sym for r in first(rets, 200)]
    syms = syms_cands

    df2 = DataFrame(map(syms) do sym
        # sym => [ismissing(x) ? missing : x.open for x in df_corr[!,sym]]
        sym => [x.close for x in df_corr[!,sym]]
    end)
    #     sym => ismissing(x) ? missing : x.open for x in dfall[!,sym])
    # for sym in syms
    #     # TODO: use all ohlc?
    #     df2[!,sym] =
    # end
    global kdf2 = df2

    # print("Correlating $(length(syms)) symbols... ")
    m = Matrix(df2)
    corr = StatsBase.cor(m)
    global kcorr = corr
    @assert all(!ismissing, corr)
    # println("done")
    # draw_mat(reverse(corr; dims=1))
    # TODO: make this algo better
    scor_corr = vec(mapslices(v -> StatsBase.median(abs.(v)), corr; dims=1))
    global kscorr = scor_corr
    scor = map(eachindex(scor_corr)) do i
        m[end,i] / m[end - ret_ndays,i] / (1 + scor_corr[i])
    end
    # scor = map(eachindex(scor_corr)) do i
    #     1 / (1 + scor_corr[i])
    # end
    global kscor = scor
    @assert all(!ismissing, scor)
    sorted = sortperm(scor; rev=true)
    permute!(syms, sorted)
    permute!(scor, sorted)

    if !isempty(cur_positions)
        syms_dropped = setdiff(cur_positions, syms)
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
            worst = findlast(sym -> sym in cur_positions, syms)
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
        targets = first(syms, num_syms)
    end
    global kcur_positions = cur_positions
    global ktargets = targets
    @assert length(targets) == num_syms string("length(targets) == $(num_syms) | ", length(targets))
    return Dict([s => target_ratio for s in targets])
end

#region Make Data
# TODO: survivorship bias problem here
function bad_syms()
    if isempty(BAD)
        append!(BAD, load_bad_syms())
    end
    return BAD
end
using FilesJLD2
load_bad_syms() = Paths.load_data(Paths.db_incoming("general", "missing-syms.jld2"), "syms")
const BAD = Vector{Symbol}()

import CSV
function make_arrow_file()
    dates = dates_list()
    # Downloaded from: https://github.com/fja05680/sp500
    dftickers = DataFrame(CSV.File(joinpath(Paths.db_incoming("general"), "SP500HistoricalComponentsChanges-12-30-2023.csv")))
    s = map(dftickers.tickers) do str
        filter(t -> !(t in bad_syms()), Symbol.(split(str, ',')))
    end
    tickers = map(dates) do date
        s[searchsortedlast(dftickers.date, date)]
    end
    df = DataFrame(date=dates, tickers=tickers)

    syms = sort!(unique(reduce(vcat, s)))

    foreach(syms) do sym
        data = allowmissing(reverse(HistData.dataDaily(dates[1], dates[end], string(sym))))
        if isempty(data)
            push!(BAD, sym)
        else
            row_dates = [row.date for row in data]
            if row_dates != dates
                global kd = (;sym, data, row_dates)
                # @assert row_dates[end] == dates[end]
                fillmissing!(data, row_dates, dates, missing)
                # ind = searchsortedfirst(dates, row_dates[1]) - 1
                # data = vcat(fill(missing, ind), data)
                # error("Dates don't match")
            end
            df[!,Symbol(sym)] = data
        end
    end
    Paths.save_data(Paths.db_incoming("general", "SP500.arrow"), df; update=true)
    return df
end

import Impute
function impute!(df)
    for i in 3:size(df, 2)
        v = df[:,i]
        ind_missing = findlast(!ismissing, v) + 1
        Impute.locf!(v)
        if ind_missing <= length(v)
            v[ind_missing:end] .= missing
        end
        df[!,i] = v
    end
    return df
end
#endregion Make Data

#region Validation
function check_inside_missings(df)
    for i in 3:size(df, 2)
        v = df[!,i]
        ind1 = findfirst(!ismissing, v)
        ind2 = findlast(!ismissing, v)
        @assert count(!ismissing, v) == (ind2 - ind1 + 1) "col $(i) has inside missings"
    end
end

function check_invalid_prices(df)
    for col in 3:size(df, 2)
        check_series(df[!,col], names(df)[col])
    end
end

function check_series(v, name="series")
    for i in Iterators.drop(eachindex(v), 2)
        check = v[(i-2):i]
        # (all(!ismissing, check) && v[i].open > 2.0) || continue
        all(!ismissing, check) || continue
        if abs(1.0 - v[i-1].close / v[i-2].close) > 0.5 &&
                abs(1.0 - v[i].close / v[i-1].close) > 0.5
            println("Found suspicous price series $([x.close for x in check]) in $(name) on $(v[i].date)")
        end
    end
end
#endregion Validation

#region Util
function last_not_missing(v)
    ind = findlast(!ismissing, v)
    isnothing(ind) ? nothing : v[ind]
end
function first_not_missing(v)
    ind = findfirst(!ismissing, v)
    isnothing(ind) ? nothing : v[ind]
end

function fillmissing!(data, keys, matches, value)
    ind_key = 1
    for i in eachindex(matches)
        match = matches[i]
        if lastindex(keys) < ind_key || keys[ind_key] != match
            insert!(data, i, value)
            # println("inserted $value at $ind_key")
        else
            ind_key += 1
        end
        # println(data)
    end
end
function test_fillmissing()
    data = [:c,:e]
    fillmissing!(data, [3,5], [1,2,3,4,5], :missing)
    @assert data == [:missing,:missing,:c,:missing,:e]

    data = [:c,:e]
    fillmissing!(data, [3,5], [1,2,3,4,5,6,7], :missing)
    @assert data == [:missing,:missing,:c,:missing,:e,:missing,:missing]
end

function remove_low_prices!(df, inds)
    remov = Int[]
    for col in 3:size(df, 2)
        if any(x -> !ismissing(x) && x.close <= 2.0, df[!,col][inds])
            push!(remov, col)
        end
    end
    if !isempty(remov)
        println("Removing symbols with low prices: $(names(df)[remov])")
        # select!(dfall, Not(Symbol(names(dfall)[col])))
        select!(df, Not(remov))
    end
    return
end

get_col(sym, c) = string(sym, "_", c)
#endregion Util

#region Tiingo Data
function make_tiingo_arrow()
    dates = dates_list()
    # Downloaded from: https://github.com/fja05680/sp500
    dftickers = DataFrame(CSV.File(joinpath(Paths.db_incoming("general"), "SP500HistoricalComponentsChanges-12-30-2023.csv")))
    tickers_by_ind = map(dftickers.tickers) do str
        Symbol.(split(str, ','))
    end
    tickers = map(dates) do date
        tickers_by_ind[searchsortedlast(dftickers.date, date)]
    end
    df = DataFrame(date=dates, tickers=tickers)

    syms = unique!(sort!(reduce(vcat, tickers_by_ind)))

    for sym in syms[1:70]
        !(sym in SYM_SKIP) || continue
        dfsymall = query_daily(sym)
        dfsym = filter(:date => (date -> dates[1] <= date <= dates[end]), dfsymall)
        # diff = symdiff(dates, dfsym.date)
        # if !isempty(diff)
        # if dfsym.date != dates
        #     global kdfsym = (;sym, dfsym, diff)
        #     error("dates don't match")
        #     # fillmissing!(data, row_dates, dates, missing)
        # end
        # cols = names(dfsym)[2:end]
        # for col in cols
        #     colname = get_col(sym, col)
        #     df[!,Symbol(colname)] = dfsym[!,Symbol(col)]
        # end
        # TODO: do we care if zero volume ealier?
        zi = findlast(!iszero, dfsym.volume)
        if !isnothing(zi) && zi < size(dfsym, 1)
            drows = zi:size(dfsym, 1)
            println("Zero volume for $(sym) on $(dfsym.date[zi]), deleteing $(drows)")
            (zi > size(dfsym, 1) / 2) || error("Zero volume in first half")
            DataFrames.deleteat!(dfsym, drows)
        end
        rename!(dfsym, [c => get_col(sym, c) for c in names(dfsym)[2:end]])
        leftjoin!(df, dfsym; on=:date) # , renamecols=identity => (c -> get_col(sym, c)))
    end
    # Paths.save_data(Paths.db_incoming("tiingo", "SP500.arrow"), df; update=true)
    return df
end

const TOKEN = "e756c79177cf09a27cf0fbfe6822a0ea9256ade0"
const BASE_URL = "https://api.tiingo.com/tiingo"
const TIINGO_DATE_FORMAT = dateformat"yyyy-mm-ddTHH:MM:SS.sss\Z"

tiingo_path_daily(sym) = Paths.db_incoming("tiingo", string(sym), "daily.arrow")

using Caches
function query_daily(sym="SPY"; age=DateUtil.FOREVER2) # age_daily())
    return cache!(DataFrame, Symbol("tiingo-daily-$(sym)"), age) do
        @assert !(sym in SYM_SKIP) "Tried to query $(sym) in skip list"
        sym = map_sym(sym)
        path = tiingo_path_daily(sym)
        if !isfile(path)
            url = "$(BASE_URL)/daily/$(sym)/prices?token=e756c79177cf09a27cf0fbfe6822a0ea9256ade0&startDate=1900-01-01"
            println("Querying daily: $(url)")
            resp = HTTP.get(url, Dict("Content-Type" => "application/json"); retry=false)
            # return map(s -> to_date(s), parseJson(String(resp.body), Dict)["response"])
            global kresp = resp
            body = String(resp.body)
            global kbody = body
            json = parseArray(body)
            global kjson = json
            proc_daily(sym, json)
        end
        return Paths.load_data(path, DataFrame)
    end
end

parseArray(body) = parseJson(body, Vector)

function proc_daily(sym, data)
    println(size(data))
    if size(data, 1) == 0
        println("No data for $(sym), adding to skip")
        push!(SYM_SKIP, sym)
    else
        df = select!(DataFrame(data), :date => (x -> Date.(x, TIINGO_DATE_FORMAT)) => :date, :open, :high, :low, :close, :volume, :adjOpen, :adjHigh, :adjLow, :adjClose, :adjVolume, :divCash, :splitFactor)
        Paths.save_data(tiingo_path_daily(sym), df)
    end
end

# Some components have changed names and can be found only under new name
const SYM_MAPPING = Dict{Symbol,Symbol}()
const SYM_SKIP = Set{Symbol}()
map_sym(sym) = get(SYM_MAPPING, Symbol(sym), sym)
function __init__()
    SYM_MAPPING[:ABC] = :COR
    SYM_MAPPING[:ABS] = :ACI
    SYM_MAPPING[:ABX] = :GOLD
    push!(SYM_SKIP, :ACKH, :ACS, :ADS, Symbol("AFS.A"), :AHM)

    # SYM_MAPPING[:ACKH] = :SKIP
    # SYM_MAPPING[:ACS] = :SKIP

    # SYM_MAPPING[:ADS] = :SKIP
    # SYM_MAPPING[Symbol("AFS.A")] = :SKIP
    # SYM_MAPPING[:AHM] = :SKIP
end
#endregion Tiingo Data

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