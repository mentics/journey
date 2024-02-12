module Portfolio
using Dates, DataFrames
using DateUtil, Paths, FilesArrow
using HistData

const DATES = DateUtil.all_bdays(Date(2000,1,1), Date(2023,12,31))
dates_list() = DATES

function run(;corr_ndays=120)
    df = Paths.load_data(Paths.db_incoming("general", "SP500HistoricalSymbols.arrow"), DataFrame)
    @assert all(issorted, df.tickers)
    @assert df.date == dates_list()

    cash = 100000
    balances = Dict{Symbol, Float64}()
    transactions = []

    for i in (corr_ndays + 1):lastindex(dates_all)
        # date = df.date[i]

        targets = make_targets(df, ind, corr_ndays)
        update!(balances, transactions, targets)
        push!(transactions, tx)
    end
end

function update!(balances, transactions, targets)
end

function make_targets(df, ind, corr_ndays)
    res = Dict{Symbol, Float64}()
    corr_inds = (i - corr_ndays):(i - 1)
    syms = filter(df.tickers[ind]) do sym
        count(ismissing, df[corr_inds,sym]) / length(corr_inds) < 0.1
    end
    println("$(ind) filtered out $(length(df.tickers[ind]) - length(syms))")

    corr = Matrix(df[corr_inds, syms])
    draw_mat(corr)
    return res
end

using Makie
function draw_mat(data)
    m, n = size(corr)
    fig = Figure(size = (1200, 600), fontsize = 20)
    ax = Axis(fig[1, 1], xticks = (1:m, xticks), yticks = (1:n, yticks))
    hmap = heatmap!(ax, data, colormap = :plasma)
    for i in 1:m, j in 1:n
        txtcolor = data[i, j] < 0.15 ? :white : :black
        text!(ax, "$(round(data[i,j], digits = 2))", position = (i, j),
            color = txtcolor, align = (:center, :center))
    end
    display(fig)
end

# TODO: survivorship bias problem here
bad_syms() = [
    :AABA,:AAMRQ,:ABC,:ABI,:ABKFQ,:ABMD,:ABS,:ABX,:ACAS,:ACKH,:ACS,:ADS,:AET,Symbol("AFS.A"),:AGC,:AGN,:AHM,:AKS,:ALXN,
    :AMCC,:ANDV,:ANDW,:ANRZQ,:ANTM,:ANV,:APC,:APCC,:APOL,:ARG,:ARNC,:AS,:ASN,:AT,:ATVI,:AV,:AVP,:AW
]
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
    # Paths.save_data(Paths.db_incoming("general", "SP500.arrow"), df; update=true)
    return df
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

end