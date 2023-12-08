module DataRead
# Minimal dependencies to read data
using Dates, DataFrames
import DataStructures:SortedSet
using DateUtil, Paths, FilesArrow, FilesJLD2
import CollUtil:push_all!

export XpirDateDicts, get_xpir_dates, get_xpirs_for_dates, get_prices, get_options

function get_xpirts(;sym="SPY", age=DateUtil.age_daily())
    return Paths.load_data(file_xpirts(;sym), "xpirts"; age)
end

struct XpirDateDicts
    xpir_to_date::Dict{Date,Vector{Date}}
    date_to_xpir::Dict{Date,Vector{Date}}
end
XpirDateDicts() = XpirDateDicts(Dict{Date,Vector{Date}}(), Dict{Date,Vector{Date}}())

function get_xpir_dates(;sym="SPY", age=DateUtil.age_daily())::XpirDateDicts
    load_xpir_dates(sym; age)
end

function get_xpirs_for_dates(dates)
    xdd = get_xpir_dates()
    dates = filter(date -> haskey(xdd.date_to_xpir, date), dates)
    return collect(mapreduce(date -> xdd.date_to_xpir[date], push_all!, dates; init=SortedSet()))
end

function get_prices(;sym="SPY", age=DateUtil.age_daily())
    load_prices(;sym, age)
end
function price_lookup(;sym="SPY", age=DateUtil.age_daily())
    prices = get_prices(;sym, age)
    return Dict(prices.ts .=> prices.price)
end

function get_options(year, month; sym="SPY", age=DateUtil.age_daily())
    return Paths.load_data(file_options(year, month; sym), DataFrame)
end

function get_vix(;age=DateUtil.age_daily())
    return Paths.load_data(file_vix(), DataFrame)
end

function get_prices_at_xpirs(; sym="SPY", age=DateUtil.age_daily())
    return Paths.load_data(file_prices_at_xpirs(;sym), DataFrame; age)
end
price_xpir_lookup(prices_at_xpirs) = Dict(prices_at_xpirs.expir .=> prices_at_xpirs.price)

#=
:expir, :style, :strike, :bid, :bid_size, :bid_condition, :ask, :ask_size, :ask_condition
=#
function get_options_at_xpirs(; sym="SPY", age=DateUtil.age_daily())
    return Paths.load_data(file_options_at_xpirs(;sym), DataFrame; age)
end
option_xpir_lookup(options_at_xpirs) = function(xpir, style, strike)
    for row in eachrow(options_at_xpirs)
        if row.xpir == xpir && row.style == style && row.strike == strike
            return (;row.price_long, row.price_short)
        end
    end
    return nothing
end

#region Local
file_xpirts(;sym="SPY") = joinpath(Paths.db_incoming(;sym), "expirts-$(sym).jld2")

file_xpirs(;sym="SPY") = joinpath(Paths.db_incoming(;sym), "expirs-$(sym).jld2")
load_xpir_dates(sym="SPY"; age=DateUtil.age_daily())::XpirDateDicts = XpirDateDicts(Paths.load_data(file_xpirs(;sym), "xpir_to_date", "date_to_xpir"; age)...)

file_prices(;sym="SPY") = joinpath(Paths.db_incoming("prices"; sym), "prices-$(sym).arrow")
load_prices(;sym, age, copycols=false)::DataFrame = Paths.load_data(file_prices(;sym), DataFrame; age, copycols)

format_ym(year, month) = "$(year)-$(lpad(month, 2, '0'))"
file_options(year, month; sym="SPY") = joinpath(Paths.db_incoming("options"; sym), "quotes-$(sym)-$(format_ym(year, month)).arrow")
load_options(year, month; sym, age)::DataFrame = Paths.load_data(file_options(year, month; sym), DataFrame; age)

file_vix() = joinpath(Paths.db("market", "incoming", "tradier", "vix"), "vix-daily.arrow")

file_prices_at_xpirs(;sym="SPY") = joinpath(Paths.db_incoming("prices_at_xpirs"; sym), "prices_at_xpirs.arrow")
file_options_at_xpirs(;sym="SPY") = joinpath(Paths.db_incoming("options_at_xpirs"; sym), "options_at_xpirs.arrow")

file_tsx(;sym="SPY") = joinpath(Paths.db_incoming("tsx"; sym), "tsx.arrow")
#endregion Local

end