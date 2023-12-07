module DataRead
# Minimal dependencies to read data
using Dates, DataFrames
import DataStructures:SortedSet
using DateUtil, Paths, FilesArrow, FilesJLD2
import CollUtil:push_all!
# Caches,

export XpirDateDicts, get_xpir_dates, get_xpirs_for_dates, get_prices, get_options

struct XpirDateDicts
    xpir_to_date::Dict{Date,Vector{Date}}
    date_to_xpir::Dict{Date,Vector{Date}}
end
XpirDateDicts() = XpirDateDicts(Dict{Date,Vector{Date}}(), Dict{Date,Vector{Date}}())

function get_xpir_dates(;sym="SPY", age=DateUtil.age_daily())::XpirDateDicts
    # return cache!(XpirDateDicts, Symbol("xpirs-dates-$(sym)"), age) do
        load_xpir_dates(sym; age)
    # end
end
# clear_xpir_cache(;sym="SPY") = Caches.clear!(Symbol("xpirs-dates-$(sym)"))

function get_xpirs_for_dates(dates)
    xdd = get_xpir_dates()
    dates = filter(date -> haskey(xdd.date_to_xpir, date), dates)
    return collect(mapreduce(date -> xdd.date_to_xpir[date], push_all!, dates; init=SortedSet()))
end

function get_prices(;sym="SPY", age=DateUtil.age_daily())
    # return cache!(DataFrame, Symbol("prices-$(sym)"), age) do
        load_prices(;sym, age)
    # end
end
# clear_prices_cache(;sym="SPY") = Caches.clear!(Symbol("prices-$(sym)"))

function get_options(year, month; sym="SPY", age=DateUtil.age_daily())
    # return cache!(DataFrame, Symbol("options-$(sym)-$(year)-$(month)"), age) do
        return load_data(file_options(year, month; sym), DataFrame)
    # end
end
# clear_options_cache(;sym="SPY") = Caches.clear!(Symbol("options-$(sym)-$(year)-$(month)"))

function get_vix(;age=DateUtil.age_daily())
    return load_data(file_vix(), DataFrame)
end

#region Local
file_xpirs(;sym="SPY") = joinpath(Paths.db_incoming(;sym), "expirs-$(sym).jld2")
load_xpir_dates(sym="SPY"; age=DateUtil.age_daily())::XpirDateDicts = XpirDateDicts(load_data(file_xpirs(;sym), "xpir_to_date", "date_to_xpir"; age)...)

file_prices(;sym="SPY") = joinpath(Paths.db_incoming("prices"; sym), "prices-$(sym).arrow")
load_prices(;sym, age, copycols=false)::DataFrame = load_data(file_prices(;sym), DataFrame; age, copycols)

format_ym(year, month) = "$(year)-$(lpad(month, 2, '0'))"
file_options(year, month; sym="SPY") = joinpath(Paths.db_incoming("options"; sym), "quotes-$(sym)-$(format_ym(year, month)).arrow")
load_options(year, month; sym, age)::DataFrame = load_data(file_options(year, month; sym), DataFrame; age)

file_vix() = joinpath(Paths.db("market", "incoming", "tradier", "vix"), "vix-daily.arrow")
#endregion Local

end