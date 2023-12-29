module DataRead
# Minimal dependencies to read data
using Dates, DataFrames
import DataStructures:SortedSet
using DateUtil, Paths, FilesArrow, FilesJLD2
import CollUtil:push_all!

export XpirDateDicts

#region Api
function get_ts(;sym="SPY", age=DateUtil.age_period())
    return Paths.load_data(file_ts(;sym), "tss"; age)
end

function get_xpirts(;sym="SPY", age=DateUtil.age_daily())
    return Paths.load_data(file_xpirts(;sym), "xpirts"; age)
end
get_xpirts_set(;sym="SPY", age=DateUtil.age_daily()) = Set(get_xpirts(;sym, age))

struct XpirDateDicts
    xpir_to_date::Dict{Date,Vector{Date}}
    date_to_xpir::Dict{Date,Vector{Date}}
end
XpirDateDicts() = XpirDateDicts(Dict{Date,Vector{Date}}(), Dict{Date,Vector{Date}}())

function get_xpir_dates(;sym="SPY", age=DateUtil.age_daily())::XpirDateDicts
    load_xpir_dates(sym; age)
end

function get_xpirs_for_dates(dates; age=DateUtil.age_daily())
    xdd = get_xpir_dates(;age)
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

function get_options(year, month; sym="SPY", age=(today() - Date(year, month, 1)))
    return Paths.load_data(file_options(year, month; sym), DataFrame; age)
end
function get_options_yms()
    names = readdir(dirname(DataRead.file_options(2012, 6)))
    # [match(r"quotes-SPY-(\d{4})-(\d{2}).arrow", f).captures[1:2] for f in fs]
    avail = [(;year=y, month=m) for (y, m) in sort!([parse.(Int, match(r"quotes-SPY-(\d{4})-(\d{2}).arrow", name).captures[1:2]) for name in names])]
    all = DateUtil.year_months()
    diff = symdiff(all, avail)
    return (;all, avail, diff)
end

function get_vix(;age=DateUtil.age_daily())
    return Paths.load_data(file_vix(), DataFrame; age)
end

function get_prices_at_xpirts(; sym="SPY", age=DateUtil.age_daily())
    return Paths.load_data(file_prices_at_xpirs(;sym), DataFrame; age)
end
function price_lookup_xpirts(;sym="SPY", age=DateUtil.age_daily())
    prices = get_prices_at_xpirts(;sym, age)
    return Dict(prices.expir .=> prices.price)
end

function get_tsx(; sym="SPY", age=DateUtil.age_daily())
    return Paths.load_data(file_tsx(;sym), DataFrame; age)
end

# TODO: where to define the names to not depend on the modules?
# import Caches:cache!
# function prob_for_tsxp(; sym="SPY", age=DateUtil.age_period())
#     return cache!(Dict{DateTime,Dict{DateTime,Vector{Float32}}}, Symbol("prob_for_tsxp-$(sym)"), age) do
#         df, _ = Paths.load_data_params(Paths.db_output("ReturnProb"), DataFrame; age)
#         d = Dict{DateTime,Dict{DateTime,Vector{Float32}}}()
#         for (key, sdf) in pairs(groupby(df, :ts))
#             d[key.ts] = Dict(sdf.expir .=> sdf.output)
#         end
#         return d
#     end
# end
function prob_for_tsxp(; age=DateUtil.age_period())
    df, _, path = Paths.load_data_params(Paths.db_output("ReturnProb"), DataFrame; age)
    println("Loaded prob_for_tsxp from $(path)")
    gdf = groupby(df, [:ts, :expir])
    return first(df.ts), function(ts, xpirts)
        only(gdf[(ts, xpirts)].output)
    end
end

# #=
# :expir, :style, :strike, :bid, :bid_size, :bid_condition, :ask, :ask_size, :ask_condition
# =#
# function get_options_at_xpirs(; sym="SPY", age=DateUtil.age_daily())
#     return Paths.load_data(file_options_at_xpirs(;sym), DataFrame; age)
# end
# option_xpir_lookup(options_at_xpirs) = function(xpir, style, strike)
#     for row in eachrow(options_at_xpirs)
#         if row.xpir == xpir && row.style == style && row.strike == strike
#             return (;row.price_long, row.price_short)
#         end
#     end
#     return nothing
# end

function get_treasury_lookup(; age=DateUtil.age_daily())
    lup = Paths.load_data(file_treasury(), "treasury_lookup"; age)
    lup_min = minimum(keys(lup))
    return function(orig_date::Date)
        date = DateUtil.lastTradingDay(orig_date)
        while true
            r = get(lup, date, nothing)
            !isnothing(r) && return r
            date = DateUtil.bdaysBefore(date, 1)
            date >= lup_min || error("No treasury lookup found for $(date)")
        end
    end
end
#endregion Api

#region Local
file_xpirts(;sym="SPY") = joinpath(Paths.db_thetadata(;sym), "expirts-$(sym).jld2")

file_xpirs(;sym="SPY") = joinpath(Paths.db_thetadata(;sym), "expirs-$(sym).jld2")
load_xpir_dates(sym="SPY"; age=DateUtil.age_daily())::XpirDateDicts = XpirDateDicts(Paths.load_data(file_xpirs(;sym), "xpir_to_date", "date_to_xpir"; age)...)

file_prices(;sym="SPY") = joinpath(Paths.db_thetadata("prices"; sym), "prices-$(sym).arrow")
load_prices(;sym, age, copycols=false)::DataFrame = Paths.load_data(file_prices(;sym), DataFrame; age, copycols)

format_ym(year, month) = "$(year)-$(lpad(month, 2, '0'))"
file_options(year, month; sym="SPY") = joinpath(Paths.db_thetadata("options"; sym), "quotes-$(sym)-$(format_ym(year, month)).arrow")
load_options(year, month; sym, age)::DataFrame = Paths.load_data(file_options(year, month; sym), DataFrame; age)

file_vix() = joinpath(Paths.db("market", "incoming", "tradier", "vix"), "vix-daily.arrow")
file_prices_at_xpirs(;sym="SPY") = joinpath(Paths.db_thetadata("prices_at_xpirs"; sym), "prices_at_xpirs.arrow")
file_tsx(;sym="SPY") = joinpath(Paths.db_thetadata("tsx"; sym), "tsx.arrow")
file_ts(;sym="SPY") = joinpath(Paths.db_thetadata("ts"; sym), "ts.arrow")

file_treasury() = joinpath(Paths.db_incoming("treasury", "treasury.jld2"))
# file_options_at_xpirs(;sym="SPY") = joinpath(Paths.db_thetadata("options_at_xpirs"; sym), "options_at_xpirs.arrow")
#endregion Local

end