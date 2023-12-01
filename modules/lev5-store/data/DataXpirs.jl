module DataXpirs
using Dates
import DataStructures:SortedSet
using ThetaData, Paths, FilesJLD2
import DateUtil
import CollUtil:push_all!
import Calendars as cal

#=
Data modules pattern:
  get_*: data both from files and query recent if necessary
  make_*: save historical data to files
  update_*: update files with recent historical data
=#

export XpirDateDicts
struct XpirDateDicts
    xpir_to_date::Dict{Date,Vector{Date}}
    date_to_xpir::Dict{Date,Vector{Date}}
end
XpirDateDicts() = XpirDateDicts(Dict{Date,Vector{Date}}(), Dict{Date,Vector{Date}}())

#region Standard Api
function get_xpir_dates(sym="SPY"; age=age_daily())::XpirDateDicts
    return cache!(XpirDateDicts, Symbol("expirs-dates-$(sym)"), age) do
        load_xpir_dates(sym; age)
    end
end

function make_xpir_dates(sym="SPY")
    xpirs = ThetaData.query_xpirs(sym)
    filter!(xpir -> xpir < cal.max_date(), xpirs)
    xdd = XpirDateDicts()
    for xpir in xpirs
        add_xpir_dates!(xdd, xpir; sym)
    end
    save_data(file_expirs(;sym); xpir_to_date, date_to_xpir)
end

function update_xpir_dates(sym="SPY")
    xpirs_from_query = ThetaData.query_xpirs(sym)
    filter!(xpir -> xpir < cal.max_date(), xpirs)
    xdd = load_xpir_dates(sym; age=DateUtil.FOREVER2)
    xpirs_from_file = keys(xdd.xpir_to_date)
    @assert maximum(xpirs_from_query) >= maximum(xpirs_from_file)
    to_proc = setdiff(xpirs_from_query, xprs_from_file)
    !isempty(to_proc) || return nothing
    for xpir in to_proc
        add_xpir_dates!(xdd, xpir; sym)
    end
    save_data(file_expirs(;sym); xpir_to_date=xdd.xpir_to_date, date_to_xpir=xdd.date_to_xpir)
    return to_proc
end
#endregion Standard Api

#region Extra Api
function get_xpirs_for_dates(dates)::XpirDateDicts
    _, date_to_xpir = get_xpir_dates()
    dates = filter(date -> haskey(date_to_xpir, date), dates)
    return collect(mapreduce(date -> date_to_xpir[date], push_all!, dates; init=SortedSet()))
end
#endregion Extra Api

#region Local
file_expirs(;sym="SPY") = joinpath(db_incoming(;sym), "expirs-$(sym).jld2")

load_xpir_dates(sym="SPY"; age=age_daily())::XpirDateDicts = XpirDateDicts(load_data(file_expirs(;sym), "xpir_to_date", "date_to_xpir"; age)...)

function add_xpir_dates!(xdd::XpirDateDicts, xpir; sym)
    dates = ThetaData.query_dates_for_xpir(xpir, sym)
    for date in dates
        push!(get!(Vector, xdd.xpir_to_date, xpir), date)
        push!(get!(Vector, xdd.date_to_xpir, date), xpir)
    end
end
#endregion Local

end