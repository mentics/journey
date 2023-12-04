module DataXpirs
using Dates
import DataStructures:SortedSet
using ThetaData, Paths, FilesJLD2, Caches
import DateUtil
import CollUtil:push_all!
import Calendars as cal

using DataRead

#=
Data modules pattern:
  get_*: data both from files and query recent if necessary
  make_*: save historical data to files
  update_*: update files with recent historical data
=#

#region Standard Api
function make_xpir_dates(;sym="SPY")
    xpirs = ThetaData.query_xpirs(sym)
    filter!(xpir -> xpir < cal.max_date(), xpirs)
    xdd = XpirDateDicts()
    for xpir in xpirs
        add_xpir_dates!(xdd, xpir; sym)
    end
    save_data(DataRead.file_xpirs(;sym); xpir_to_date, date_to_xpir)
end

function update_xpir_dates(;sym="SPY")
    xpirs_from_query = ThetaData.query_xpirs(sym)
    filter!(xpir -> xpir < cal.max_date(), xpirs_from_query)
    xdd = DataRead.load_xpir_dates(sym; age=DateUtil.FOREVER2)
    xpirs_from_file = keys(xdd.xpir_to_date)
    @assert maximum(xpirs_from_query) >= maximum(xpirs_from_file)
    to_proc = setdiff(xpirs_from_query, xpirs_from_file)
    path = DataRead.file_xpirs(;sym)
    if isempty(to_proc)
        println("Xpir_dates: no dates found to process")
        touch(path)
        return nothing
    end
    for xpir in to_proc
        add_xpir_dates!(xdd, xpir; sym)
    end
    println("saving... $(DataRead.file_xpirs(;sym))")
    save_data(path; xpir_to_date=xdd.xpir_to_date, date_to_xpir=xdd.date_to_xpir)
    return to_proc
end
#endregion Standard Api

#region Local
function add_xpir_dates!(xdd::XpirDateDicts, xpir; sym)
    dates = ThetaData.query_dates_for_xpir(xpir, sym)
    for date in dates
        push!(get!(Vector, xdd.xpir_to_date, xpir), date)
        push!(get!(Vector, xdd.date_to_xpir, date), xpir)
    end
end
#endregion Local

end