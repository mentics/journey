module DataXpirts
using Dates
using ThetaData, Paths, FilesJLD2
import DateUtil
import Calendars as cal

using DataRead

#=
Data modules pattern:
  get_*: data both from files and query recent if necessary
  make_*: save historical data to files
  update_*: update files with recent historical data
=#

#region Standard Api
function make_xpirts(;sym="SPY")
    xpirs = ThetaData.query_xpirs(sym)
    filter!(xpir -> xpir < cal.max_date(), xpirs)
    xpirts = ThetaData.to_xpirts.(xpirs)
    Paths.save_data(DataRead.file_xpirts(;sym); xpirts)
end

function update_xpirts(;sym="SPY")
    xpirs = ThetaData.query_xpirs(sym)
    filter!(xpir -> xpir < cal.max_date(), xpirs)
    xpirts = DataRead.get_xpirts(; sym, age=DateUtil.FOREVER2)
    @assert issorted(xpirs)
    @assert issorted(xpirts)
    last_xpirts = xpirts[end]
    if xpirs[end] > last_xpirts
        ind = searchsortedfirst(xpirs, DateUtil.market_date(last_xpirts) + Day(1))
        to_append = ThetaData.to_xpirts.(xpirs[ind:end])
        xpirts = vcat(xpirts, to_append)
        @assert issorted(xpirts)
        @assert allunique(xpirts)
        @assert length(xpirs) == length(xpirts)
        Paths.save_data(DataRead.file_xpirts(;sym); xpirts, update=true)
    else
        println("xpirts already up to date $(xpirts[end])")
        touch(DataRead.file_xpirts(;sym))
        return nothing
    end
end
#endregion Standard Api

end