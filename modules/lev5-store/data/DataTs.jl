module DataTs
using Paths, FilesJLD2
import DateUtil, DataCheck, DataRead

#=
Data modules pattern:
  get_*: data both from files and query recent if necessary
  make_*: save historical data to files
  update_*: update files with recent historical data
=#

#region Standard Api
function make_ts(;sym="SPY")
    tss = DateUtil.all_bdays_ts()
    filter!(x -> !(x in DataCheck.missing_allowed()), tss)
    Paths.save_data(DataRead.file_ts(;sym); tss)
    return tss
end

function update_ts(;sym="SPY")
    tss = DateUtil.all_bdays_ts()
    ts_last = DataRead.get_ts()[end]
    if tss[end] > ts_last
        Paths.save_data(DataRead.file_ts(;sym); tss)
    else
        println("DataTs already up to date $(ts_last)")
    end
end
#endregion Standard Api

end