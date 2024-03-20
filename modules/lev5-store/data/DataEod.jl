module DataEod
using Dates, DataFrames
using BaseTypes
using ThetaData, Paths, FilesArrow, Caches
import DateUtil
# import Calendars as cal # TODO: might not need this after fix market open ts missing

using DataConst, DataRead, DataCheck

#region Standard API
function make_options(;sym="UPRO")
    date_start = DateUtil.DEFAULT_DATA_START_DATE
    date_end = today()
    xpirs = ThetaData.query_xpirs(sym)
    @assert issorted(xpirs)
    @assert allunique(xpirs)
    xpirs = reverse(xpirs)

    dfs = map(xpirs) do xpir
        date = xpir < Date(2023,6,1) ? xpir - Month(3) : date_start
        d = ThetaData.query_options_eod(date, min(xpir, date_end), xpir; sym)
        !isnothing(d) || println("WARN: no data for xpir: $(xpir)")
        return d
    end
    df = reduce(vcat, filter(!isnothing, dfs))
    sort!(df, keycols())
    @assert allunique(df, keycols())
    Paths.save_data(DataRead.file_options_eod(;sym), df)
    return df
end
#endregion Standard API

#region Local
keycols() = [:style, :date, :expir, :strike]
#endregion Local

end
