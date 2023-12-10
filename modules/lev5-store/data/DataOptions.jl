module DataOptions
using Dates, DataFrames
using BaseTypes
using ThetaData, Paths, FilesArrow, Caches
import DateUtil
import DataXpirs:get_xpirs_for_dates

using DataConst, DataRead, DataCheck

#region Standard API
function make_options(year, month; sym="SPY")
    date_start = Date(year, month, 1)
    date_end = Dates.lastdayofmonth(date_start)
    # xpirs = get_xpirs_for_dates(date_start:date_end)
    xpirs = filter(xpir -> xpir - date_end <= DataConst.XPIRS_WITHIN, get_xpirs_for_dates(date_start:date_end))
    @assert issorted(xpirs)
    @assert allunique(xpirs)
    # df = mapreduce(vcat, xpirs) do xpir
    #     ThetaData.query_options(date_start, min(xpir, date_end), xpir)
    # end

    dfs = map(xpirs) do xpir
        d = ThetaData.query_options(date_start, min(xpir, date_end), xpir)
        isnothing(d) && println("WARN: no data for $(year)-$(month) xpir: $(xpir)")
        return d
    end
    df = reduce(vcat, filter(!isnothing, dfs))
    # println("Completed acquiring data for ($(year), $(month)) in $(stop - start) seconds")
    sort!(df, [:style, :ts, :expir, :strike])
    @assert allunique(df, [:style, :ts, :expir, :strike])
    Paths.save_data(DataRead.file_options(year, month; sym), df)
    return df
end

function update_options(year, month; sym="SPY")
    # TODO: how to make sure that when month passes, previous month is finished?
    df1 = Paths.load_data(year, month; sym, age=DateUtil.FOREVER2)
    last_ts = df1.ts[end]
    start_date = market_date(last_ts)
    end_date = Dates.lastdayofmonth(start_date)
    df2 = ThetaData.query_options(start_date, end_date; sym, age=Minute(15))
    df = combine_dfs(df1, df2)
    diff = check_ts(df.ts)
    if !isempty(diff)
        println("ERROR: DataOptions not all ts found. Not saved.")
        return diff
    end
    Paths.save_data(DataRead.file_options(year, month; sym), df; update=true)
    return
end
#endregion Standard API

#region Local
#endregion Local

end