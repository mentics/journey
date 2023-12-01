module DataOptions
using Dates, DataFrames
using BaseTypes
using ThetaData, Paths, FilesArrow, Caches
import DateUtil
import DataXpirs:get_xpirs_for_dates

#region Standard API
function get_options(year, month; sym="SPY", age=DateUtil.age_daily())
    return cache!(DataFrame, Symbol("quotes-$(sym)-$(year)-$(month)"), age) do
        return load_data(file_options(year, month; sym), DataFrame)
    end
end

function make_options(year, month; sym="SPY")
    path = file_options(year, month; sym)
    date_start = Date(year, month, 1)
    date_end = Dates.lastdayofmonth(date_start)
    xpirs = get_xpirs_for_dates(date_start:date_end)
    df = mapreduce(vcat, xpirs) do xpir
        ThetaData.query_options(date_start, min(xpir, date_end), xpir)
    end
    # println("Completed acquiring data for ($(year), $(month)) in $(stop - start) seconds")
    sort!(df, [:style, :ts, :expir, :strike])
    save_data(path, df)
    return df
end

function update_options(; sym="SPY")
    # TODO: how to make sure that when month passes, previous month is finished?
    df = load_data(file_options(year, month; sym), DataFrame)
    last_ts = df.ts[end]
end
#endregion Standard API

#region Local
format_ym(year, month) = "$(year)-$(lpad(month, 2, '0'))"
file_options(year, month; sym="SPY") = joinpath(db_incoming("options"; sym), "quotes-$(sym)-$(format_ym(year, month)).arrow")
#endregion Local

end