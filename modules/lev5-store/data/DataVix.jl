module DataVix
using Dates, DataFrames
using DateUtil, Paths, FilesArrow
using TradierData
import DataConst:DATE_START
import DataRead
import DataCheck

#region Standard API
function make_vix()
    raw = TradierData.tradierHistQuotes("daily", DATE_START, DateUtil.market_today(), "VIX")
    df = DataFrame(raw)
    select!(df,
            :date => (ds -> Date.(ds)) => :date,
            :open => f32 => :open,
            :high => f32 => :high,
            :low => f32 => :low,
            :close => f32 => :close)
    diff = check_dates(df.date)
    if !isempty(diff)
        println("ERROR: DataVix dates didn't match expected. Not saving.")
        return diff
    end
    Paths.save_data(DataRead.file_vix(), df)
    return df
    # dates_df = DataFrame(:date => collect(DateUtil.all_weekdays()))
    # vix_alldates_df = sort!(leftjoin(dates_df, df, on=:date), [:date])
    # save(path_vix_alldates(), vix_alldates_df)
    # return vix_alldates_df
end

function update_vix()
    df1 = DataRead.get_vix(;age=DateUtil.FOREVER2)
    last_date = df1.date[end]
    if last_date == DateUtil.market_today()
        println("VIX already up to date $(last_date)")
        return nothing
    end
    raw = TradierData.tradierHistQuotes("daily", last_date + Day(1), DateUtil.market_today(), "VIX")
    if isempty(raw)
        println("VIX no new data")
        touch(DataRead.file_vix())
        return nothing
    end
    df2 = DataFrame(raw)
    select!(df2,
            :date => (ds -> Date.(ds)) => :date,
            :open => f32 => :open,
            :high => f32 => :high,
            :low => f32 => :low,
            :close => f32 => :close)
    df = DataCheck.combine_dfs(df1, df2; keycols=[:date])
    diff = check_dates(df.date)
    if !isempty(diff)
        println("ERROR: DataVix dates didn't match expected. Not saving.")
        return diff
    end
    Paths.save_data(DataRead.file_vix(), df; update=true)
    return df
end
#endregion Standard API

#region Util
check_dates(dates) = symdiff(Iterators.filter(DateUtil.isbd, DateUtil.all_weekdays()), dates)

f32(v::Vector) = Float32.(v)
#endregion Util

end