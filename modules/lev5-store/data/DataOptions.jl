module DataOptions
using Dates, DataFrames
using BaseTypes
using ThetaData, Paths, FilesArrow, Caches
import DateUtil
import DataXpirs:get_xpirs_for_dates
import Calendars as cal # TODO: might not need this after fix market open ts missing

using DataConst, DataRead, DataCheck

# TODO: Not a bday: 2019-02-02

#region Standard API
function make_options(year, month; sym="SPY")
    date_start = Date(year, month, 1)
    date_end = min(DateUtil.market_today(), Dates.lastdayofmonth(date_start))
    xpirs = DataRead.get_xpirs_for_dates(date_start:date_end; age=(now(UTC) - DateTime(date_end)))
    filter!(xpir -> xpir - date_end <= DataConst.XPIRS_WITHIN_CALC, xpirs)
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
    sort!(df, keycols())
    @assert allunique(df, keycols())
    Paths.save_data(DataRead.file_options(year, month; sym), df)
    return df
end


function update_options(year, month; sym="SPY")
    # TODO: how to make sure that when month passes, previous month is finished?
    # df1 = Paths.load_data(year, month; sym, age=DateUtil.FOREVER2)
    path = DataRead.file_options(year, month; sym)
    tss = filter(x -> Dates.year(x) == year && Dates.month(x) == month, DataRead.get_ts(;sym))
    # TODO: deal with that thetadata is 15 minute delayed
    df1 = Paths.load_data(path, DataFrame; age=DateUtil.FOREVER2)
    last_ts = df1.ts[end]
    # if last_ts == tss[end]
    #     println("Options already up to date: $(last_ts)")
    #     touch(path)
    #     return
    # end

    start_date = DateUtil.bdaysBefore(DateUtil.market_date(last_ts), 1)
    # end_date = min(DateUtil.market_today(), Dates.lastdayofmonth(start_date))
    end_date = DateUtil.market_today()

    @show start_date end_date
    xpirs = DataRead.get_xpirs_for_dates(start_date:end_date; age=Minute(30))
    filter!(xpir -> xpir - end_date <= DataConst.XPIRS_WITHIN_CALC, xpirs)
    println("Found xpirs to process: $(xpirs)")
    @assert issorted(xpirs)
    @assert allunique(xpirs)

    df2 = mapreduce(vcat, xpirs) do xpir
        d = ThetaData.query_options(start_date, min(xpir, end_date), xpir)#; cache=false)
        isnothing(d) && error("WARN: no data for $(year)-$(month) xpir: $(xpir)")
        return d
    end
    global kdfs = (;df1, df2)
    @assert df1 isa DataFrame
    @assert df2 isa DataFrame
    df = DataCheck.combine_dfs(df1, df2; keycols=keycols())

    # TODO: thetadata query options doesn't return near market open, so maybe do something for that?
    tss = filter(ts -> ts != cal.getMarketOpen(Date(ts)), tss)
    diff = symdiff(df.ts, tss)
    if !isempty(diff)
        println("ERROR: DataOptions not all ts found. Not saved.")
        return diff
    end

    Paths.save_data(DataRead.file_options(year, month; sym), df; update=true)
    return
end
#endregion Standard API

#region Local
keycols() = [:style, :ts, :expir, :strike]
#endregion Local

end

#=
WARN: too few strikes for 2017-09-27T20:00:00 2017-10-25T20:00:00
WARN: too few strikes for 2017-03-01T19:30:00 2017-03-08T21:00:00
WARN: too few strikes for 2017-03-01T20:00:00 2017-03-08T21:00:00
WARN: too few strikes for 2017-03-01T20:30:00 2017-03-08T21:00:00
WARN: too few strikes for 2017-03-27T20:00:00 2017-04-12T20:00:00
WARN: too few strikes for 2017-03-01T19:30:00 2017-03-08T21:00:00
WARN: too few strikes for 2017-03-01T20:00:00 2017-03-08T21:00:00
WARN: too few strikes for 2017-03-01T20:30:00 2017-03-08T21:00:00
=#