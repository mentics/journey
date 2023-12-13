module DataPricesAtXpirs
using Dates, DataFrames
using ThetaData, Paths
import DateUtil, PricingBase
using DataRead, DataXpirts, DataPrices

#=
Data modules pattern:
  get_*: data both from files and query recent if necessary
  make_*: save historical data to files
  update_*: update files with recent historical data
=#

#region Standard Api
function make_prices_at_xpirs(;sym="SPY")
    xpirtss = get_xpirtss(;sym)
    df = proc(xpirtss)
    Paths.save_data(DataRead.file_prices_at_xpirs(;sym), df)
    return df
end

function update_prices_at_xpirs(;sym="SPY")
    # updated in DataUpdate
    # DataXpirts.update_xpirts(;sym)
    # DataPrices.update_prices(;sym)

    xpirtss_all = get_xpirtss(;sym)
    @assert issorted(xpirtss_all)
    df = DataRead.get_prices_at_xpirts(; sym, age=DateUtil.FOREVER2)
    @assert issorted(df.expir)
    last_proced_xpirts = df.expir[end]
    if xpirtss_all[end] > last_proced_xpirts
        ind = searchsortedfirst(xpirtss_all, last_proced_xpirts + Hour(1))
        # @show ind lastindex(xpirtss_all)
        df_append = proc(xpirtss_all[ind:end])
        df = vcat(df, df_append)
        Paths.save_data(DataRead.file_prices_at_xpirs(;sym), df; update=true)
        return xpirtss_all[ind:end]
    else
        println("options_at_xpirs already up to date $(last_proced_xpirts)")
        touch(DataRead.file_prices_at_xpirs(;sym))
        return nothing
    end
end
#endregion Standard Api

#region Local
get_xpirtss(;sym) = filter(xpirts -> xpirts < now(UTC), DataRead.get_xpirts(;sym))

function proc(xpirtss)
    df = DataRead.get_prices()
    df = rename(df, :ts => :expir)
    return mapreduce(vcat, xpirtss) do xpirts
        ind = searchsortedfirst(df.expir, xpirts)
        @assert df.expir[ind] == xpirts
        return df[ind:ind,:]
    end
end
# function add_xpir_dates!(xdd::XpirDateDicts, xpir; sym)
#     dates = ThetaData.query_dates_for_xpir(xpir, sym)
#     for date in dates
#         push!(get!(Vector, xdd.xpir_to_date, xpir), date)
#         push!(get!(Vector, xdd.date_to_xpir, date), xpir)
#     end
# end
#endregion Local

end