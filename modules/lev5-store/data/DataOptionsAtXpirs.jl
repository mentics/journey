module DataOptionsAtXpirs
using Dates, DataFrames
# import DataStructures:SortedSet
using ThetaData, Paths
import DateUtil, PricingBase

using DataRead

# TODO: might need to update this to use DataRead.get_xpirts but not sure... or at least validate between them?

#=
Data modules pattern:
  get_*: data both from files and query recent if necessary
  make_*: save historical data to files
  update_*: update files with recent historical data
=#

#region Standard Api
function make_options_at_xpirs(;sym="SPY")
    xpirs = filter(xpir -> xpir < DateUtil.market_today(), ThetaData.query_xpirs(sym))
    df = proc(xpirs)
    Paths.save_data(DataRead.file_options_at_xpirs(;sym), df)
    return df
end

function update_options_at_xpirs(;sym="SPY")
    xpirs = ThetaData.query_xpirs(sym)
    @assert issorted(xpirs)
    filter!(xpir -> xpir < DateUtil.market_today(), xpirs)
    df = DataRead.get_options_at_xpirs(; sym, age=DateUtil.FOREVER2)
    @assert issorted(df.expir)
    last_expir = df.expir[end]
    if xpirs[end] > last_expir
        ind = searchsortedfirst(xpirs, last_expir + Day(1))
        df_append = proc(xpirs[ind:end])
        df = vcat(df, df_append)
        # Paths.save_data(DataRead.file_options_at_xpirs(;sym), df; update=true)
        return xpirs[ind:end]
    else
        println("options_at_xpirs already up to date $(last_expir)")
        return nothing
    end
end
#endregion Standard Api

#region Local
function proc(xpirs)
    df = mapreduce(vcat, xpirs) do xpir
        df = DataRead.get_options(year(xpir), month(xpir))
        return filter(r -> (Date(r.expir) == xpir) && (r.ts == r.expir), df; view=true)
    end
    # for r in eachrow(df); (@assert r.ts == r.expir) end
    select!(df, Not(:ts))
    if hasproperty(df, :under)
        select!(df, Not(:under))
    end
    transform!(df, [:bid,:ask] => ((b, a) -> PricingBase.quote_long.(b, a)) => :price_long, [:bid,:ask] => ((b, a) -> PricingBase.quote_short.(b, a)) => :price_short)
    return df
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