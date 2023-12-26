module DataTreasury
using Dates, Tables, CSV, HTTP
using Caches, DateUtil, DataConst, Paths, DataRead

function make_treasury()
    treasury_lookup = Dict{Date,Float32}()
    for y in year(DataConst.DATE_START):year(today())
        s = query_rates(y)
        csv = CSV.Rows(IOBuffer(s); types=(i, cn) -> i == 1 ? Date : Float32, dateformat="mm/dd/yyyy")
        ind = findfirst(Tables.schema(csv).names) do name_sym
            name_str = string(name_sym)
            occursin("COUPON", name_str) && occursin("13", name_str)
        end
        for row in csv
            treasury_lookup[row[1]] = row[ind]
        end
    end
    Paths.save_data(DataRead.file_treasury(); treasury_lookup)
    return treasury_lookup
end

#region Local
query_rates(y) = return cache!(String, Symbol("treasury-$(y)"), DateUtil.FOREVER2) do
    url = "https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/$(y)/all?type=daily_treasury_bill_rates&field_tdr_date_value=$(y)&page&_format=csv"
    println("Querying treasury for year: $(url)")
    resp = HTTP.get(url; retry=false)
    return String(resp.body)
end
#endregion Local

end