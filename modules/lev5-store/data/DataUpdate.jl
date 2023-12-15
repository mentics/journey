module DataUpdate
using Dates
import DataXpirs, DataXpirts, DataTs, DataPrices, DataPricesAtXpirs, DataTsx, DataOptions

function update_all()
    DataTs.update_ts()
    DataPrices.update_prices()
    DataXpirs.update_xpir_dates()
    DataXpirts.update_xpirts()
    DataPricesAtXpirs.update_prices_at_xpirs()
    # TODO: update last month
    DataOptions.update_options(year(today()), month(today()))
    # DataOptions.update_options(year(today()), month(today()))
    # DataTsx.update_tsx()
end

end