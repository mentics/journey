module DataUpdate
import DataXpirs, DataXpirts, DataTs, DataPrices, DataPricesAtXpirs, DataTsx, DataOptions

function update_all()
    DataTs.update_ts()
    DataPrices.update_prices()
    DataXpirs.update_xpir_dates()
    DataXpirts.update_xpirts()
    DataPricesAtXpirs.update_prices_at_xpirs()
    # DataOptions.update_options()
    # DataTsx.update_tsx()
end

end