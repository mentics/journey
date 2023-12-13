module DataUpdate
import DataTs, DataPrices, DataPricesAtXpirs, DataTsx

function update_all()
    DataTs.update_ts()
    DataPrices.update_prices()
    DataPricesAtXpirs.update_prices_at_xpirs()
    DataOptions.update_options()
    # DataTsx.update_tsx()
end

end