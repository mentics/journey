module DataUpdate
import DataPrices, DataTsx

function update_all()
    DataPrices.update_prices()
    # DataTsx.update_tsx()
end

end