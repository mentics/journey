module Miriam
using HistData

#=
1) Get daily ohlc data for spy and vix
2) For the target ts, get it for
    date = Date(ts)
    (date-):(bdaysBefore(date, 1)
=#
function run()
    HistData.dataDaily()

end



end