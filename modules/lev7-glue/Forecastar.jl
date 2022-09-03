module Forecastar
import Dates:Dates,Date
import Flux:Flux, gpu
using BaseTypes
import Forecast:Forecast,N

function config()
    return (;
        inputWidth = 12,
        inputLen = 50,
        outputInds = [4],
        castLen = 10,
        binCnt = 21,
        batchLen = 128,
        testHoldOut = .3,
        lossTarget = 0.001,
        maxIter = 10000
    )
end


end