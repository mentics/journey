module GiantCondors
using Dates
using BaseTypes, SmallTypes
using DateUtil, ChainUtil
using Markets, Expirations, Chains
import Backtests as bt

function findGC(side=Side.long; xgte=60, off=20.0, prof=0.8)
    findEntry = side == Side.long ? bt.findLongSpreadEntry : bt.findShortSpreadEntry
    # TODO: loop through expirs to find balance and good greeks
    oqss = ChainUtil.getOqss(chain(xp.expirGte(bdaysAfter(today(), xgte))).chain, market().curp)
    lmss = findEntry(oqss, (;off, prof)) |> collect
end

end