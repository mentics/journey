include("repl-simple.jl")

using DrawUtil
using Calendars, Markets, Chains, Expirations
using Store, StoreTrade, ProcOrder, TradeInfo
using CmdTrading, CmdPos

import Combos as c, SeekingAlpha as sa
# import Backtests as bt, GiantCondors as gc

dbChecks()
procOrders()
# display(todo())
# toc()
# todup()
# x3(1)
# xdr(5)
tradesOpen()

# c.findRoll("F", 14, .12, Style.put)
# c.findRoll("F", 14.5, 0, Style.call)

import CmdTradeStrat as darts
darts.run()

# devon()
# so(r.lms)
# devoff()

# gc.live(Side.long)
# bt.run(gc.strat, gc.stratDay, 2022; maxSeconds=2)

# jorn(1:2)
# jorn(2:3; all=true)
# i = 2 ; r = j.ress[i][1] ; xdr(i, r.lms)

# j.runlc(5; maxSpreads=100, maxIter=1000)
