include("repl-simple.jl")

using DrawUtil, Pricing
import Backtests as bt
using BacktestUtil
import BacktestAnalysis as lyze
# import TestStrat as strat1
import SimpleStore as SS
import ChainUtil as ch
import LinesLeg as LL
# import StratPutSpread as pstrat
import StratButter as stratb

SS.loadTss()

bt.run(stratb.makeStrat(), DateTime(2020,1,1), DateTime(2020,2,1); maxSeconds=1) ; lyze.showResult()
bt.run(stratb.makeStrat(), DateTime(2020,1,1), DateTime(2020,2,1); maxSeconds=4) ; lyze.showResult()
# bt.run(strat1.makeStrat(), DateTime(2022,1,1), DateTime(2022,3,4); maxSeconds=1)
acc() = bt.keepAcct
tcs = acc().closed;


lms = tcs[1].open.lms
segs = LL.toSegmentsWithZeros(lms)
lyze.drawTrade(1)
# draw(:lines, toDraw(lms))

prob = stratb.probFor(tcs[1].open.ts, trad.getTargetDate(tcs[1]))
curp = prob.center
# commit = max(Pricing.calcMarg(curp, segs))