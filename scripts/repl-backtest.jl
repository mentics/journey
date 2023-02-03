include("repl-simple.jl")

import Backtests as bt
import BacktestAnalysis as lyze
import TestStrat as strat1
import SimpleStore as sstor
import ChainUtil as ch
import LinesLeg as LL

bt.run(strat1.makeStrat(), DateTime(2022,1,1), DateTime(2022,3,4); maxSeconds=1)
lms = bt.keepAcct.open[1].lms
segs = LL.toSegments(lms)
z = LL.toSections(segs)
