@assert Threads.nthreads() >= 12
using Pkg
Pkg.activate(".")

include("../repl-backtest.jl")
include("../repl-trade.jl")
include("../repl-ml.jl")

# using Snapshots
# import TestUtil:TU

# devon()
# # TU.testStart()
# # snap(1)
# so(r.lms)
# # TU.testStop()
# devoff()
# # snop()

# ll = c.lookAll(sa.getCandidates(;maxSyms=10); ratio=.95)


# using Dates
# using SH, BaseTypes, Globals, Bins, SmallTypes, StratTypes, LegTypes, LegQuoteTypes
# using DateUtil, Scoring
# using StoreUtil, Store
# using Snapshots, Calendars, Markets, Expirations, Chains
# using CmdStrats, CmdCheck, CmdTrading, CmdExplore, CmdSched
# using ProcOrder
# using SchedLive, Sched
# xx = CmdExplore

# isrtd() = !intest() && !dev() && isMarketOpen() && tenv() == :prod && isnothing(snap()) && urp() && Sched.ison();
# devon();
# snap(5,24,0,0);

# # an(1;maxRun=100000, scorer=scoreRand, lmsPos=Vector{LegQuote}());
# # dr(1);
# drsh("s400p@1 / l402p@2 / l404c@2 / s406c@1", expirs()[1:3]);

# snop();
# @show market()

# useDbProd();
# display(todo());
# dbChecks();
# devoff();
# procOrders();
# SchedLive.add();
# println("Ready to trade: ", isrtd());

# using Sched, SchedBg
# SchedBg.add()
# Sched.start()

# bbres = xx.bball();
# xx.bbToClose();

# using Combos, SeekingAlpha, Joe ; c = Combos ; sa = SeekingAlpha ; j = Joe
# ll = c.lookAll()

# using Forecast ; fc = Forecast ; using ForecastSpy ; fs = ForecastSpy
# fs.run()

# println("repl-all done.")

exit()
