include("repl-trade.jl")

using Dates
using SH, BaseTypes, Globals, Bins, SmallTypes, StratTypes, LegTypes, LegMetaTypes
using DateUtil, Scoring
using StoreUtil, Store
using Snapshots, Calendars, Markets, Expirations, Chains
using CmdStrats, CmdCheck, CmdTrading, CmdExplore, CmdSched
using ProcOrder
using SchedLive, Sched
xx = CmdExplore

isrtd() = !intest() && !dev() && isMarketOpen() && tenv() == :prod && isnothing(snap()) && urp() && Sched.ison();
devon();
snap(5,24,0,0);

bbres = xx.bball();

an(1;maxRun=100000, scorer=scoreRand, lmsPos=Vector{LegMeta}());
dr(1);
drsh("s400p@1 / l402p@2 / l404c@2 / s406c@1", expirs()[1:3]);

snop();

useDbProd();
display(todo());
dbChecks();
devoff();
procOrders();
SchedLive.add();
println("Ready to trade: ", isrtd());

using Sched, SchedBg
SchedBg.add()
Sched.start()

bbres = xx.bball();
xx.bbToClose();
