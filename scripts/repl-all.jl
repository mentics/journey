include("repl-base.jl")

using Dates
using SH, BaseTypes, Globals, Bins, SmallTypes, StratTypes, LegTypes, LegMetaTypes
using DateUtil, Scoring
using StoreUtil, Store
using Snapshots, Calendars, Markets, Expirations, Chains
using CmdStrats, CmdCheck, CmdTrading, CmdExplore
using SchedLive, Sched
isrtd() = !intest() && !dev() && isMarketOpen() && tenv() == :prod && isnothing(snap()) && urp() && Sched.ison()
devon()
snap(1)
an(;maxRun=100000, scorer=scoreRand, lmsPos=Vector{LegMeta}())
dr(1)
snop()
useDbProd()
todo()
devoff()
urpon()
SchedLive.add()
Sched.start()
println("Ready to trade: ", isrtd())