include("repl-base.jl")

using Dates
using SH, BaseTypes, Globals, Bins, StratTypes, LegMetaTypes
using DateUtil, Scoring
using StoreUtil, Store
using Snapshots, Calendars, Markets, Expirations, Chains
using CmdStrats, CmdCheck, CmdTrading, CmdExplore
using SchedAll, Sched
isrtd() = !intest() && !dev() && isMarketOpen() && tenv() == :prod && isnothing(snap()) && urp() && Sched.ison()
snap(1)
an(;maxRun=100000, scorer=scoreRand, lmsPos=Vector{LegMeta}())
dr(1)
snop()
useDbProd()
todo()
devoff()
urpon()
SchedAll.start()
println("Ready to trade: ", isrtd())