include("repl-simple.jl")

using Calendars, Markets, Chains, Expirations, Sched
using Store, StoreTrade, ProcOrder
using CmdTrading, CmdExplore, CmdFind
using CmdPos

using Combos, SeekingAlpha, Joe ; c = Combos ; sa = SeekingAlpha ; j = Joe

dbChecks();
procOrders();
display(todo());
x3(1)
xdr(1)
