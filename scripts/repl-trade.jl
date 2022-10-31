include("repl-simple.jl")

using Calendars, Markets, Chains, Expirations
using Store, StoreTrade, ProcOrder
using CmdTrading, CmdPos

using Combos, SeekingAlpha, Joe ; c = Combos ; sa = SeekingAlpha ; j = Joe

dbChecks();
procOrders();
toc();
display(todo());
x3(1);
xdr(1);
jorn(1:2; all=true);
