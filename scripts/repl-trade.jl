include("repl-simple.jl")

using Calendars, Markets, Chains, Expirations
using Store, StoreTrade, ProcOrder
using CmdTrading, CmdPos

using Combos, SeekingAlpha, Joe ; c = Combos ; sa = SeekingAlpha ; j = Joe

dbChecks()
procOrders()
display(todo())
toc()
todup()
x3(1)
xdr(5)

c.findRoll("F", 14, .12, Style.put)

# jorn(1:2)
# jorn(2:3; all=true)
# i = 2 ; r = j.ress[i][1] ; xdr(i, r.lms)

# j.runlc(5; maxSpreads=100, maxIter=1000)
