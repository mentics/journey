include("repl-base.jl")

using Dates
# , StaticArrays
# import IterTools, NamedTupleTools
# IT = IterTools
# NTT = NamedTupleTools
# using SH, BaseTypes, Globals, SmallTypes, LegTypes, LegQuoteTypes, TypeOutputs
# using OutputUtil

import ModuleUtil
if ModuleUtil.findCycles()
    error("Startup aborted due to module cycles detected")
end