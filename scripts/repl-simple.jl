include("repl-base.jl")

using Dates
# import IterTools, NamedTupleTools
# IT = IterTools
# NTT = NamedTupleTools
using SH, BaseTypes, Globals, SmallTypes, LegTypes, LegMetaTypes
using OutputUtil, TypeOutputs

import ModuleUtil
if ModuleUtil.findCycles()
    error("Startup aborted due to module cycles detected")
end