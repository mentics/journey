include("repl-base.jl")

using Dates
# import IterTools, NamedTupleTools
# iter = IterTools
# ntt = NamedTupleTools
using BaseTypes, OutputUtil, CudaUtil

using Flux, Transformers
import MLExplore as mlx

import SimpleStore as SS
import MarketHist
import Trade4Data as T4

SS.loadTss()
SS.make_otoqs()

MarketHist.populate_curps()
MarketHist.populate_vixs()
;