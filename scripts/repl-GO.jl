include("repl-simple.jl")

using DrawUtil
using Calendars, Markets, Chains, Expirations
using Between, Store, StoreTrade, ProcOrder, TradeInfo, Pricing

using DrawUtil, Explore, DataFiles, Trading
import DataFiles as dat
import ProbMultiKde as pmk
import VectorCalcUtil as vcu
import DataFilesExplore as dfe

# using DataFrames

import Flux:gpu
import MLRun as ml
import HistoricalModel as hm

# ml.setup()