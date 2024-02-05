include("repl-base.jl")

# using DrawUtil
# using Calendars, Markets, Chains, Expirations
# using Between, Store, StoreTrade, ProcOrder, TradeInfo, Pricing

# using DrawUtil, Explore, DataFiles, Trading
# import DataFiles as dat
# import ProbMultiKde as pmk
# import VectorCalcUtil as vcu
# import DataFilesExplore as dfe

# # using DataFrames

# import Flux:gpu
# import ProbML as ml

# # ml.setup()

using Dates, DataFrames, MLUtils

using DateUtil
import Calendars as cal

using DataRead, DataUpdate, Paths, FilesArrow, FilesJLD2

import MLTrain as ml
import OhlcShapeData as osd
import OhlcShapeModel as osm
import ReturnProbData as rpd
import ReturnProbModel as rpm
trainee = rpm.make_trainee(); training = ml.setup(trainee);
ml.train(training; epochs=1)

# using ModelUtil
# import DataFilesExplore as dfe
