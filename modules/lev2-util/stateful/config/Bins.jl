module Bins

export numVals, numBins, binXs, binX, binWidth, binMin, binMax, binItr, binsMid
export leftVal, rightVal
export binsLeft, binsRight, binNearest
export binLeftOf, binRightOf

# min w b1@min w b2 ... - b199 w b200@max w max so binwidth is (max-min) / (numBins - 1). The -1 is because there is half a width left of min and right of max.
# because each binX represents the middle of the bin
# so bin[1] is theoretically everything left of min-binWidth
# The first and last bin represent the "everything beyond this", so numVals == numBins+2'

# const NB = 401 # needs to be odd so we have a center
# const BIN_MIN = .95
# const BIN_MAX = 1.05
# const BIN_SPAN = .1
# const BIN_WIDTH = .00025
# const BIN_XS = collect(.94975:.00025:1.05025)

# binXs() = collect(.94975:.00025:1.05025)

const NB = 601 # needs to be odd so we have a center
const BIN_MIN = .925
const BIN_MAX = 1.075
const BIN_SPAN = .15
const BIN_WIDTH = .00025
const BINS_XS = collect(.92475:.00025:1.07525)

@assert (BIN_MAX - BIN_MIN) ≈ BIN_SPAN
@assert BIN_WIDTH ≈ BIN_SPAN / (NB-1)

const BINS_ITR = 2:(NB+1)
const BINS_MID = 1 + 1 + div(NB, 2) # 1 for left bin, and 1 for rounding up for odd number of bins
numVals() = NB+2
numBins() = NB
binXs() = BINS_XS
binWidth() = BIN_WIDTH
binMin() = BIN_MIN
binMax() = BIN_MAX
binItr() = BINS_ITR
binsMid() = BINS_MID
binSpan() = BIN_SPAN

# NOTE: I think this should match with binXs(), but I'm not sure if we need special handing for first and last: should they match binsLeft/binsRight?
binX(i::Int) = binXs()[i] #binMin() + (i-1)*binWidth()

# Averaging values further out causes problems when combining rets, so I removed that. Can widen bins if need more span.
# leftVal(f, ctx) = (f(ctx, BIN_MIN - binWidth()) + f(ctx, BIN_MIN - 0.5 * binSpan()) + f(ctx, BIN_MIN - 1.0 * binSpan())) / 3.0
# rightVal(f, ctx) = (f(ctx, BIN_MAX + binWidth()) + f(ctx, BIN_MAX + 0.5 * binSpan()) + f(ctx, BIN_MAX + 1.0 * binSpan())) / 3.0
leftVal(f, ctx) = f(ctx, BIN_MIN - binWidth())
rightVal(f, ctx) = f(ctx, BIN_MAX + binWidth())

binsLeft() = binMin() - binWidthHalf()
binsRight() = binMax() + binWidthHalf()

binNearest(x::Float64)::Int = isBinLeft(x) ? 1 : isBinRight(x) ? numVals() : 2 + round(Int, (x - binMin()) / binWidth())

# TODO: fix these to be exact because estAt is going to use them
binLeftOf(x::Float64)::Int = max(1, 1 + floor(Int, (x - binX(1)) / binWidth()))
binRightOf(x::Float64)::Int = min(numVals(), numVals() - floor(Int, (binX(numVals()) - x) / binWidth()))
# 1 + floor(Int, (x - binMin()) / binWidth()) + 2

binWidthHalf() = 0.5 * binWidth()
isBinLeft(x::Float64) = x <= binsLeft()
isBinRight(x::Float64) = x >= binsRight()

end