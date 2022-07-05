module Bins

r(x::Float64) = round(x; sigdigits=8)

# min w b1@min w b2 ... - b199 w b200@max w max so binwidth is (max-min) / (numBins - 1). The -1 is because there is half a width left of min and right of max.
# because each binX represents the middle of the bin
# so bin[1] is theoretically everything left of min - (binWidth/2)
# The first and last bin represent the "everything beyond this", so numVals == numBins+2'

# const NB = 601 # needs to be odd so we have a center
# const BIN_MIN = .925
# const BIN_MAX = 1.075
# const BIN_SPAN = .15
# const BIN_WIDTH = .00025
# const BINS_XS = # WRONG: collect(.92475:.00025:1.07525)

const NUM = 601 # needs to be odd so we have a center
const SPAN = 0.3

const VNUM = NUM + 2
const WIDTH = r(SPAN / (NUM - 1))         ; @assert WIDTH == 0.0005
const MFIRST = r(1.0 - 0.5 * SPAN)        ; @assert MFIRST == 0.85 # 0.9
const MLAST = r(1.0 + 0.5 * SPAN)         ; @assert MLAST == 1.15 # 1.1
const XLEFT = r(MFIRST - 0.5 * WIDTH)     ; @assert XLEFT == 0.84975 # 0.89975 "XLEFT not match $(XLEFT)"
const XRIGHT = r(MLAST + 0.5 * WIDTH)     ; @assert XRIGHT == 1.15025 # 1.10025
const MIDS = collect(MFIRST:WIDTH:MLAST)  ; @assert length(MIDS) == NUM "length(MIDS) == NUM $(length(MIDS)) != $(NUM)"
const XS = vcat(XLEFT, MIDS, XRIGHT)      ; @assert length(XS) == NUM+2
const XSI = collect(zip(1:VNUM, XS))
const MIDSI = collect(zip(2:(NUM+1), MIDS))

const IND_MID = 1 + 1 + div(NUM, 2) # 1 for left bin, and 1 for rounding up for odd number of bins
x(i::Int)::Float64 = XS[i]
xs() = XS
xsi() = XSI
midsi() = MIDSI
# mids() = MIDS
width() = WIDTH
center() = IND_MID # div(VNUM, 2)
inds() = 1:VNUM
binds() = 2:(VNUM-1)
halfRight() = center():VNUM
halfLeft() = 1:center()
shiftis(off::Int) = off < 0 ? (1:(VNUM + 2*off)) : (2*off):VNUM

# Averaging values further out causes problems when combining rets, so I removed that. Can widen bins if need more span.
valLeft(f, ctx) = f(ctx, XLEFT)
valRight(f, ctx) = f(ctx, XRIGHT)

nearest(x::Float64)::Int = isLeft(x) ? 1 : isRight(x) ? VNUM : 2 + round(Int, (x - XLEFT) / WIDTH)

# TODO: fix these to be exact because estAt is going to use them
leftOf(x::Float64)::Int = isRight(x) ? VNUM : max(1, 2 + floor(Int, (x - MFIRST) / WIDTH))
rightOf(x::Float64)::Int = isLeft(x) ? 1 : min(VNUM, -1 + VNUM - floor(Int, (MLAST - x) / WIDTH))

isLeft(x::Float64) = x <= XLEFT
isRight(x::Float64) = x >= XRIGHT

with(x::Float64)::Vector{Float64} = fill(x, VNUM)
with(f::Function)::Vector{Float64} = map(f, xs())
empty() = Vector{Float64}(undef, VNUM)
isValidInd(i::Int)::Bool = 1 <= i <= VNUM

end