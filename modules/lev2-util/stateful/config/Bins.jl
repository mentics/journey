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

const NUM = 501 # needs to be odd so we have a center
const SPAN = 0.5

const VNUM = NUM + 2
const WIDTH = r(SPAN / (NUM - 1))         ; @assert WIDTH == 0.001
const MFIRST = r(1.0 - 0.5 * SPAN)        ; @assert MFIRST == 0.75 # 0.85 # 0.9
const MLAST = r(1.0 + 0.5 * SPAN)         ; @assert MLAST == 1.25 # 1.15 # 1.1
const WIDTH_HALF = WIDTH / 2              ; @assert WIDTH_HALF == 0.0005
const XLEFT = r(MFIRST - WIDTH_HALF)      ; @assert XLEFT == 0.7495 # 0.84975 # 0.89975 "XLEFT not match $(XLEFT)"
const XRIGHT = r(MLAST + WIDTH_HALF)      ; @assert XRIGHT == 1.2505 # 1.10025
const MIDS = collect(MFIRST:WIDTH:MLAST)  ; @assert length(MIDS) == NUM "length(MIDS) == NUM $(length(MIDS)) != $(NUM)"
const XS = vcat(XLEFT, MIDS, XRIGHT)      ; @assert length(XS) == NUM+2
const XSI = collect(zip(1:VNUM, XS))
const MIDSI = collect(zip(2:(NUM+1), MIDS))

const IND_MID = 1 + 1 + div(NUM, 2) # 1 for left bin, and 1 for rounding up for odd number of bins
x(i::Int)::Float64 = XS[i]
xs() = XS
xs(::Nothing) = XS
# xs(extentHalf::Float64) = xs()[nearest(1.0 - extentHalf):nearest(1.0 + extentHalf)]
xsi() = XSI
midsi() = MIDSI
# mids() = MIDS
width() = WIDTH
center() = IND_MID # div(VNUM, 2)
left() = XLEFT
right() = XRIGHT
inds() = 1:VNUM
binds() = 2:(VNUM-1)
# halfRight() = center():VNUM
# halfLeft() = 1:center()
# shiftis(off::Int) = off < 0 ? (1:(VNUM + 2*off)) : (2*off):VNUM

binPercent() = 1.0 / VNUM # weird, just used in probkde

# Averaging values further out causes problems when combining rets, so I removed that. Can widen bins if need more span.
valLeft(f, ctx) = f(ctx, XLEFT)
valRight(f, ctx) = f(ctx, XRIGHT)

nearest(x::Float64)::Int = isLeft(x) ? 1 : isRight(x) ? VNUM : 2 + round(Int, (x - XS[2]) / WIDTH)
function testNearest()
    for i in eachindex(XS)[2:end-1]
        @assert nearest(XS[i]) == i string(i, ' ', XS[i])
        @assert nearest(XS[i] + WIDTH/4) == i string(i, ' ', XS[i] + WIDTH/4)
    end
    for i in 1:10000
        x = rand(XS[2:end-2]) + rand() - 0.5
        x = clamp(x, XS[2], XS[end-1])
        n = nearest(x)
        c1 = abs(x - XS[n-1])
        c2 = abs(x - XS[n])
        c3 = abs(x - XS[n+1])
        @assert c2 < c1 && c2 < c3 string(i, ' ', x, ' ', n, ' ', c1, ' ', c2, ' ', c3)
    end
end

# TODO: fix these to be exact because estAt is going to use them
leftOf(x::Float64)::Int = isRight(x) ? VNUM : max(1, 2 + floor(Int, (x - MFIRST) / WIDTH))
rightOf(x::Float64)::Int = isLeft(x) ? 1 : min(VNUM, -1 + VNUM - floor(Int, (MLAST - x) / WIDTH))

isLeft(x::Real) = x <= XLEFT
isRight(x::Real) = x >= XRIGHT

# function ratio(x::Float64)
#     !isLeft(x) || return (;leftI=0, rightI=1, leftX=0.0, rightX=XS[1], ratioLeft=x/XS[1], ratioRight=1.0-x/XS[1])
#     !isRight(x) || return (;leftI=VNUM, rightI=Inf, leftX=XS[end], rightX=Inf, ratioLeft=1.0-XS[end]/x, ratioRight=XS[end]/x)
#     leftI = leftOf(x)
#     rightI = leftI + 1
#     leftX = XS[leftI]
#     rightX = XS[rightI]
#     width = rightX - leftX
#     ratioLeft = (rightX - x) / width
#     ratioRight = 1.0 - ratioLeft
#     return (;leftI, rightI, leftX, rightX, ratioLeft, ratioRight)
# end
function ratio(x::Float64)
    !isLeft(x) || return (;ind=1, leftX=0.0, rightX=XS[1], ratioLeft=x/XS[1], ratioRight=1.0-x/XS[1])
    !isRight(x) || return (;ind=VNUM, leftX=XS[end], rightX=Inf, ratioLeft=1.0-XS[end]/x, ratioRight=XS[end]/x)
    ind = nearest(x)
    leftX = XS[ind] - WIDTH_HALF
    rightX = XS[ind] + WIDTH_HALF
    width = rightX - leftX
    ratioLeft = (x - leftX) / width
    ratioRight = 1.0 - ratioLeft
    # @show x ind leftX rightX width ratioLeft ratioRight
    @assert 0.0 <= ratioLeft <= 1.0 string(0.0, " <= ", ratioLeft, " <= ", 1.0)
    @assert 0.0 <= ratioRight <= 1.0 string(0.0, " <= ", ratioRight, " <= ", 1.0)
    return (;ind, leftX, rightX, ratioLeft, ratioRight)
end
# @assert ratio(XS[302]) == (ind = 302, leftX = 1.0, rightX = 1.0005, ratioLeft = 1.0, ratioRight = 0.0)
# function valAt(vals, x::Float64)
#     (;leftI, rightI, ratioLeft, ratioRight) = ratio(x)
#     return vals[leftI] * ratioLeft + vals[rightI] * ratioRight
# end

with(x::Float64)::Vector{Float64} = fill(x, VNUM)
with(f::Function)::Vector{Float64} = map(f, xs())
empty() = Vector{Float64}(undef, VNUM)
isValidInd(i::Int)::Bool = 1 <= i <= VNUM

function iterBins(f)
    f(1, 0.0, XS[1])
    for i in 2:VNUM-1
        mid = XS[i]
        f(i, mid - WIDTH_HALF, mid + WIDTH_HALF)
    end
    f(VNUM, XS[end], Inf)
end

end