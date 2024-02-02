module ProbMeta
export Bins

struct BinsInfo
    NUM
    SPAN

    VNUM
    WIDTH
    MFIRST
    MLAST
    WIDTH_HALF
    XLEFT
    XRIGHT
    MIDS
    XS
    XSI
    MIDSI
end

function make_bins(num, span)
    NUM = num
    SPAN = span

    VNUM = NUM + 2
    WIDTH = r(SPAN / (NUM - 1))
    MFIRST = r(1.0 - 0.5 * SPAN)
    MLAST = r(1.0 + 0.5 * SPAN)
    WIDTH_HALF = WIDTH / 2
    XLEFT = r(MFIRST - WIDTH_HALF)
    XRIGHT = r(MLAST + WIDTH_HALF)
    MIDS = collect(MFIRST:WIDTH:MLAST)
    XS = vcat(XLEFT, MIDS, XRIGHT)
    XSI = collect(zip(1:VNUM, XS))
    MIDSI = collect(zip(2:(NUM+1), MIDS))
    BinsInfo(NUM, SPAN, VNUM, WIDTH, MFIRST, MLAST, WIDTH_HALF, XLEFT, XRIGHT, MIDS, XS, XSI, MIDSI)
end

const BINS2 = Ref{BinsInfo}()
function __init__()
    init_bins()
end
init_bins(nbins=201, span=0.1) = BINS2[] = make_bins(nbins, span)

num_bins() = BINS2[].NUM
num_edges() = BINS2[].VNUM
xs() = BINS2[].XS

r(x::Float64) = round(x; sigdigits=8)

end