module ProbMultiKde
using Dates, Intervals
using BaseTypes, SmallTypes
import SH, OptionUtil
import Calendars
import DataFiles as dat
import VectorCalcUtil

export GridKde, KdeProb

#region Types
const IntervalFCC = Interval{Float64,Closed,Closed}

struct Coord{T}
    ret::T
    tex::T
    vol::T
end
const CoordF = Coord{Float64}

struct Xs2
    xs::Vector{Float64}
    numbins::Int
    binwidth::Float64
end

struct Transform{T}
    ad::T
    scale::T
end
const TransformC = Transform{CoordF}

struct GridKde{T}
    m::Array{Vector{T}, 3}
    txs::Transform
    width::Float64
    extents::Coord{IntervalFCC}
    xs::Ref{Xs2}
end
const Kde3C = GridKde{CoordF}

struct KdeProb{S}
    src::S
    ts::DateTime
    center::Float64
    pdf::Vector{Float64}
    cdf::Vector{Float64}
    xs::Xs2
end
#endregion

#region Public
function make_kde(ts::DateTime; len=10000, gridcount=20, area=4)
    tsx = dat.tsx_for_prob(ts::DateTime; len)

    m = [Vector{CoordF}() for _ in 1:gridcount, _ in 1:gridcount, _ in 1:gridcount]

    data = (tsx.ret, tsx.tex, tsx.vol)
    extents = Coord(map(d -> IntervalFCC(extrema(d)...), data)...)
    tfs = VectorCalcUtil.fit_01.(data)

    tx = Transform(map(tf -> tf.ad, tfs), map(tf -> tf.scale, tfs))

    width = 1.0 / (gridcount - 1)
    gridcs = [togrid(tfs[i].v, width) for i in eachindex(tfs)]

    for i in eachindex(tfs[1].v)
        coord = CoordF(tfs[1].v[i], tfs[2].v[i], tfs[3].v[i])
        for x in toarea(gridcs[1][i], area, gridcount), y in toarea(gridcs[2][i], area, gridcount), z in toarea(gridcs[3][i], area, gridcount)
            push!(m[x, y, z], coord)
        end
    end
    return GridKde(m, tx, width, extents, Ref(Xs2([], 0, 0.0)))
end

# function makeprob(kde, curp, ts, xpir; kws...)
#     return makeprob(kde, mkt.curp, mkt.tsMarket, Calendars.getMarketClose(xpir); kws...)
# end
# makeprob(tkde, curp, xpir::Date) = makeprob(tkde, curp, Calendars.getMarketClose(xpir))
function makeprob(kde, curp, ts::DateTime, xpirts::DateTime, oqs; numbins=600, k=0.001)
    (;tex, vol) = make_kde_args(curp, ts, xpirts, oqs)
    if !(tex in kde.extents.tex)
        println("WARN: tex:$(tex) not in kde extents $(kde.extents.tex)")
    end
    if !(vol in kde.extents.vol)
        println("WARN: vol:$(vol) not in kde extents $(kde.extents.vol)")
    end
    sxs = kde_xs(kde, numbins)
    xs = sxs.xs

    pdfvals = Vector{Float64}(undef, numbins)
    for i in 1:numbins
        coord = (xs[i], tex, vol)
        pdfvals[i] = kde_pdf(kde, coord, k)
    end
    VectorCalcUtil.normalize!(pdfvals)
    cdfvals = accumulate(+, pdfvals)
    return KdeProb(kde, ts, F(curp), pdfvals, cdfvals, sxs)
end

apply(x, f1, f2) = (f1(x), f2(x))
function cdf(prob::KdeProb, x::Float64)::Float64
    xr = x / prob.center
    mn, mx = apply(getxextent(prob), first, last)
    xr > mn || return 0.0
    xr < mx || return 1.0
    xs = getxs(prob)
    binwidth = getbinwidth(prob)
    indright = floor(Int, 2 + (xr - mn) / binwidth) # this was faster: 19.7 ns vs. 25.4 ns
    indleft = indright - 1
    indleft > 0 || error("cdf: indleft > 0")
    xin = xr
    xleft = xs[indleft]
    xright = xs[indright]
    @assert xleft <= xin <= xright string((;xleft, xin, xright))
    @assert xs[indleft] <= xr <= xs[indright] string((;xsleft=xs[indleft], xr, xsright=xs[indright]))
    rightratio = (xin - xleft) / (xright - xleft)
    rightpart = rightratio * prob.cdf[indright]
    leftpart = (1.0 - rightratio) * prob.cdf[indleft]
    res = leftpart + rightpart
    @assert res >= 0.0 "assertion failed, prob cdf >= 0: $(res)"
    res > 1e-10 || return 0.0
    res < (1.0 - 1e-10) || return 1.0
    return res
end
#endregion Public

#region Local
getxs(prob::KdeProb) = prob.xs.xs
getxextent(prob::KdeProb) = prob.src.extents.ret
getbinwidth(prob::KdeProb) = prob.xs.binwidth

@inline togrid(x, width) = 1 .+ round.(Int, x ./ width)
@inline toarea(mid::Integer, width::Integer, gridcount::Integer) = (max(1, mid - width)):(min(gridcount, mid + width))

# import Calendars, OptionUtil
# function makekdeargs(xpirts::DateTime)
#     mkt = market()
#     curp = mkt.curp
#     ts = mkt.tsMarket
#     xpirts = Calendars.getMarketClose(xpir)
#     makekdeargs(curp, ts, xpirts)
# end
# function make_kde_args(curp, ts::DateTime, xpirts::DateTime, oqs_calls, oqs_puts) = ...
function make_kde_args(curp, ts::DateTime, xpirts::DateTime, oqs)
    # chain = Chains.chain(Date(xpirts)).chain
    tex = Calendars.calcTex(ts, xpirts)
    vol = dat.calc_vol(curp, tex, oqs)
    return (;tex, vol)
end

# function ntm4s(ts, xpir, curp, oqs, style)
#     f_extrin = style == Style.call ? OptionUtil.extrin_call : OptionUtil.extrin_put
#     # TODO: could use that heap thing to do this more efficiently nextrem ?
#     sort!(oqs; by=oq -> abs(curp - SH.getStrike(oq)))
#     oq4 = oqs[1:4]
#     fntm = dat.make_ntm_data(f_extrin, "", style)
#     return fntm((ts,), (xpir,), curp, SH.getStrike.(oq4), SH.getBid.(oq4), SH.getAsk.(oq4))
# end

function kde_xs(kde::GridKde, numbins)
    if kde.xs[].numbins != numbins
        kde.xs[] = calcxs(kde, numbins)
    end
    return kde.xs[]
    # return Xs2(kde.xs[].xs, numbins, kde.xs[].binwidth)
end

function calcxs(kde::GridKde, numbins)
    mn, mx = extrem(kde.extents.ret)
    binwidth = (mx - mn) / (numbins - 1)
    xs = collect(map(i -> mn + i * binwidth, 0:(numbins-1)))
    return Xs2(xs, numbins, binwidth)
end

extrem(il::IntervalFCC) = return first(il), last(il)

function kde_pdf(kde::GridKde, x, k)
    # @show x
    try
        xin = txin(kde, x) # (x .+ kde.txs.ad) .* kde.txs.scale
        coord = togrid.(xin, kde.width)
        # @show coord
        obs = kde.m[coord...]
        dens = kernel(obs, xin, k)
        return dens
    catch e
        global kerr = (;kde, x, k)
        rethrow(e)
    end
end

txin(kde::GridKde, x) = (x .+ kde.txs.ad) .* kde.txs.scale
txout(kde::GridKde, x) = (x ./ kde.txs.scale) .- kde.txs.ad

function kernel(obs, x, k)
    # e^(-(obs - x)^2/k)
    # TODO: multikde uses prod() instead of sum?
    return sum(map(obs) do o
        exp.(-subdot2(o, x) / k)
    end; init=0.0)
    # return sum(exp.(-dot2.(tsub.(obs, x)) / k))
end

# @inline Base.broadcasted(::typeof(-), c::CoordF, x::Vector{Float64}) = CoordF(c.ret)
# @inline dot2(v::CoordF) = return v.ret^2 + v.tex^2 + v.vol^2
@inline subdot2(c::CoordF, x::NTuple{3,Float64}) = (c.ret - x[1])^2 + (c.tex - x[2])^2 + (c.vol - x[3])^2
#endregion Local

end