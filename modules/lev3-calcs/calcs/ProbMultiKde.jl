module ProbMultiKde
using Dates, Intervals
using BaseTypes, SmallTypes, BaseUtil
import SH, OptionUtil
import Calendars
import DataFiles as dat
import VectorCalcUtil as vcu

export GridKde, KdeProb

#region Types
const IntervalFCC = Interval{Float64,Closed,Closed}

struct Coord{T}
    ret::T
    tex::T
    vol::T
end
const CoordF = Coord{Float64}

struct Xs
    xs::Vector{Float64}
    # TODO: remove numbins: redundant with length(xs)
    numbins::Int
    binwidth::Float64
end

struct Transform{T}
    ad::T
    scale::T
end
const TransformC = Transform{CoordF}

struct GridKde{T}
    gridcount::Int
    m::Array{Vector{T}, 3}
    txs::Transform
    width::Float64
    extents::Coord{IntervalFCC}
    xs::Ref{Xs}
end
# const Kde3C = GridKde{CoordF}

struct KdeProb{S}
    src::S
    ts::DateTime
    center::Float64
    prob_mass::Vector{Float64}
    cumu_mass::Vector{Float64}
    xs::Xs
end
#endregion

#region Public
function make_kde(ts::DateTime; len=200000, gridcount=20, area=4)
    global kerr = nothing
    tsx = dat.tsx_for_prob(ts; len)
    println("Making kde for ts:$(ts) using $(size(tsx,1)) events from $(tsx.ts[1]) to $(tsx.ts[end])")

    m = [Vector{CoordF}() for _ in 1:gridcount, _ in 1:gridcount, _ in 1:gridcount]

    data = (tsx.ret, tsx.tex, tsx.vol)
    extents = Coord(map(d -> IntervalFCC(extrema(d)...), data)...)
    tfs = VectorCalcUtil.fit_01.(data)
    global ktfs = tfs

    tx = Transform(map(tf -> tf.ad, tfs), map(tf -> tf.scale, tfs))

    width = 1.0 / (gridcount - 1)
    try
        gridcs = [togrid(tfs[i].v, width) for i in eachindex(tfs)]

        for i in eachindex(tfs[1].v)
            coord = CoordF(tfs[1].v[i], tfs[2].v[i], tfs[3].v[i])
            for x in toarea(gridcs[1][i], area, gridcount), y in toarea(gridcs[2][i], area, gridcount), z in toarea(gridcs[3][i], area, gridcount)
                push!(m[x, y, z], coord)
            end
        end
        return GridKde(gridcount, m, tx, width, extents, Ref(Xs([], 0, 0.0)))
    catch e
        global kerr = data
        @show ts
        rethrow(e)
    end
end

# function makeprob(kde, curp, ts, xpir; kws...)
#     return makeprob(kde, mkt.curp, mkt.tsMarket, dat.market_close(xpir); kws...)
# end
# makeprob(tkde, curp, xpir::Date) = makeprob(tkde, curp, dat.market_close(xpir))
# binwidth is hardcoded to 10th of a percent
function makeprob(kde, curp, ts::DateTime, xpirts::DateTime, oqs; binwidth=1e-3, k=0.001)
    (;tex, vol) = make_kde_args(curp, ts, xpirts, oqs)
    if !(tex in kde.extents.tex)
        println("WARN: tex:$(tex) not in kde extents $(kde.extents.tex)")
    end
    if !(vol in kde.extents.vol)
        println("WARN: vol:$(vol) not in kde extents $(kde.extents.vol)")
    end

    xs = Vector{Float64}()
    pdfvals = Vector{Float64}()
    ret_min, ret_max = extrem(kde.extents.ret)
    start = round_step(binwidth, 0.9 * ret_min)
    stop = round_step(binwidth, 1.1 * ret_max)
    # Can't search for left and right beforehand because we're going to normalize it, so we don't know what threshold to use.
    # xleft, pdleft = CollUtil.find2(x -> kde_pdf(kde, (x, tex, vol), k), y -> y > 1e-8, start:binwidth:ret_max)
    # xright, pdright = CollUtil.find2(x -> kde_pdf(kde, (x, tex, vol), k), y -> y > 1e-8, stop:-binwidth:start)
    xs = collect(start:binwidth:stop)
    pdfvals = map(xs) do x
        return kde_pdf(kde, (x, tex, vol), k, allow_oob=true)
    end
    # global kxs = xs
    # global kpdfvals = pdfvals
    VectorCalcUtil.normalize!(pdfvals)
    left = findfirst(x -> x > 1e-6, pdfvals)
    right = findlast(x -> x > 1e-6, pdfvals)
    inds = left:right
    xs = xs[inds]
    pdfvals = pdfvals[inds]
    VectorCalcUtil.normalize!(pdfvals) # not sure if this second normalize! is ideal
    cdfvals = accumulate(+, pdfvals)
    return KdeProb(kde, ts, F(curp), pdfvals, cdfvals, Xs(xs,-1,binwidth))
    # sxs = kde_xs(kde, numbins)
    # xs = sxs.xs

    # pdfvals = Vector{Float64}(undef, numbins)
    # for i in 1:numbins
    #     coord = (xs[i], tex, vol)
    #     pdfvals[i] = kde_pdf(kde, coord, k)
    # end
    # VectorCalcUtil.normalize!(pdfvals)
    # cdfvals = accumulate(+, pdfvals)
    # return KdeProb(kde, ts, F(curp), pdfvals, cdfvals, sxs)
end

apply(x, f1, f2) = (f1(x), f2(x))
function cdf(prob::KdeProb, x::Float64)::Float64
    xr = x / prob.center
    # mn, mx = apply(getxextent(prob), first, last)
    mn, mx = apply(prob.xs.xs, first, last)
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
    rightpart = rightratio * prob.cumu_mass[indright]
    leftpart = (1.0 - rightratio) * prob.cumu_mass[indleft]
    res = leftpart + rightpart
    @assert res >= 0.0 "assertion failed, prob cdf >= 0: $(res)"
    res > 1e-10 || return 0.0
    res < (1.0 - 1e-10) || return 1.0
    return res
end

# function pdf_trim(prob::KdeProb, left, right)
#     left = max(left, first(prob.src.extents.ret))
#     right = min(right, last(prob.src.extents.ret))
#     right > left || return NaN, NaN

# end
#endregion Public

#region Local
getxs(prob::KdeProb) = prob.xs.xs
# getxextent(prob::KdeProb) = prob.src.extents.ret
getbinwidth(prob::KdeProb) = prob.xs.binwidth

@inline togrid(x, width) = 1 .+ round.(Int, x ./ width)
@inline togrid(x, width, gridcount) = clamp.(1 .+ round.(Int, x ./ width), 1, gridcount)
@inline toarea(mid::Integer, width::Integer, gridcount::Integer) = (max(1, mid - width)):(min(gridcount, mid + width))

# function pdf_ind(prob::KdeProb, x)
#     xr = x / prob.center
#     ind = max(0, floor(Int, 1 + (xr - mn) / binwidth))
#     return ind
# end
# function pdf_fast(prob::KdeProb, x)
#     xr = x / prob.center
#     ind = max(0, floor(Int, 1 + (xr - mn) / binwidth))
#     return prob.pdf[ind]
# end

# import Calendars, OptionUtil
# function makekdeargs(xpirts::DateTime)
#     mkt = market()
#     curp = mkt.curp
#     ts = mkt.tsMarket
#     xpirts = dat.market_close(xpir)
#     makekdeargs(curp, ts, xpirts)
# end
# function make_kde_args(curp, ts::DateTime, xpirts::DateTime, oqs_calls, oqs_puts) = ...
function make_kde_args(curp, ts::DateTime, xpirts::DateTime, oqs)
    # chain = Chains.chain(Date(xpirts)).chain
    tex = Calendars.calcTex(ts, xpirts)
    vol = dat.calc_vol(curp, tex, oqs, ts)
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

# TODO: I should review all the bin finding code as far as what does a bin/boundary mean
# function findbin(mn, binwidth, x)
#     ind = max(0, floor(Int, 1 + (x - mn) / binwidth))
#     return ind
# end

# function kde_xs(kde::GridKde, numbins)
#     if kde.xs[].numbins != numbins
#         kde.xs[] = calcxs(kde, numbins)
#     end
#     return kde.xs[]
#     # return Xs2(kde.xs[].xs, numbins, kde.xs[].binwidth)
# end

# function calcxs(kde::GridKde, numbins)
#     mn, mx = extrem(kde.extents.ret)
#     binwidth = (mx - mn) / (numbins - 1)
#     xs = collect(map(i -> mn + i * binwidth, 0:(numbins-1)))
#     return Xs2(xs, numbins, binwidth)
# end

extrem(il::IntervalFCC) = return first(il), last(il)

function kde_pdf(kde::GridKde, x, k; allow_oob=false)
    try
        xin = txin(kde, x) # (x .+ kde.txs.ad) .* kde.txs.scale
        if allow_oob
            coord = togrid.(xin, kde.width, kde.gridcount)
        else
            coord = togrid.(xin, kde.width)
        end
        obs = kde.m[coord...]
        dens = kernel(obs, xin, k)
        if isnan(dens)
            @show x k xin dens
            error("NaN for dens")
        end
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