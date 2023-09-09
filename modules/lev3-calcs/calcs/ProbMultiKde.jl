module ProbMultiKde
using Dates, Intervals, Memoization, ThreadPools
using BaseTypes, SmallTypes, BaseUtil
import SH, OptionUtil
import Calendars
import DataFiles as dat
import VectorCalcUtil as vcu

export SimpleKde, KdeProb

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
const TheKde = GridKde{CoordF}

struct SimpleKde
    obs::Vector{CoordF}
    txs::Transform
    extents::Coord{IntervalFCC}
end

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
const KDE_CACHE5 = Dict{Date,SimpleKde}()
get_kde(ts::TimeType) = get_kde(year(ts), month(ts))
function get_kde(y, m)
    date = Date(y, m)
    return get!(KDE_CACHE5, date) do
        make_kde(DateTime(date))
    end
end

import DataStructures as DS
import StatsBase as SB

function make_kde(ts::DateTime)
    len::Int=200000
    tsx = dat.tsx_for_prob(ts, len)
    println("Making simple kde for ts:$(ts) using $(size(tsx,1)) events from $(tsx.ts[1]) to $(tsx.ts[end])")

    data::NTuple{3,Vector{Float64}} = (tsx.ret, tsx.tex, tsx.vol)
    extents = Coord(map(d -> IntervalFCC(extrema(d)...), data)...)
    tfs::NTuple{3,NamedTuple} = vcu.fit_01.(data)
    tx = Transform(map(tf -> tf.ad, tfs), map(tf -> tf.scale, tfs))
    coords = [CoordF(tfs[1].v[i], tfs[2].v[i], tfs[3].v[i]) for i in eachindex(tfs[1].v)]
    return SimpleKde(coords, tx, extents)
end

function vec_sized(T, size)
    # v = Vector{T}(undef, size)
    # empty!(v)
    v = Vector{T}()
    sizehint!(v, size)
    return v
end

# const INDICES20_2 = collect(map(x -> x.I, CartesianIndices((20,20,20))))
const INDICES3 = Vector{Array{NTuple{3,Int},3}}(undef, 100)
function ensure_indices(size::Int)
    if !isdefined(INDICES3, size)
        INDICES3[size] = collect(map(x -> x.I, CartesianIndices((size,size,size))))
    end
end
indices(size::Int)::Array{NTuple{3,Int},3} = INDICES3[size]

# dist_euc(c1::NTuple{3,<:Real}, c2::NTuple{3,<:Real}) = (c1[1] * c2[1])^2 + (c1[2] * c2[2])^2 + (c1[3] * c2[3])^2
dist_euc(c1::CoordF, c2::NTuple{3,<:Real}) = (c1.ret * c2[1])^2 + (c1.tex * c2[2])^2 + (c1.vol * c2[3])^2
# dist_euc(c1::CoordF, c2::NTuple{3,Float64}) = (c1.ret * c2[1])^2 + (c1.tex * c2[2])^2 + (c1.vol * c2[3])^2

# function make_kde(ts::DateTime)
#     len::Int=200000
#     per_cell::Int=20000
#     grid_size::Int=20
#     global kerr = nothing
#     grid_extent = 1..grid_size
#     width = 1.0 / grid_size
#     ensure_indices(grid_size)

#     tsx = dat.tsx_for_prob(ts, len)
#     println("Making kde for ts:$(ts) using $(size(tsx,1)) events from $(tsx.ts[1]) to $(tsx.ts[end])")

#     data::NTuple{3,Vector{Float64}} = (tsx.ret, tsx.tex, tsx.vol)
#     extents = Coord(map(d -> IntervalFCC(extrema(d)...), data)...)
#     tfs::NTuple{3,NamedTuple} = vcu.fit_01.(data)
#     tx = Transform(map(tf -> tf.ad, tfs), map(tf -> tf.scale, tfs))

#     m_index = [Vector{CoordF}() for _ in 1:grid_size, _ in 1:grid_size, _ in 1:grid_size]
#     m = [vec_sized(CoordF, per_cell) for _ in 1:grid_size, _ in 1:grid_size, _ in 1:grid_size]

#     v_shells = [vec_sized(CoordF, per_cell) for _ in 1:Threads.nthreads()]
#     scratchs = [vec_sized(CoordF, per_cell) for _ in 1:Threads.nthreads()]

#     try
#         coords = [CoordF(tfs[1].v[i], tfs[2].v[i], tfs[3].v[i]) for i in eachindex(tfs[1].v)]
#         coords_cell = map(c -> togrid(c, width), coords)
#         @assert length(coords) ==  length(coords_cell) == len
#         global kcoords = coords
#         global kcoords_cell = coords_cell

#         len_max = 0
#         for i in eachindex(coords)
#             v_cell = m_index[coords_cell[i]...]
#             push!(v_cell, coords[i])
#             len_max = max(len_max, length(v_cell))
#         end
#         @assert len_max < per_cell

#         @bthreads for cell in indices(grid_size)
#             v_cell = m[cell...]
#             thid = Threads.threadid()
#             v_shell = v_shells[thid]
#             scratch = scratchs[thid]

#             if length(v_cell) < per_cell
#                 empty!(v_shell)
#                 radius = 0
#                 while true
#                     radius += 1
#                     for cell_offset in SHELLS[radius]
#                         # coord_grid = cell_center.I .+ cell_offset
#                         # is_in(coord_grid, grid_size) || continue
#                         # append!(v_shell, m_index[coord_grid...])
#                         ret = cell[1] + cell_offset[1]
#                         ret in grid_extent || continue
#                         tex = cell[2] + cell_offset[2]
#                         tex in grid_extent || continue
#                         vol = cell[3] + cell_offset[3]
#                         vol in grid_extent || continue
#                         append!(v_shell, m_index[ret, tex, vol])
#                     end
#                     len_cell = length(v_cell)
#                     len_shell = length(v_shell)
#                     len = len_cell + len_shell
#                     if len >= per_cell
#                         take_count = per_cell - len_cell
#                         cell_coord_center = (cell .- 0.5) .* width
#                         sort!(v_shell; by=c -> dist_euc(c, cell_coord_center), scratch)
#                         for i in 1:take_count
#                             push!(v_cell, v_shell[i])
#                         end
#                         # top = first(v_shell, take_count)

#                         # above sort was a lot faster
#                         # top = DS.nlargest(take_count, v_shell; by=c -> dist_euc(c, cell_coord_center))
#                         # append!(v_cell, top)
#                         break
#                     end
#                     yield()
#                 end
#             end
#             @assert length(v_cell) == per_cell
#         end

#         return GridKde(grid_size, m, tx, width, extents, Ref(Xs([], 0, 0.0)))
#     catch e
#         global kerr = data
#         @show ts
#         rethrow(e)
#     end
# end

# const SHELLS = Vector{Vector{NTuple{3,Int}}}(undef, 20)
# function setup_shells()
#     for i in 1:20
#         SHELLS[i] = shell(i, 3)
#     end
# end

# function shell(radius::Int, ndims::Int)::Vector{NTuple{3,Int}}
#     println("shell: $(radius) $(ndims)")
#     return collect(map(x -> x.I, filter(coord -> !isnothing(findfirst(i -> abs(i) == radius, coord.I)), CartesianIndices(Tuple(fill(-radius:radius, ndims))))))
# end

# is_inside_grid(coord, grid_size) = isnothing(findfirst(x -> x < 1 || x > grid_size, coord))
# function is_inside_grid(coord::CoordF, grid_size)
#     coord.ret
#     coord.tex
#     coord.vol
# end

# is_in(coord::NTuple{3,<:Integer}, len) = is_in(coord[1], len) && is_in(coord[2], len) && is_in(coord[3], len)
# is_in(x::Integer, len::Integer) = x >= 1 && x <= len
# # isnothing(findfirst(x -> x < 1 || x > grid_size, coord))

# function makeprob(kde, curp, ts, xpir; kws...)
#     return makeprob(kde, mkt.curp, mkt.tsMarket, dat.market_close(xpir); kws...)
# end
# makeprob(tkde, curp, xpir::Date) = makeprob(tkde, curp, dat.market_close(xpir))
# binwidth is hardcoded to 10th of a percent

function makeprob(kde::SimpleKde, curp, ts::DateTime, xpirts::DateTime, oqs; binwidth=1e-3, k=(0.001, 0.01, 0.01))
    # global kmakeprobargs = (kde, curp, ts, xpirts, oqs)
    (;tex, vol) = make_kde_args(curp, ts, xpirts, oqs)
    # println("makeprob: $(tex), $(vol)")
    if !(tex in kde.extents.tex)
        println("WARN: tex:$(tex) not in kde extents $(kde.extents.tex)")
    end
    if !(vol in kde.extents.vol)
        println("WARN: vol:$(vol) not in kde extents $(kde.extents.vol)")
    end

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
    # global kxs1 = copy(xs)
    # global kpdfvals1 = copy(pdfvals)
    # global kxs = xs
    # global kpdfvals = pdfvals
    mn = minimum(pdfvals)
    pdfvals .-= mn
    vcu.normalize!(pdfvals)
    left = findfirst(x -> x > 1e-5, pdfvals)
    right = findlast(x -> x > 1e-5, pdfvals)
    global kpdfvals = pdfvals
    inds = left:right
    xs = xs[inds]
    pdfvals = pdfvals[inds]
    vcu.normalize!(pdfvals) # not sure if this second normalize! is ideal
    cdfvals = accumulate(+, pdfvals)
    return KdeProb(kde, ts, F(curp), pdfvals, cdfvals, Xs(xs,-1,binwidth))
    # sxs = kde_xs(kde, numbins)
    # xs = sxs.xs

    # pdfvals = Vector{Float64}(undef, numbins)
    # for i in 1:numbins
    #     coord = (xs[i], tex, vol)
    #     pdfvals[i] = kde_pdf(kde, coord, k)
    # end
    # vcu.normalize!(pdfvals)
    # cdfvals = accumulate(+, pdfvals)
    # return KdeProb(kde, ts, F(curp), pdfvals, cdfvals, sxs)
end

# function makeprob(kde::TheGrid, curp, ts::DateTime, xpirts::DateTime, oqs; binwidth=1e-3, k=(0.001, 0.01, 0.01))
#     global kmakeprobargs = (kde, curp, ts, xpirts, oqs)
#     (;tex, vol) = make_kde_args(curp, ts, xpirts, oqs)
#     println("makeprob: $(tex), $(vol)")
#     if !(tex in kde.extents.tex)
#         println("WARN: tex:$(tex) not in kde extents $(kde.extents.tex)")
#     end
#     if !(vol in kde.extents.vol)
#         println("WARN: vol:$(vol) not in kde extents $(kde.extents.vol)")
#     end

#     pdfvals = Vector{Float64}()
#     ret_min, ret_max = extrem(kde.extents.ret)
#     start = round_step(binwidth, 0.9 * ret_min)
#     stop = round_step(binwidth, 1.1 * ret_max)
#     # Can't search for left and right beforehand because we're going to normalize it, so we don't know what threshold to use.
#     # xleft, pdleft = CollUtil.find2(x -> kde_pdf(kde, (x, tex, vol), k), y -> y > 1e-8, start:binwidth:ret_max)
#     # xright, pdright = CollUtil.find2(x -> kde_pdf(kde, (x, tex, vol), k), y -> y > 1e-8, stop:-binwidth:start)
#     xs = collect(start:binwidth:stop)
#     pdfvals = map(xs) do x
#         return kde_pdf(kde, (x, tex, vol), k, allow_oob=true)
#     end
#     global kxs1 = copy(xs)
#     global kpdfvals1 = copy(pdfvals)
#     # global kxs = xs
#     # global kpdfvals = pdfvals
#     vcu.normalize!(pdfvals)
#     left = findfirst(x -> x > 1e-6, pdfvals)
#     right = findlast(x -> x > 1e-6, pdfvals)
#     inds = left:right
#     xs = xs[inds]
#     pdfvals = pdfvals[inds]
#     vcu.normalize!(pdfvals) # not sure if this second normalize! is ideal
#     cdfvals = accumulate(+, pdfvals)
#     return KdeProb(kde, ts, F(curp), pdfvals, cdfvals, Xs(xs,-1,binwidth))
#     # sxs = kde_xs(kde, numbins)
#     # xs = sxs.xs

#     # pdfvals = Vector{Float64}(undef, numbins)
#     # for i in 1:numbins
#     #     coord = (xs[i], tex, vol)
#     #     pdfvals[i] = kde_pdf(kde, coord, k)
#     # end
#     # vcu.normalize!(pdfvals)
#     # cdfvals = accumulate(+, pdfvals)
#     # return KdeProb(kde, ts, F(curp), pdfvals, cdfvals, sxs)
# end

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

@inline function togrid(x::CoordF, width)
    @assert x.ret in 0.0..1.0 && x.tex in 0.0..1.0 && x.vol in 0.0..1.0
    c = (togrid(x.ret, width), togrid(x.tex, width), togrid(x.vol, width))
    bounds = 1..(1/width)
    @assert c[1] in bounds && c[2] in bounds && c[3] in bounds string((;x, width, c, bounds))
    return c
end
@inline togrid(x, width, gridcount) = clamp(togrid(x, width), 1, gridcount)
@inline togrid(x, width) = ( width += 1e-10 ; round(Int, (x + (width/2)) / width, RoundNearestTiesUp) )
# @inline togrid(x, width) = 1 .+ round.(Int, x ./ width)
# @inline togrid(x, width, gridcount) = clamp.(1 .+ round.(Int, x ./ width), 1, gridcount)
# @inline toarea(mid::Integer, width::Integer, gridcount::Integer) = (max(1, mid - width)):(min(gridcount, mid + width))

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

function kde_pdf(kde::SimpleKde, x, k; allow_oob=false)
    try
        xin = txin(kde, x) # (x .+ kde.txs.ad) .* kde.txs.scale
        dens = kernel(kde.obs, xin, k)
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

# function kde_pdf(kde::GridKde, x, k; allow_oob=false)
#     try
#         xin = txin(kde, x) # (x .+ kde.txs.ad) .* kde.txs.scale
#         if allow_oob
#             coord = togrid.(xin, kde.width, kde.gridcount)
#         else
#             coord = togrid.(xin, kde.width)
#         end
#         obs = kde.m[coord...]
#         dens = kernel(obs, xin, k)
#         if isnan(dens)
#             @show x k xin dens
#             error("NaN for dens")
#         end
#         return dens
#     catch e
#         global kerr = (;kde, x, k)
#         rethrow(e)
#     end
# end

txin(kde, x) = (x .+ kde.txs.ad) .* kde.txs.scale
txout(kde, x) = (x ./ kde.txs.scale) .- kde.txs.ad

# function kernelold(obs, x, k::Real)
#     # e^(-(obs - x)^2/k)
#     # TODO: multikde uses prod() instead of sum?
#     return sum(map(obs) do o
#         exp.(-subdot2(o, x) / k)
#     end; init=0.0)
#     # return sum(exp.(-dot2.(tsub.(obs, x)) / k))
# end

# # @inline Base.broadcasted(::typeof(-), c::CoordF, x::Vector{Float64}) = CoordF(c.ret)
# # @inline dot2(v::CoordF) = return v.ret^2 + v.tex^2 + v.vol^2
# @inline subdot2(c::CoordF, x::NTuple{3,Float64}) = (c.ret - x[1])^2 + (c.tex - x[2])^2 + (c.vol - x[3])^2

# function kernel_k_v(obs, x, k)::Float64
#     # e^(-(obs - x)^2/k)
#     # TODO: multikde uses prod() instead of sum?

#     # return sum(map(obs) do o
#     #     exp(-inside(o, x, k))
#     # end; init=0.0)

#     # return sum(exp.(-dot2.(tsub.(obs, x)) / k))

#     s = 0.0
#     for ob in obs
#         s += exp(-inside(ob, x, k))
#     end
#     return s
# end

function kernel(obs, x, k)::Float64
    s = 0.0
    for ob in obs
        s += inside1(ob, x, k)
    end
    return s
end

# @inline Base.broadcasted(::typeof(-), c::CoordF, x::Vector{Float64}) = CoordF(c.ret)
# @inline Base.broadcasted(::typeof(-), c::CoordF, x::NTuple{3,Float64}) = c.ret - x[1] ...
# @inline dot2(v::CoordF) = return v.ret^2 + v.tex^2 + v.vol^2
# @inline inside1(c::CoordF, x::NTuple{3,Float64})::Float64 = 1 / (1 + abs(c.ret - x[1])) + 1 / (1 + abs(c.tex - x[2])) + 1 / (1 + abs(c.vol - x[3]))
# @inline inside1(c::CoordF, x::NTuple{3,Float64})::Float64 = 1 / (1 + (c.ret - x[1])^2) + 1 / (1 + (c.tex - x[2])^2) + 1 / (1 + (c.vol - x[3])^2)

# @inline inside1(c::CoordF, x::NTuple{3,Float64}, k)::Float64 = 1 / (1 + (c.ret - x[1])^2/k[1]) + 1 / (1 + (c.tex - x[2])^2/k[2]) + 1 / (1 + (c.vol - x[3])^2/k[3])
# @inline inside1(c::CoordF, x::NTuple{3,Float64}, k)::Float64 = 1 / (1 + (c.ret - x[1])^2/k[1]) * 1 / (1 + (c.tex - x[2])^2/k[2]) * 1 / (1 + (c.vol - x[3])^2/k[3])



@inline inside1(c::CoordF, x::NTuple{3,Float64}, k)::Float64 = exp(-(c.ret - x[1])^2/k[1]) + exp(-(c.tex - x[2])^2/k[2]) + exp(-(c.vol - x[3])^2/k[3])
# @inline inside1(c::CoordF, x::NTuple{3,Float64}, k)::Float64 = exp(-(c.ret - x[1])^2/k[1]) * exp(-(c.tex - x[2])^2/k[2]) * exp(-(c.vol - x[3])^2/k[3])


# @inline inside1(c::CoordF, x::NTuple{3,Float64})::Float64 = exp(-(c.ret - x[1])^2) + exp(-(c.tex - x[2])^2) + exp(-(c.vol - x[3])^2)
# @inline inside1(c::CoordF, x::NTuple{3,Float64})::Float64 = exp(-(c.ret - x[1])^2) * exp(-(c.tex - x[2])^2) * exp(-(c.vol - x[3])^2)


# @inline inside(c::CoordF, x::NTuple{3,Float64}, k::NTuple{3,Float64})::Float64 = (c.ret - x[1])^2 / k[1] + (c.tex - x[2])^2 / k[2] + (c.vol - x[3])^2 / k[3]

#endregion Local

end