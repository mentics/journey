module Kelly
using Roots
using BaseTypes, ProbTypes
import ProbUtil

#=
integral(s)[ ( pdf(s) * out(s) ds ) / ( 1 + out(s) x ) ]
integral(s(0:k))[ ( (pdf_m * s + pdf_left) * (out_m * s + out_left) ds ) / ( 1 + (out_m * s + out_left) x ) ]
integral of (q * x + p) * (m * x + o) / (1 + (m * x + o) * b) <- x is s, not the x above
Can be integrated: https://bit.ly/3mud4SA
(x (2 b m p + b m q x - 2 q))/(2 b^2 m) - ((b m p - b o q - q) log(b m x + b o + 1))/(b^3 m^2) + constant
=#

# https://math.stackexchange.com/a/662210/235608
const PROB_INTEGRAL_WIDTH2 = 1.0
# Commit, not risk, because in the formula, it's balance/commit to get number of contracts
function calckel(prob::Prob, commit::Real, segsWithZeros; dpx=PROB_INTEGRAL_WIDTH2, probadjust=1.0)
    cdfLeft = 0.0
    pbs = NTuple{3,Float64}[]
    sizehint!(pbs, 200)
    # pbs = Tuple{Float64,Float64,Any}[]
    ptotal = 0.0
    ev = 0.0
    # pts = []
    for seg in segsWithZeros
        # push!(pts, (seg.left.x, seg.right.x))
        x_right = seg.right.x
        cdfRight = ProbUtil.cdf(prob, x_right)
        if seg.slope == 0.0
            # @assert seg.left.y == seg.right.y
            outcome = seg.left.y / commit
            p = cdfRight - cdfLeft
            p *= outcome > 0 ? 1.0 - probadjust : 1.0 + probadjust
            po = p * outcome
            ev += po
            ptotal += p
            # push!(pbs, (p * outcome, outcome, (;seg, left=seg.left.x, right=x_right, cdfLeft, cdfRight, p)))
            push!(pbs, (po, outcome, p))
        else
            # chop it into more pieces
            # integral[ outcome ] = (outcome.left + outcome.right) / 2 # trapezoid area, I think width can be 1 because discrete and a type of "width" is in prob term
            # integral[ prob * outcome ] =
            span = x_right - seg.left.x
            num = ceil(span / dpx)
            width = span / num
            outcomeStep = (span / num) * seg.slope

            left = seg.left.x
            outcomeLeft = seg.left.y
            for i in 0:num-1
                right = left + width
                outcome = (outcomeLeft + outcomeStep / 2) / commit
                cdfR2 = ProbUtil.cdf(prob, right)
                p = cdfR2 - cdfLeft
                p *= outcome > 0 ? 1.0 - probadjust : 1.0 + probadjust
                po = p * outcome
                ev += po
                ptotal += p
                # push!(pbs, (p * outcome, outcome, (;seg, left, right, cdfLeft, cdfRight=cdfR2, p)))
                push!(pbs, (po, outcome, p))
                left = right;
                outcomeLeft += outcomeStep
                cdfLeft = cdfR2
            end
        end
        cdfLeft = cdfRight;
    end

    for i in eachindex(pbs)
        pb = pbs[i]
        pbs[i] = (pb[1] / ptotal, pb[2], pb[3] / ptotal)
    end
    ev /= ptotal

    # global kpbs = pbs
    # sump = sum(x -> x[3], pbs)
    # # @assert ptotal == sump
    # if sump > 1.000001 || sump < 0.999
    #     println("WARN: sumpbs not 1: $sump")
    # end

    kel = Kelly.findZero() do x
        s = sum(pbs) do (pb, b, _)
            pb / (1 + b*x)
        end
        return s
    end
    # ev = evlogret(kel, pbs)
    evret = calcevret(kel, pbs)
    return evret > 0.0 ? (;kel, evret, ev) : (;kel=NaN, evret, ev)
end

#=
Expected value of sum of log of the resulting balance
expected value = sum ( prob * outcome ) where sum(prob) == 1
x is the percentage of balance to invest
sum(prob_i *
    log(
        M + # Amount you did not risk
        (x*M / commit) * outcome_i # outcome_i * numcontracts ; numcontracts = x*M / commit ; (x*M / commit) * outcome_i
    )
)
M can be 1 = 100% of balance
sum(pi * log(1 + x * oi / commit))
to maximize that, derivative with respect to x
d/dx = sum( p_i * (o_i / commit) / (1 + x * o_i / commit) )
=#

probouts() = [(;prob=pb[3].p, outcome=pb[2]) for pb in Kelly.kpbs]
devlogret(x) = devlogret(x, probouts())
function devlogret(x, probouts)
    sum(po -> po.prob * po.outcome / (1 + x * po.outcome), probouts)
end

evlogretpo(x) = evlogretpo(x, probouts())
function evlogretpo(x, probouts)
    sum(po -> po.prob * log(max(0.00001, 1.0 + x * po.outcome)), probouts)
end
evlogret(x, pbs) = sum(pb -> pb[3].p * log(max(0.00001, 1.0 + x * pb[2])), pbs)
# evret adjusted for duration is what we want to maximize
# pb[2] (outcome) is already divided by commit so it's a percentage return
# kel is multiplied in there already
calcevret(kel, pbs) = sum(pb -> pb[3] * (kel * pb[2]), pbs)


# calc(pvals, vals) = ded(pvals, vals)
calc!(pvals, vals) = ded(buf, pvals, vals)

# const BINS_ONES = Bins.with(1.0)
# logain(prob, vals, ratio) = prob .* log1p.(ratio .* vals)
# eee(prob, vals, ratio) = sum(prob .* log1p.(ratio .* vals))
eee(pvals, vals, ratio) = sum(pvals[i] * log1p(ratio * vals[i]) for i in eachindex(pvals))

# deePrev(pvals, vals, ratio) = sum(pvals .* vals ./ (1.0 .+ (ratio .* vals)))
# dee(pvals, vals, ratio) = sum(pvals[i] * vals[i] / (1.0 + (ratio * vals[i])) for i in eachindex(pvals))
# dee2(pvals, vals, ratio) = sum(pvals[i] * vals[i] / (1.0 + (ratio * vals[i])) for i in 1:length(pvals))
# dee3(pvals, vals, ratio) = sum(pvals[i] * vals[i] / (1.0 + (ratio * vals[i])) for i in 1:603)
# deeDot(pvals, vals, ratio) = sum(@. (pvals * vals / (1.0 + (ratio * vals))))
# function deeFor(pvals, vals, ratio)
#     s = 0.0
#     for i in eachindex(pvals)
#         s += pvals[i] * vals[i] / (1.0 + (ratio * vals[i]))
#     end
#     return s
# end
# function deeBuf(buf, pvals, vals, ratio)
#     broadcast!(*, buf, ratio, vals)
#     buf .+= 1
#     broadcast!(bbb, buf, pvals, vals, buf)
#     return sum(buf)
# end
bbb(pval, val, denom) = pval * val / denom

dee1(pv, vals, ratio) = sum(pv ./ (1.0 .+ (ratio .* vals)))
dee2(pv, vals, ratio) = sum(pv[i] / (1.0 + (ratio .* vals[i])) for i in eachindex(pv))
# dee4(pv, vals, ratio) = sum(pv ./ (1.0 .+ (ratio .* vals)))
# dee5(pv, vals, ratio) = sum(pv ./ (1.0 .+ (ratio .* vals)))
function ded!(buf1, buf2, prob, origvals, risk)
    broadcast!(/, buf2, origvals, risk)
    vals = buf2
    broadcast!(*, buf1, prob, vals)
    pv = buf1
    # pv = prob .* vals
    xleft = .001
    xright = .999
    xmid = 0.5
    left = dee2(pv, vals, xleft)
    right = dee2(pv, vals, xright)
    left > 0.0 || return -Inf
    right < 0.0 || return 1.0
    mid = dee2(pv, vals, xmid)
    if mid < 0.0
        right = mid
        xright = xmid
    else
        left = mid
        xleft = xmid
    end

    for _ in 1:50
        w = xright - xleft
        rat = left / (left - right)
        xmid = rat > .1 ? xleft + rat * w : xleft + .25 * w
        mid = dee2(pv, vals, xmid)
        abs(mid) > 0.001 || return xmid
        if mid < 0.0
            right = mid
            xright = xmid
        else
            left = mid
            xleft = xmid
        end
    end
    # TODO: resolve this
    # @error "Could not solve for kelly" xleft xmid xright left mid right
    return -Inf
    # TODO: maybe it's concave up, check first?
    # error("could not solve")
end
# function ded(prob, vals, ratio)
#     dee(prob, vals, .001)
# end
# sum += prob * ret / (1.0 .+ ratio * ret)

# export findZero, kellySimple, kellySimpleF, kellyTerm, dKellyTerm
# export kelly, kellyReturn, kellyOptimum

# kelly = winprob + (winprob - 1)/gain%
# kelly = winprob + (winprob - 1)/(win/loss)
simple(probWin::Float64, win::Float64, loss::Float64) = probWin + (probWin - 1.0) / (win / loss)

function optimize(probs::AVec{Float64}, rets::AVec{Float64}, min=0.001, max=1.0)::Float64
    findZero(expectedReturnDerivF(probs, rets), min, max)
end

kellyTerm(probWin::Float64, win::Float64, loss::Float64, ratio::Float64) = probWin * log(1.0 + ratio * (win / loss))

# Assumes data is already normalized to risk = -1.0
kellyTerm(prob, ret, ratio) = prob * log(1.0 + ratio * ret)
dKellyTerm(prob::Float64, ret::Float64, ratio::Float64) = prob * ret / (1.0 + ratio * ret)
# ddKellyTerm(prob::Float64, ret::Float64, ratio::Float64) = -prob * ret * ret / (1.0 + ratio * ret)
# so ddKellyTerm = -ret * dKellyTerm

# Assumes data is already normalized to risk = -1.0
function expectedReturnF(probs::AbstractVector{Float64}, rets::AbstractVector{Float64})
    function(ratio)
        sum = 0.0
        @inbounds for i = 1:length(probs)#eachindex(probs)
            prob = probs[i]
            ret = rets[i]
            sum += prob * log(1.0 + ratio * ret)
        end
        return sum
    end
end

# Assumes data is already normalized to risk = -1.0
function expectedReturnDerivF(probs::AbstractVector{Float64}, rets::AbstractVector{Float64})
    function(ratio)
        sum = 0.0
        @inbounds for i = 1:length(probs)#eachindex(probs)
            @inbounds prob = probs[i]
            @inbounds ret = rets[i]
            sum += prob * ret / (1.0 + ratio * ret)
        end
        return sum
    end
end
# p * r * (1 - rat * r) / (1 - rat^2 * r^2)

function kellySimpleF2(probs::AbstractVector{Float64}, rets::AbstractVector{Float64}, termF=dKellyTerm)
    function(ratio)
        sum = 0.0
        for i in eachindex(probs)
            sum += termF(probs[i], rets[i], ratio)
        end
        return sum
    end
end

# function kellySimple2(probs::AbstractVector{Float64}, rets::AbstractVector{Float64}, min=0.001, max=1.0)::Float64
#     f = ratio -> begin
#         sum = 0.0
#         for i in 1:length(probs)#eachindex(probs)
#             sum += kellyTermDeriv(probs[i], rets[i], ratio)
#         end
#         return sum
#     end
#     if f(min) * f(max) < 0.0
#         # return solve(ZeroProblem(f, (0.001, 0.999)))
#         return find_zero(f, (0.001, 0.999))
#         # return f(rand())
#     else
#         return NaN
#     end
# end


# Assumes data is already normalized to risk = -1.0
function kellySimple(probs::AbstractVector{Float64}, rets::AbstractVector{Float64}, min=0.001, max=1.0)::Float64
    findZero(kellySimpleF(probs, rets), min, max)
end

function findZero(f, mn=0.001, mx=.999)
    # f1 = f(mn)
    # f2 = f(mx)
    # if f(mn) * f(mx) < 0.0
        # TODO: this code might be doing 1 allocation. but have to check at higher level
        # try
        try
            return solve(ZeroProblem(f, (mn, mx)))
        catch e
            # ignore
            return NaN
        end
        # catch e
        #     @error "findZero2" mn mx f(mn) f(mx) f1 f2 (f(mn) * f(mx)) (f(mn) * f(mx) < 0.0)
        #     rethrow(e)
        # end
        # TODO: find_zero is doing 12 allocations. Should be optimizable.
        # return find_zero(f, (min, max))
    # else
    #     return NaN
    # end
end

# kellyNormalize(vprob::AbstractVector{Float64}, vret::AbstractVector{Float64}, min=0.0001, max=.9999)::Float64 = kellySimple(vprob, vret ./ -minimum(vret), min, max)

# function kellyTermR(prob::Float64, ret::Float64, risk::Float64, ratio::Float64)
#     k = ret / risk
#     if (1.0 + ratio * k) < 0
#         @warn "neg:" prob ret risk ratio
#         return 0
#     end
#     return prob * log(1.0 + ratio * k)
# end

# function kellyTermDerivR(prob::Float64, ret::Float64, risk::Float64, ratio::Float64)
#     k = ret / risk
#     return prob * k / (1.0 + ratio * k)
# end


# calcKelly(pv, rets) = calcKelly!(pv, copy(rets))
# function calcKelly!(pv, rets)
#     # mn, mx = extrema(rets)
#     # risk = abs(mn)
#     mn = minimum(rets)
#     risk = abs(mn)
#     if mn > 0.0
#         return 17 + 1000 * mn
#     end
#     rets ./= risk
#     # @assert minimum(rets) ≈ -1.0
#     if abs(minimum(rets) + 1.0) > 0.0001
#         error("What the hog doin'? ", minimum(rets))
#     end
#     kel = Kelly.optimize(pv, rets)
#     isnan(kel) && return NaN
#     return kel
# end


# kelly = winprob + (winprob - 1)/gain%
# 2:1 odds at 50% = .5 + (.5 - 1)/2 = .25
# 1:1 odds at 70% = .7 + (.7 - 1)/1 = .4
# 0.5:1 odds at 70% = .7 + (.7 - 1)/0.5 = .1
# 0.5:1 odds at 80% = .8 + (.8 - 1)/0.5 = .4
# 0.78:3.22 odds at 80.8% -> .242:1 = .808 + (.808 - 1)/0.242 = .014611
# 0.78:3.22 odds at 81.8% -> .242:1 = .818 + (.818 - 1)/0.242 = .06593388

end