module Kelly
using Roots
using BaseTypes

calc(pvals, vals) = ded(pvals, vals)

# const BINS_ONES = Bins.with(1.0)
# logain(prob, vals, ratio) = prob .* log1p.(ratio .* vals)
eee(prob, vals, ratio) = sum(prob .* log1p.(ratio .* vals))
dee(prob, vals, ratio) = sum(prob .* vals ./ (1.0 .+ (ratio .* vals)))
dee2(pv, vals, ratio) = sum(pv ./ (1.0 .+ (ratio .* vals)))
function ded(prob, vals)
    pv = prob .* vals
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
    @error "Could not solve for kelly" xleft xmid xright left mid right
    return -Inf
    # TODO: maybe it's concave up, check first?
    # error("could not solve")
end
function ded(prob, vals, ratio)
    dee(prob, vals, .001)
end
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
    if f(mn) * f(mx) < 0.0
        # TODO: this code might be doing 1 allocation. but have to check at higher level
        # try
            return solve(ZeroProblem(f, (mn, mx)))
        # catch e
        #     @error "findZero2" mn mx f(mn) f(mx) f1 f2 (f(mn) * f(mx)) (f(mn) * f(mx) < 0.0)
        #     rethrow(e)
        # end
        # TODO: find_zero is doing 12 allocations. Should be optimizable.
        # return find_zero(f, (min, max))
    else
        return NaN
    end
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

# kelly = winprob + (winprob - 1)/gain%
# 2:1 odds at 50% = .5 + (.5 - 1)/2 = .25
# 1:1 odds at 70% = .7 + (.7 - 1)/1 = .4
# 0.5:1 odds at 70% = .7 + (.7 - 1)/0.5 = .1
# 0.5:1 odds at 80% = .8 + (.8 - 1)/0.5 = .4
# 0.78:3.22 odds at 80.8% -> .242:1 = .808 + (.808 - 1)/0.242 = .014611
# 0.78:3.22 odds at 81.8% -> .242:1 = .818 + (.818 - 1)/0.242 = .06593388

end